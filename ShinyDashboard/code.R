
suppressMessages(library(tidyverse))
suppressMessages(library(ggplot2))
suppressMessages(library(SDMTools))
suppressMessages(library(caret))
suppressMessages(library(effects))

df <- read.csv("brazilian_forest_data.csv", stringsAsFactors = T)
set.seed(100) 
s <- sample(nrow(df), round(.7*nrow(df)))
train <- df[s,]
test <- df[-s,]

X_train <- train[1:9]
X_test <- test[1:9]

y_train <- train[10:length(train)]
y_test <- test[10:length(test)]


##################################################Correlation matrix

# corMat <- cor(df, method="spearman")
# corMat
# 
# hst <- sort(corMat)[(nrow(corMat)-1)*nrow(corMat)]
# fst <- sort(corMat)[1]
# hstCorr <- ifelse(abs(fst) > hst, fst, hst)
# 
# param <- which(corMat == hstCorr, arr.ind = TRUE)[1,]
# par1 <- rownames(corMat)[param[1]]
# par2 <- rownames(corMat)[param[2]]
# paste(par1," and ",par2," have the highest correlation with value of ",hstCorr)


##################################################### Logistic Regression on each Biom
biome_accuracy <- data.frame(colnames(y_test))
biome_accuracy$accuracy_logit <- 0

# sink("logit.txt")
i = 0
logitList <- vector(mode="list", length=20)
for (column in colnames(y_train)) {
  i = i + 1
  print(column)
  fm <- as.formula(paste(column, "~ (lat +	long +	alt +	temp2m + humidity + 
                         precip +	atm +	wind + m.fapar)^2"))
  fit <- glm(fm,family = "binomial",data = df)
  
  logit <- step(fit, direction = "backward",trace = 0)
  # print(summary(logit))
  
  logitList[[i]] <- logit
  # plot(allEffects(logitList[[i]]))
  #testing the logit model on the test data
  # pred <- predict(logit,X_test)
  # print(table(Actual = y_test[,which(colnames(y_test)==column)], predict = pred > 0.5))
  # print(accuracy(y_test[,which(colnames(y_test)==column)], pred))
  # biome_accuracy[i, 2] = accuracy(y_test[,which(colnames(y_test)==column)], pred)[,6]
}

i = 0
for (column in colnames(y_train)) {
  i = i + 1
  print(column)
  plot(allEffects(logitList[[2]])[10])
}


# model
# lat *long * (p1+p2...) + (p1+p2...)^2
# (p1+p2....)^3
# sum(choose(9, 0:3))
# sink()




############################################################ Neural net

library(neuralnet)

normalize = function(x){
  return((x-min(x))/(max(x)-min(x)))
}

df_norm=as.data.frame(lapply(df, normalize))
df_train = df_norm[1:606,]
df_test = df_norm[607:865,]

set.seed(1000000)

df_model = neuralnet(df_train$Campo_Rupestre + df_train$Cerrado_lato_sensu +
                       + df_train$Floresta_de_Terra_Firme + 
                       df_train$Floresta_Estacional_Semidecidual + 
                       df_train$Floresta_Ombrofila_Densa + df_train$Restinga + 
                       df_train$Savana_Estepica + df_train$Floresta_de_Igapo + 
                       df_train$Floresta_de_Varzea + df_train$Campo_de_Altitude + 
                       df_train$Campo_de_Varzea + 
                       df_train$Vegetacao_Sobre_Afloramentos_Rochosos + 
                       df_train$Savana_Amazonica + 
                       df_train$Campinarana + 
                       df_train$Campo_Limpo + 
                       df_train$Floresta_Estacional_Decidual + 
                       df_train$Floresta_Estacional_Perenifolia + 
                       df_train$Floresta_Ombrofila_Mista + df_train$Palmeiral + 
                       df_train$Manguezal ~ ., data = df_train, 
                     linear.output=FALSE, hidden = 4, stepmax=1e+6
                     , likelihood = TRUE, algorithm = "rprop+", err.fct = "sse")#)#
 #hidden = c(5,3) 

plot(df_model, rep="best")

# gwplot(df_model, selected.covariate="alt")
# gwplot(df_model, selected.covariate="m.fapar")
# gwplot(df_model, selected.covariate="precip")
# gwplot(df_model, selected.covariate="temp2m")
# gwplot(df_model, selected.covariate="atm")
# gwplot(df_model, selected.covariate="wind")

model_results = neuralnet::compute(df_model,df_test)
predicted_strength = model_results$net.result

# cor(predicted_strength,df_test$strength)

################################ which bioms got predicted
# predicted_df <- df[607:865,][1:9]
# predicted_df$predicted_biom <- colnames(df_test[10:29])[apply(predicted_strength, 1, which.max)]

# view((predicted_strength > 0.9))
predStr <- predicted_strength
predStr[predStr > 0.5] = 1
predStr[predStr < 0.5] = 0
# view(predStr)

biome_accuracy$accuracy_nnet <- 0

for(i in 1:20) {
    results <- data.frame(actual = df_test[, 9+i], prediction = predStr[,i])
    roundedresults<-sapply(results,round,digits=0)
    roundedresultsdf=data.frame(roundedresults)
    biome_accuracy[i, 3] = accuracy(roundedresultsdf$actual, roundedresultsdf$prediction)[,6]
}


############################################################ Multivariate Random Forest
library(MultivariateRandomForest)

n_tree = 2
m_feature = 5
min_leaf = 40
Prediction <- build_forest_predict(as.matrix(X_train), as.matrix(y_train), n_tree, 
                                   m_feature, min_leaf, as.matrix(X_test))
# ?build_forest_predict
# plot(Prediction)

mrfPred_y <- Prediction
mrfPred_y[mrfPred_y > 0.5] = 1
mrfPred_y[mrfPred_y < 0.5] = 0

biome_accuracy$accuracy_MRF <- 0

for(i in 1:20) {
  results <- data.frame(actual = y_test[,i], prediction = mrfPred_y[,i])
  roundedresults<-sapply(results,round,digits=0)
  roundedresultsdf=data.frame(roundedresults)
  biome_accuracy[i, 4] = accuracy(roundedresultsdf$actual, roundedresultsdf$prediction)[,6]
}

summary(Prediction)
# average accuracy for logit and nnet
biome_accuracy$avg_accuracy_logit <- mean(biome_accuracy$accuracy_logit)
biome_accuracy$avg_accuracy_nnet <- mean(biome_accuracy$accuracy_nnet)
biome_accuracy$avg_accuracy_MRF <- mean(biome_accuracy$accuracy_MRF)





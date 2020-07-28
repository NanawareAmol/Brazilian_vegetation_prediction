
suppressMessages(library(tidyverse))
suppressMessages(library(ggplot2))
# suppressMessages(library(GGally))
# suppressMessages(library(dendextend))
# suppressMessages(library(car))
suppressMessages(library(SDMTools))
suppressMessages(library(caret))

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

corMat <- cor(df, method="spearman")
corMat

hst <- sort(corMat)[(nrow(corMat)-1)*nrow(corMat)]
fst <- sort(corMat)[1]
hstCorr <- ifelse(abs(fst) > hst, fst, hst)

param <- which(corMat == hstCorr, arr.ind = TRUE)[1,]
par1 <- rownames(corMat)[param[1]]
par2 <- rownames(corMat)[param[2]]
paste(par1," and ",par2," have the highest correlation with value of ",hstCorr)


##################################################### Logistic Regression on each Biom

sink("logit.txt")

for (column in colnames(y_train)) {
  print(column)
  fm <- as.formula(paste(column, "~ (lat +	long +	alt +	temp2m + humidity + 
                         precip +	atm +	wind + m.fapar)^3"))
  fit <- glm(fm,family = "binomial",data = df)
  
  # model
  # lat *long * (p1+p2...) + (p1+p2...)^2
  # (p1+p2....)^3
  # sum(choose(9, 0:3))
  
  logit <- step(fit, direction = "backward",trace = 0)
  print(summary(logit))
  
  #testing the logit model on the test data
  pred <- predict(logit,X_test)
  print(table(Actual = y_test[,which(colnames(y_test)==column)], predict = pred > 0.5))
  print(accuracy(y_test[,which(colnames(y_test)==column)], pred))
}

sink()

# #Training data prediction confusion matrix
# ptrain_all <- predict(logit,X_train)
# table(Actual=y_train$Campo_Rupestre, predict=ptrain_all>0.5)
# suppressMessages(library(caret))
# accuracy(y_train$Campo_Rupestre, ptrain_all)
# 
# 
# #Testing data prediction confusion matrix
# ptest_all <- predict(logit,X_test)
# table(Actual=y_test$Campo_Rupestre, predict=ptest_all>0.5)
# accuracy(y_test$Campo_Rupestre, ptest_all)



############################################################ Neural net

# install.packages("neuralnet")
library(neuralnet)

# #Get the data from http://archive.ics.uci.edu/ml 30
# concrete=read.csv(“Concrete_Data.csv”)
str(df)
normalize = function(x){
  return((x-min(x))/(max(x)-min(x)))
}

df_norm=as.data.frame(lapply(df, normalize))

df_train = df_norm[1:606,]
df_test = df_norm[607:865,]

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
                       df_train$Manguezal ~ ., data = df_train, linear.output=FALSE) #hidden = c(5,3) 

plot(df_model)
model_results = compute(df_model,df_test)

str(df_test)

predicted_strength = model_results$net.result

cor(predicted_strength,df_test$strength)

predicted_strength[1:5,]

################################ which bioms got predicted
predicted_df <- df[607:865,][1:9]
predicted_df$predicted_biom <- colnames(df_test[10:29])[apply(predicted_strength, 1, which.max)]

view(predicted_strength > 0.5)


############################################################ Multivariate Random Forest
library(MultivariateRandomForest)

n_tree=2
m_feature=20
min_leaf=20
Prediction <- build_forest_predict(as.matrix(X_train), as.matrix(y_train), n_tree, 
                                   m_feature, min_leaf, as.matrix(X_test))

xtst <- as.matrix(X_test)
as.matrix(X_test[sample(X_test, 9), ])

dim(as.matrix(X_train))
dim(as.matrix(y_train))
dim(transpose(X_test))
typeof(as.matrix(transpose(X_test)))
typeof(t(as.matrix(X_test)))
xtst <- t(X_test)
as.matrix(xtst[sample(xtst, 432), ])
dim(xtst)


############################################################################### maximum entropy
# install.packages("maxent")
# model <- maximumentropy(Species ~ ., data=iris, iteration=100)
# predict(model, new_dataset)

library(rminer)




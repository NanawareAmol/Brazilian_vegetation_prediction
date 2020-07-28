library(shiny)
library(shinydashboard)

suppressMessages(library(tidyverse))
suppressMessages(library(ggplot2))
suppressMessages(library(SDMTools))
suppressMessages(library(caret))
suppressMessages(library(neuralnet))
suppressMessages(library(shinycssloaders))
suppressMessages(library(MultivariateRandomForest))

server <- function(input, output) {
 
  # vals<-reactiveValues()
  # 
  # vals <- eventReactive(input$)
  
  df <- read.csv("brazilian_forest_data.csv", stringsAsFactors = T)
  set.seed(100) 
  s <- sample(nrow(df), round(.7*nrow(df)))
  train <- df[s,]
  test <- df[-s,]
  
  X_train <- train[1:9]
  X_test <- test[1:9]
  
  y_train <- train[10:length(train)]
  y_test <- test[10:length(test)]
  
  biome_accuracy <- data.frame(colnames(y_test))
  if(!file.exists("logitList.rda")) {
    biome_accuracy$accuracy_logit <- 0
  }
  if(!file.exists("model_results.rda")) {
    biome_accuracy$accuracy_nnet <- 0
  }
  if(!file.exists("mrfPred.rda")) {
    biome_accuracy$accuracy_MRF <- 0
  }
  
  
  
  
  # plot()
  
  
  
  
  Cases <- reactive({
    if (input$selectedvariable=="Logistic") {
      if(file.exists("logitList.rda")) {
        ## load model
        load("logitList.rda")
        i = 0
        for (column in colnames(y_train)) {
          i = i + 1
          print(column)
          print(summary(logitList[[i]]))
        }
      } else {
        ## (re)fit the model
        biome_accuracy$accuracy_logit <- 0
        i = 0
        logitList <- vector(mode="list", length=20)
        for (column in colnames(y_train)) {
          i = i + 1
          print(column)
          fm <- as.formula(paste(column, "~ (lat +	long +	alt +	temp2m + humidity + 
                         precip +	atm +	wind + m.fapar)^2"))
          fit <- glm(fm,family = "binomial",data = df)
          
          logit <- step(fit, direction = "backward",trace = 0)
          print(summary(logit))
          
          logitList[[i]] <- logit
          
          #testing the logit model on the test data
          pred <- predict(logit,X_test)
          print(table(Actual = y_test[,which(colnames(y_test)==column)], predict = pred > 0.5))
          print(accuracy(y_test[,which(colnames(y_test)==column)], pred))
          biome_accuracy[i, 2] = accuracy(y_test[,which(colnames(y_test)==column)], pred)[,6]
        }
        save(logitList, file = "logitList.rda")
        biome_accuracy$avg_accuracy_logit <- mean(biome_accuracy$accuracy_logit)
      }
      
    }else if (input$selectedvariable=="Neural Net") {
      
      if(file.exists("nnet_model.rda")) {
        ## load model
        load("nnet_model.rda")
        print(summary(nnet_model))
        
      } else {
        ## (re)fit the model
        normalize = function(x){
          return((x-min(x))/(max(x)-min(x)))
        }
  
        df_norm=as.data.frame(lapply(df, normalize))
        df_train = df_norm[1:606,]
        df_test = df_norm[607:865,]
  
        set.seed(1000000)
  
        nnet_model = neuralnet(df_train$Campo_Rupestre + df_train$Cerrado_lato_sensu +
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
                             linear.output=FALSE, hidden = 4, stepmax=1e+6) #hidden = c(5,3)
  
        model_results = compute(nnet_model,df_test)
        predicted_strength = model_results$net.result
  
        predStr <- predicted_strength
        predStr[predStr > 0.5] = 1
        predStr[predStr < 0.5] = 0
  
        print(summary(nnet_model))
        # save(model_results, file = "model_results.rda")
        save(nnet_model, file = "nnet_model.rda")
        
        biome_accuracy$accuracy_nnet <- 0

        for(i in 1:20) {
          results <- data.frame(actual = df_test[, 9+i], prediction = predStr[,i])
          roundedresults<-sapply(results,round,digits=0)
          roundedresultsdf=data.frame(roundedresults)
          biome_accuracy[i, 3] = accuracy(roundedresultsdf$actual, roundedresultsdf$prediction)[,6]
        }

        biome_accuracy$avg_accuracy_nnet <- mean(biome_accuracy$accuracy_nnet)
        
      }
      
    } else if (input$selectedvariable=="Multivariate Random Forest") {
        
        if(file.exists("mrfPred.rda")) {
          ## load model
          load("mrfPred.rda")
          print(summary(mrfPred))
          
        } else {
          n_tree = 2
          m_feature = 5
          min_leaf = 40
          mrfPred <- build_forest_predict(as.matrix(X_train), as.matrix(y_train), n_tree, 
                                             m_feature, min_leaf, as.matrix(X_test))
          
          mrfPred_y <- mrfPred
          mrfPred_y[mrfPred_y > 0.5] = 1
          mrfPred_y[mrfPred_y < 0.5] = 0
          
          biome_accuracy$accuracy_MRF <- 0
          
          for(i in 1:20) {
            results <- data.frame(actual = y_test[,i], prediction = mrfPred_y[,i])
            roundedresults<-sapply(results,round,digits=0)
            roundedresultsdf=data.frame(roundedresults)
            biome_accuracy[i, 4] = accuracy(roundedresultsdf$actual, roundedresultsdf$prediction)[,6]
          }
          
          biome_accuracy$avg_accuracy_MRF <- mean(biome_accuracy$accuracy_MRF)
          save(mrfPred, file = "mrfPred.rda")
          print(summary(mrfPred))
        }
      }
      
  })
  
  output$detailedPlot <- renderPlot({
    # input$newplot
    if(input$selectedvariable == "Logistic"){
      
    } else if (input$selectedvariable=="Neural Net") {
      load("nnet_model.rda")
      plot(nnet_model, rep="best")
    } else if (input$selectedvariable=="Multivariate Random Forest") {
      
    }
  })
  
  output$Summary<-renderPrint({
    Cases()
  })
}
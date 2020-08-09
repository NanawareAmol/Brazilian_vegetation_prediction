
# Required libraries

suppressMessages(library(shiny))
suppressMessages(library(shinydashboard))
suppressMessages(library(tidyverse))
suppressMessages(library(SDMTools))
suppressMessages(library(neuralnet))
suppressMessages(library(shinycssloaders))
suppressMessages(library(MultivariateRandomForest))
suppressMessages(library(DT))
suppressMessages(library(ggiraph))
suppressMessages(library(effects))


server <- function(input, output, session) {
  
  df <- read.csv("brazilian_forest_data.csv", stringsAsFactors = T)
  
  set.seed(100)
  s <- sample(nrow(df), round(.7*nrow(df)))
  train <- df[s,]
  test <- df[-s,]
  
  X_train <- train[1:9]
  X_test <- test[1:9]
  
  y_train <- train[10:length(train)]
  y_test <- test[10:length(test)]
  
  # Normalise the data for Neural net
  
  normalize <- function(x){
    return((x-min(x))/(max(x)-min(x)))
  }
  
  df_norm <- as.data.frame(lapply(df, normalize))
  df_train <- df_norm[1:606,]
  df_test <- df_norm[607:865,]
  
  # checks for biome_accuracy file and other model files
  
  if(!file.exists("biome_accuracy.rda")) {
    biome_accuracy <- data.frame(colnames(y_test))
  }else{
    load("biome_accuracy.rda")
  }
  if(!file.exists("logitList.rda")) {
    biome_accuracy$accuracy_logit <- 0
  } else {
    load("logitList.rda")
  }
  
  if(!file.exists("model_results.rda")) {
    biome_accuracy$accuracy_nnet <- 0
  }
  if(!file.exists("mrfPred.rda")) {
    biome_accuracy$accuracy_MRF <- 0
  }
  
  # Raw page, data file display
  
  output$dataTable = DT::renderDataTable({
    df
  })
  
  # display biome accuracy data tables
  
  output$accuracyTables = DT::renderDataTable({
    if(file.exists("biome_accuracy.rda")) {
      load("biome_accuracy.rda")
      if(input$selectedvariable == "Logistic"){
        biome_accuracy %>%
          rename(Biome_name = colnames.y_test.) %>% 
          select(Biome_name, accuracy_logit, avg_accuracy_logit)
      } else if (input$selectedvariable=="Multivariate Random Forest") {
          biome_accuracy %>%
            rename(Biome_name = colnames.y_test.) %>% 
            select(Biome_name, accuracy_MRF, avg_accuracy_MRF)
      }
    }
  })
  
  output$allAccuracyTable = DT::renderDataTable({
    if(file.exists("biome_accuracy.rda")) {
      load("biome_accuracy.rda")
      biome_accuracy %>% 
        rename(Biome_name = colnames.y_test.)
    }
  })
  
  # Model building and display summary
  
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
        if(file.exists("biome_accuracy.rda")) {
          load("biome_accuracy.rda")
        }
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
          biome_accuracy[i, "accuracy_logit"] = accuracy(y_test[,which(colnames(y_test)==column)], pred)[,6]
        }
        save(logitList, file = "logitList.rda")
        
        biome_accuracy$avg_accuracy_logit <- mean(biome_accuracy$accuracy_logit)
        save(biome_accuracy, file = "biome_accuracy.rda")
        
        print("from logit")
        print(biome_accuracy)
      }
      
    }else if (input$selectedvariable=="Neural Net") {
      
      if(file.exists("nnet_model.rda")) {
        ## load model
        load("nnet_model.rda")
        print(summary(nnet_model))
        
      } else {
        ## (re)fit the model
        if(file.exists("biome_accuracy.rda")) {
          load("biome_accuracy.rda")
        }
          
        set.seed(1000000)
  
        nnet_model <- neuralnet(Campo_Rupestre + Cerrado_lato_sensu +
                                  + Floresta_de_Terra_Firme +
                                  Floresta_Estacional_Semidecidual +
                                  Floresta_Ombrofila_Densa + Restinga +
                                  Savana_Estepica + Floresta_de_Igapo +
                                  Floresta_de_Varzea + Campo_de_Altitude +
                                  Campo_de_Varzea +
                                  Vegetacao_Sobre_Afloramentos_Rochosos +
                                  Savana_Amazonica +
                                  Campinarana +
                                  Campo_Limpo +
                                  Floresta_Estacional_Decidual +
                                  Floresta_Estacional_Perenifolia +
                                  Floresta_Ombrofila_Mista + Palmeiral +
                                  Manguezal ~ ., data = df_train,
                                linear.output=FALSE, hidden = 4, stepmax=1e+6)
        
        model_results <- compute(nnet_model,df_test)
        predicted_strength <- model_results$net.result
  
        predStr <- predicted_strength
        predStr[predStr > 0.5] = 1
        predStr[predStr < 0.5] = 0
  
        print(summary(nnet_model))
        save(nnet_model, file = "nnet_model.rda")
        
        biome_accuracy$accuracy_nnet <- 0

        for(i in 1:20) {
          results <- data.frame(actual = df_test[, 9+i], prediction = predStr[,i])
          roundedresults<-sapply(results,round,digits=0)
          roundedresultsdf <- data.frame(roundedresults)
          biome_accuracy[i, "accuracy_nnet"] <- accuracy(roundedresultsdf$actual, roundedresultsdf$prediction)[,6]
        }

        biome_accuracy$avg_accuracy_nnet <- mean(biome_accuracy$accuracy_nnet)
        save(biome_accuracy, file = "biome_accuracy.rda")
      }
      
    } else if (input$selectedvariable=="Multivariate Random Forest") {
        
        if(file.exists("mrfPred.rda")) {
          ## load model
          load("mrfPred.rda")
          print(summary(mrfPred))
          
        } else {
          if(file.exists("biome_accuracy.rda")) {
            load("biome_accuracy.rda")
          }
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
            biome_accuracy[i, "accuracy_MRF"] = accuracy(roundedresultsdf$actual, roundedresultsdf$prediction)[,6]
          }
          
          biome_accuracy$avg_accuracy_MRF <- mean(biome_accuracy$accuracy_MRF)
          save(mrfPred, file = "mrfPred.rda")
          save(biome_accuracy, file = "biome_accuracy.rda")
          
          print(summary(mrfPred))
        }
      }
      
  })
  
  # plot rendering code and display
  
  output$detailedPlot <- renderPlot({
    if (input$selectedvariable=="Neural Net") {
      load("nnet_model.rda")
      plot(nnet_model, rep="best")
    }
  })
  
  observe({
    lenGraphs <- length(allEffects(logitList[[match(input$logitEffects,colnames(y_train))]]))
    logitEffectsPlotList <<- vector(mode = "list", length = lenGraphs)
    for (l in 1:lenGraphs) {
      logitEffectsPlotList[[l]] <<- names(allEffects(logitList[[match(input$logitEffects,colnames(y_train))]])[l])
    }
    updateSelectInput(session = session ,inputId = "logitEffectsOptions", 
                      choices = logitEffectsPlotList
    )
  })
  
  output$effectsPlot <- renderPlot({
    listIndex <- match(input$logitEffectsOptions, logitEffectsPlotList)
    effectsNum <- as.numeric(listIndex)
    
    if (is.na(listIndex))
      delay(50, plot(allEffects(logitList[[match(input$logitEffects,colnames(y_train))]])[listIndex]))
    else
      plot(allEffects(logitList[[match(input$logitEffects,colnames(y_train))]])[listIndex])
  })
  
  
  # doughnut plots for dashboard
  girafePlot <- function(val1, val2){
    if(file.exists("biome_accuracy.rda")) {
      load("biome_accuracy.rda")
    }
    
    donut_data <- data.frame(Accuracy = c("Wrong_Prediction", "Correct_Prediction"), 
                                 value = c( val1, val2)
    ) %>%
      mutate(
        percentage = value / sum(value),
        hover_text = paste0(Accuracy, ": ", value)
      ) %>%
      mutate(percentage_label = paste0(round(100 * percentage, 1), "%"))
    
    donut_plot <- ggplot(donut_data, aes(y = value, fill = Accuracy)) +
      geom_bar_interactive(
        aes(x = 1, tooltip = hover_text),
        width = 0.1,
        stat = "identity",
        show.legend = FALSE
      ) +
      annotate(
        geom = "text",
        x = 0,
        y = 0,
        label = donut_data[["percentage_label"]][donut_data[["Accuracy"]] == "Correct_Prediction"],
        size = 8,
        color = "light green"
      ) +
      scale_fill_manual(values = c(Wrong_Prediction = "gray", Correct_Prediction = "light green")) +
      coord_polar(theta = "y") +
      theme_void()
    girafe(ggobj = donut_plot, height_svg = 2.7,
    options = list(
      opts_sizing(rescale = TRUE, width = .35) )
    )
  }
  
  output$logisticAccuracyPlot <- renderGirafe({
    val1 <- (nrow(y_test) * (1-biome_accuracy$avg_accuracy_logit[1]))
    val2 <- (nrow(y_test) * biome_accuracy$avg_accuracy_logit[1])
    
    girafePlot(val1, val2)
  })
  output$nnetAccuracyPlot <- renderGirafe({
    val1 <- (nrow(df_test) * (1-biome_accuracy$avg_accuracy_nnet[1]))
    val2 <- (nrow(df_test) * biome_accuracy$avg_accuracy_nnet[1])
    
    girafePlot(val1, val2)
  })
  output$mrfAccuracyPlot <- renderGirafe({
    val1 <- (nrow(y_test) * (1-biome_accuracy$avg_accuracy_MRF[1]))
    val2 <- (nrow(y_test) * biome_accuracy$avg_accuracy_MRF[1])
    
    girafePlot(val1, val2)
  })
  
  output$accuracyBarPlot <- renderGirafe({
    if(file.exists("biome_accuracy.rda")) {
      load("biome_accuracy.rda")
    }
    
    allAcc <- biome_accuracy %>% 
      gather(Algorithm, Accuracy, accuracy_logit:accuracy_MRF) %>%
      rename(
        Biome_name = colnames.y_test.
      )
    
    p <- ggplot(allAcc, aes( x = Biome_name, y = Accuracy, fill = Algorithm,
                          data_id = Biome_name ) ) +
      geom_bar_interactive(
        position=position_dodge(),
        stat = "identity") + 
      geom_text(aes(label = round(100*Accuracy, digits = 1)), vjust=1.6, color="white",
                position = position_dodge(0.9), size=2.5) + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      scale_fill_manual_interactive(
        values = c(accuracy_logit = "#5cb85c", accuracy_nnet = "#5bc0de", accuracy_MRF = "#f0ad4e"),
        data_id = c(accuracy_logit = "LR Accuracy", accuracy_nnet = "Neural Net Accuracy", 
                    accuracy_MRF = "MRF Accuracy"),
        tooltip = c(accuracy_logit = "LR Accuracy", accuracy_nnet = "Neural Net Accuracy", 
                    accuracy_MRF = "MRF Accuracy")
      )
    x <- girafe(ggobj = p, width_svg = 10)
  })
  
  # function call for summary from UI request
  output$Summary<-renderPrint({
    Cases()
  })
}
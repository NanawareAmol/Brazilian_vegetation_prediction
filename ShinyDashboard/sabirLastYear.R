## required libraries

library(ISLR)
library(MASS)
library(rpart)
library(rpart.plot)
library(shiny)
library(randomForest)
library(DMwR)


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      
      uiOutput("select.data"),
      br(),
      
      ##upload file
      
      fileInput("file1", "Upload CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv"), placeholder = "Upload a dataset"),
      
      checkboxInput("header", "Header", TRUE),
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      br(),
      numericInput("partition", "Enter the partition value for train data (%)", 50,
                   min = 1, max = 100),
      br(),
      uiOutput("response"),
      br(),
      uiOutput("CP"),
      numericInput("mtry", "Number of predictors for split in Random forest (mtry)",1),
      br(),
      numericInput("ntree", "Number of trees for Random forest (ntree)",25)
      
      
    ),
    
    mainPanel(
      tags$hr(),
      h2("Decision Tree App"),
      tags$hr(),
      h5("Choose sample dataset or upload your dataset and select uploaded data"),
      tabsetPanel(type = "tab",
                  tabPanel("View",plotOutput("View")),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("PlotCp",plotOutput("Plotcp")),
                  tabPanel("Pruned Tree",plotOutput("prune.Tree")),
                  tabPanel("Test RMSE", verbatimTextOutput("print.error")),
                  tabPanel("Influence plot", plotOutput("influence.plot"))
                  
                  
      ),
      tags$details("Created by - Sabir Shaikh | email - mail.sabirshaikh@gmail.com")
    )
  )
)

server <- function(input, output) {
  
  #load user data
  data1 <- reactive({
    inFile <- input$file1
    if (is.null(inFile)){return()}
    uploaded_data <- read.csv(inFile$datapath, header = input$header, sep = input$sep)
    uploaded_data <- as.data.frame(uploaded_data)
  })
  
  
  ## names of column
  Vars <- reactive({
    uploaded_data <- data1()
    switch(input$dataset,
           "Hitters" = names(Hitters),
           "Boston" = names(Boston),
           "uploaded_data" = names(uploaded_data)
    )
  })
  
  ## data selection
  output$select.data <- renderUI({
    uploaded_data <- data1()
    selectInput(inputId = "dataset",
                label = "Choose a dataset:",
                choices = c("Boston"="Boston", "Hitters"="Hitters","uploaded data"="uploaded_data"))
  })
  
  
  ## train data
  train.data <- reactive({
    set.seed(123)
    uploaded_data <- data1()
    my.Data <- knnImputation(get(input$dataset),k=5)
    train <- sample(1:nrow(my.Data), nrow(my.Data) * (input$partition/100))
    
  })
  
  
  
  #fitting the model
  tree.Fit <- reactive({
    uploaded_data <- data1()
    train <- train.data()
    my.Data <- knnImputation(get(input$dataset),k=5)
    my.Data <- my.Data[train,]
    y <- my.Data[,input$response]
    index <- which(names(my.Data) == input$response)
    tree.Model <- rpart(y ~ ., data = my.Data[,-index])
    return(tree.Model)
  })
  
  
  ## prune fit
  
  prune.Tree <- reactive({
    tree.Model <-tree.Fit()
    prune.Model <- prune(tree.Model, cp = input$CpValue)
    return(prune.Model)
  })
  
  ##validation error
  
  validation.error <- reactive({
    uploaded_data <- data1()
    train <- train.data()
    tree.Model <- tree.Fit()
    prune.Model <- prune.Tree()
    my.Data.test <- knnImputation(get(input$dataset),k=5)
    my.Data.test <- my.Data.test[-train,]
    y <- my.Data.test[,input$response]
    index <- which(names(my.Data.test) == input$response)
    tree.predict <- predict(prune.Model, my.Data.test)
    return(sqrt(mean((tree.predict - y)^2)))
  })
  
  ## Random forest
  Rforest <- reactive({
    set.seed(123)
    uploaded_data <- data1()
    train <- train.data()
    my.Data.full <- knnImputation(get(input$dataset),k=5)
    my.Data.train <- my.Data.full[train,]
    y <- my.Data.train[,input$response]
    index <- which(names(my.Data.train) == input$response)
    rf.data <- randomForest(y~., data = my.Data.train[,-index], importance = TRUE, mtry=input$mtry,ntree=input$ntree, na.action = na.omit)
  })
  
  ## Random forest test
  Rf.test <- reactive({
    rf.data <- Rforest()
    uploaded_data <- data1()
    train <- train.data()
    my.Data.full <- knnImputation(get(input$dataset),k=5)
    my.Data.test <- my.Data.full[-train,]
    y <- my.Data.test[,input$response]
    index <- which(names(my.Data.test) == input$response)
    test.rf <- predict(rf.data , newdata = my.Data.test)
    return(sqrt(mean(test.rf - y)^2))
  })
  
  
  ##print validation result
  
  output$print.error <- renderPrint({
    
    test.error <- validation.error()
    test.error.rf <- Rf.test()
    uploaded_data <- data1()
    test.result <- c("sample split" = test.error, "Random Forest" = test.error.rf)
    results <- as.data.frame(round(test.result,4))
    names(results) <- "Result"
    results
  })
  
  
  
  ## Importance 
  output$influence.plot <- renderPlot({
    Rforest.plot <- Rforest()
    uploaded_data <- data1()
    varImpPlot(Rforest.plot)
  })
  
  ## Cp value Range for pruning
  cp.Range <- reactive({
    tree.Model <-tree.Fit()
    cp.Range <- range(printcp(tree.Model)[,1])
    return(round(cp.Range, 3))
  })
  
  ## Dynamic UI
  
  ## Response selection
  output$response <- renderUI({
    selectInput("response", "Select the response variable", choices = Vars())
  })
  
  ## Cp Value selection - slider
  output$CP <- renderUI({
    cprange <- cp.Range()
    sliderInput("CpValue","Select Cp for pruning", 
                min = cprange[1], max = cprange[2], value = 0,
                step = round(abs(diff(cprange))/50, 3) + .001)
  })
  
  
  ##Output
  
  ## Partition summary
  
  output$summary <- renderPrint({
    tree.Model <-tree.Fit()
    tree.Model
  })
  
  ## Decision full tree plot
  
  output$View <- renderPlot({
    tree.Model <-tree.Fit()
    rpart.plot(tree.Model)
    
  })
  
  ## Cp Value plot
  
  output$Plotcp <- renderPlot({
    tree.Model <-tree.Fit()
    plotcp(tree.Model)
  })
  
  
  
  ## Pruned tree plot - based on cp value
  
  output$prune.Tree <- renderPlot({
    prune.Model <- prune.Tree()
    rpart.plot(prune.Model)

  })
  
  # output$ack <- renderText({
  #   "Created by 
  #  Sabir Shaikh
  #  mail.sabirshaikh@gmail.com"
  # })
  
}

shinyApp(ui=ui, server=server)

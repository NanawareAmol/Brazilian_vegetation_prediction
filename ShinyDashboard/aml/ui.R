library(shiny)
library(shinydashboard)
suppressMessages(library(shinycssloaders))
library(DT)
library(ggiraph)

header <- dashboardHeader(
    title = "Brazilian Forest"
)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Detailed Analysis", tabName = "detailedAnalysis", icon = icon("bar-chart")),
        menuItem("Raw Data", tabName = "rawData", icon = icon("table"), 
                 badgeLabel = "New", badgeColor = "green")
    )
)

options(spinner.color="#0275D8", 
        spinner.color.background="#ffffff", 
        spinner.size=1
        )

body <- dashboardBody(
    tabItems(
        tabItem(
            tabName = "dashboard",
            fluidRow(
                box(
                    width = 4,
                    height = "200px",
                    title = "LR Accuracy",
                    girafeOutput("logisticAccuracyPlot")
                ),
                box(
                    width = 4,
                    height = "200px",
                    title = "Neural Net Accuracy",
                    girafeOutput("nnetAccuracyPlot")
                ),
                box(
                    width = 4,
                    height = "200px",
                    title = "MRF Accuracy",
                    girafeOutput("mrfAccuracyPlot")
                )
            ),
            fluidRow(
                box(
                    width = 12,
                    title = "Individual Biome Accuracy Plot",
                    girafeOutput("accuracyBarPlot")
                )
            ),
            fluidRow(
                box(
                    width = 12,
                    title = "Individual Biome Accuracy Table",
                    withSpinner(DT::dataTableOutput("allAccuracyTable"), type = 2),
                    tags$head(tags$style("#allAccuracyTable{
                                                overflow-y:scroll; 
                                                width: auto;
                                             }")
                              )
                )
            )
        ),
        tabItem(
            tabName = "detailedAnalysis",
            fluidRow(
                column(
                    4,
                    selectInput(
                        "selectedvariable",
                        label = "Select Model:",
                        choices = c("Logistic", "Neural Net", "Multivariate Random Forest")
                    )
                ),width=2
            ),
            mainPanel(
                fluidRow(
                    tabBox(
                        width = "500px",
                        tabPanel(id = "summaryPanel", title = "Summary", 
                                 withSpinner(verbatimTextOutput("Summary"), type = 2)
                        ),
                        tabPanel(id = "plotPanel", title = "Graph/Table", status = "primary", 
                                 withSpinner(DT::dataTableOutput("accuracyTables"), type = 2),
                                 solidHeader = T, plotOutput("detailedPlot")
                                 ),
                        tags$head(tags$style("#Summary{overflow-y:scroll;
                                                 max-height: 450px; width: auto;
                                                 background: ghostwhite;}",
                                             "#accuracyTables{
                                                overflow-y:scroll;
                                                max-height: 450px; 
                                                width: auto;
                                                position: relative;
                                                float: left;
                                             }")
                                  )
                    )
                )
            )
        ),
        tabItem(
            tabName = "rawData",
            mainPanel(
                fluidRow(
                    tabBox(
                        width = "500px",
                        tabPanel(id = "rawDataTable", title = "Dataset", 
                                withSpinner(DT::dataTableOutput("dataTable"), type = 2)
                        ),
                        tags$head(tags$style("#dataTable{overflow-y:scroll;
                                                 max-height: 500px; width: auto;
                                                 background: ghostwhite;}"))
                ))
            )
        )
    )
)
    



ui <- dashboardPage(header, sidebar, body)

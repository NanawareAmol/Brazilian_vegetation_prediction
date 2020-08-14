suppressMessages(library(shiny))
suppressMessages(library(shinydashboard))
suppressMessages(library(shinycssloaders))
suppressMessages(library(DT))
suppressMessages(library(ggiraph))

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
        # ######### Dashboard #############
        tabItem(
            tabName = "dashboard",
            # ######### overall accuracy percentage panel #############
            fluidRow(
                box(
                    width = 4,
                    height = "200px",
                    title = "LR Accuracy",
                    withSpinner(girafeOutput("logisticAccuracyPlot"), type = 2)
                ),
                box(
                    width = 4,
                    height = "200px",
                    title = "Neural Net Accuracy",
                    withSpinner(girafeOutput("nnetAccuracyPlot"), type = 2)
                ),
                box(
                    width = 4,
                    height = "200px",
                    title = "MRF Accuracy",
                    withSpinner(girafeOutput("mrfAccuracyPlot"), type = 2)
                )
            ),
            # ######### accuracy bar plot #############
            fluidRow(
                box(
                    width = 12,
                    title = "Individual Biome Accuracy Plot",
                    withSpinner(girafeOutput("accuracyBarPlot"), type = 2)
                )
            ),
            # ######### Overall and individual accuracy table #############
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
        # ######### Detailed Analysis Page #############
        tabItem(
            tabName = "detailedAnalysis",
            # ######### Dropdown menu #############
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
            # ######### Summary and graph panel #############
            mainPanel(
                fluidRow(
                    tabBox(
                        width = "500px",
                        id = "detailedPanelBox",
                        tabPanel(id = "summaryPanel", title = "Summary", 
                                 withSpinner(verbatimTextOutput("Summary"), type = 2)
                        ),
                        tabPanel(id = "plotPanel", title = "Graph/Table", 
                                 withSpinner(DT::dataTableOutput("accuracyTables"), type = 2),
                                 solidHeader = T, plotOutput("detailedPlot")
                                 ),
                        tabPanel(
                            id = "logitEffectsPlots", 
                            title = "Effects Plot",
                            column(width = 6,
                                   selectInput(
                                       "logitEffects",
                                       label = "Select Biome:",
                                       choices = colnames(read.csv("brazilian_forest_data.csv", stringsAsFactors = T)[10:29])
                                   )
                            ),
                            column(width = 6, 
                                   selectInput(
                                       "logitEffectsOptions",
                                       label = "Select Plot:",
                                       choices = 1:10
                                   )
                            ),
                            withSpinner(plotOutput("effectsPlot"), type = 2)
                        ),
                        # ######### css for styling the web elements #############
                        tags$head(tags$style("#Summary{overflow-y:scroll;
                                                 max-height: 460px; width: auto;
                                                 background: ghostwhite;}
                                            #accuracyTables{
                                                overflow-y:scroll;
                                                max-height: 460px;
                                                width: auto;
                                                position: relative;
                                                float: left;
                                             }
                                             div[data-value='Graph/Table']{
                                                overflow: hidden !important;
                                             }
                                             .content-wrapper{
                                                overflow-y: auto;
                                             }
                                             #effectsPlot{
                                                overflow-y: scroll;
                                                width: auto;
                                             }")
                                  )
                    )
                )
            )
        ),
        # ######### Raw data page #############
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
    

# ################################################################
## assigning all the pages content ui code to the UI object using 
## dashboardPage function 
#################################################################


ui <- dashboardPage(header, sidebar, body)

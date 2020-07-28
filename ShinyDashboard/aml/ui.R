library(shiny)
library(shinydashboard)

header <- dashboardHeader(
    title = "Brazilian Forest",
    dropdownMenuOutput("msgOutput"),
    dropdownMenu(
        type = "notifications",
        notificationItem(
            text = "The International Space Station is overhead!",
            href = "",
            icon = icon("dashboard"),
            status = "success"
        )
    ),
    dropdownMenu(
        type = "tasks",
        taskItem(
            text = "The International Space Station is overhead!",
            value = 15, #------ (this is % of task completion)
            color = "red"
        ),
        taskItem(
            text = "task 2",
            value = 55,
            color = "yellow"
        ),
        taskItem(
            text = "task 3",
            value = 80,
            color = "aqua"
        )
    )
    # dropdownMenu(
    #     type = "messages",
    #     messageItem(
    #         from = "Lucy",
    #         message = "You can view the International Space Station!",
    #         href = "https://spotthestation.nasa.gov/sightings/"
    #     )
    # )
)

sidebar <- dashboardSidebar(
    sidebarMenu(    # sidebarMenu is just a container for css styling 
        sidebarSearchForm("searchID", "searchB", "Search"),
        # Create two `menuItem()`s, "Dashboard" and "Inputs"
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Detailed Analysis", tabName = "detailedAnalysis", icon = icon("bar-chart")),
        menuItem("Raw Data", tabName = "rawData", icon = icon("table"), 
                 badgeLabel = "New", badgeColor = "green"),
        sliderInput("bins", "Number of Breaks", 1, 100, 50),    # here bins is a ID
        textInput("input_id", "Search Opportunities", "12345")
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
                        tabPanel(id = "plotPanel", title = "Graph", status = "primary", 
                                     solidHeader = T, plotOutput("detailedPlot")),
                        tabPanel(id = "summaryPanel", title = "Summary", 
                                 withSpinner(verbatimTextOutput("Summary"), type = 2)
                        ),
                        tags$head(tags$style("#Summary{overflow-y:scroll;
                                                 max-height: 450px; width: auto;
                                                 background: ghostwhite;}"))
                    )
                )
            )
        )
    )
)
    



ui <- dashboardPage(header, sidebar, body)

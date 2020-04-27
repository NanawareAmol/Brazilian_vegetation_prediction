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
body <- dashboardBody(
    tabItems(
        tabItem(
            tabName = "dashboard",
            h1("Histogram plot")
        ),
        tabItem(
            tabName = "detailedAnalysis",
            fluidRow(
                box(title = "Histogram of faithful", status = "primary", 
                    solidHeader = T, plotOutput("histogram"))
                #histogram is ID in output variable which we will be using in server code
                
                # box(title = "box 2", status = "warning",
                    # "you can add body text directly like this for this box", br()
                    # "2nd text"
                #     solidHeader = T, sliderInput("bins", "Number of Breaks", 1, 100, 50),    # here bins is a ID
                    # textInput("input_id", "Search Opportunities", "12345"))
            )
        ),
        tabItem(
            tabName = "rawData",
            h2("rawData")
        )
    )
    
    # Create a tabBox
    # tabItems(
    #     tabItem(
    #         tabName = "dashboard",
    #         tabBox(
    #             title = "International Space Station Fun Facts",
    #             tabPanel("Fun Fact 1"),
    #             tabPanel("Fun Fact 2")
    #         )
    #     ),
    #     tabItem(tabName = "inputs")
    # )
)


shinyUI(
    dashboardPage(header, sidebar, body)
)


# ui <- dashboardPage(header = header,
#                     sidebar = sidebar,
#                     body = body
# )
# 
# server <- function(input, output) {}
# 
# shinyApp(ui, server)

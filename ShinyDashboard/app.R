library(shiny)
library(shinydashboard)

header <- dashboardHeader(
    title = "Brazilian Forest",
    dropdownMenu(
        type = "messages",
        messageItem(
            from = "Lucy",
            message = "You can view the International Space Station!",
            href = "https://spotthestation.nasa.gov/sightings/"
        ),
        # Add a second messageItem() 
        messageItem(
            from = "Lucy",
            message = "Learn more about the International Space Station",
            href = "https://spotthestation.nasa.gov/faq.cfm"
        )
        # type = "notifications",
        # notificationItem(
        #     text = "The International Space Station is overhead!",
        #     href = ""
        # )
        # type = "tasks",
        # taskItem(
        #     text = "The International Space Station is overhead!",
        #     value = 15 ------ (this is % of task completion)
        # )
    )
)

sidebar <- dashboardSidebar(
    sidebarMenu(
        # Create two `menuItem()`s, "Dashboard" and "Inputs"
        menuItem("Dashboard", tabName = "dashboard"),
        menuItem("Detailed Analysis", tabName = "detailedAnalysis"),
        menuItem("Raw Data", tabName = "rawData")
    )
)
body <- dashboardBody(
    fluidRow(
        box(plotOutput("histogram"))
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






# # Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white')
#     })
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)

library(shiny)
library(shinydashboard)
#/Users/francis/Library/CloudStorage/OneDrive-Personal/Documents/chapman/CS 614/FinalProject/code/level.png
# UI
ui <- dashboardPage(
  dashboardHeader(title = "REER Insights"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(
        title = "REER Analytics Hub",
        tags$a( img(src = "cover.jpg", height = 150, width = 300)),
        width = 8
      ),
      box(
        title = "Regression Analysis at Levels(Non Stationary) ",
                tags$a(href = "https://f2l4iu-francis0jeomoan-kurian.shinyapps.io/fx_level/",
               img(src = "level.png", height = 150, width = 300)),
        width = 8
      ),
      box(
        title = "Regression Analysis at First Differences",
            tags$a(href = "https://f2l4iu-francis0jeomoan-kurian.shinyapps.io/difference/",
               img(src = "diff.png", height = 150, width = 300)),
        width = 8
      )
    )
  )
)

# Server
server <- function(input, output) {
  # You can add server logic here if needed
}

# Run the Shiny app
shinyApp(ui, server)
#https://f2l4iu-francis0jeomoan-kurian.shinyapps.io/cover/

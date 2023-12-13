library(dplyr)
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(shinythemes)

file_path <- "ATHData-Lecture2-Geocoded.RData"
file_path <- "/Users/francis/Library/CloudStorage/OneDrive-Personal/Documents/chapman/CS 614/hw 8/ATHData-Lecture2-Geocoded.RData"
load(file_path)

ui = fluidPage(
  theme = shinytheme("cyborg"),
  titlePanel('Lecture 8 - ATH Homeless Survey Data', window = 'CS614 Example'),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("countryOutput"),
      radioButtons("barPlotDisplay", "Display Bar Plot", choices = c("Yes", "No"), selected = "No")
    ),
    
    mainPanel(
      br(),
      h3(textOutput("nOutput"), style = "color: blue;"),
      conditionalPanel(
        condition = "input.barPlotDisplay == 'Yes'",
        plotOutput("barplotOutput")
      ),
      plotOutput("scatterplotOutput"),
      br(),
      dataTableOutput("tableOutput")
    )
  )
)

server <- function(input, output) {
  
  filtered <- reactive({
    if(is.null(input$countryInput)) return(NULL)
    
    df %>%
      filter(A28_6_rgn %in% input$countryInput)
  })
  
  output$scatterplotOutput <- renderPlot({
    if(is.null(input$countryInput)) return(NULL)
    
    ggplot(filtered(), aes(x = A20_3, y = A27_1)) +
      geom_point() +
      geom_smooth(method = "lm", se = TRUE, col = "blue") +
      labs(x = "% of the homeless population that uses crack/cocaine", y = "Cost of sheltering a homeless person in LA")
  })
  
  output$barplotOutput <- renderPlot({
    if(is.null(input$countryInput)) return(NULL)
    
    ggplot(filtered(), aes(x = A28_2, y = A20_3)) +
      geom_bar(stat = "summary", fun = "mean", position = "dodge", fill = "darkgrey") +
      labs(x = "Respondent Gender", y = "Mean % of Homeless Population using Crack/Cocaine")
  })
  
  output$tableOutput <- renderDataTable({
    filtered()
  })
  
  output$nOutput <- renderText({
    nn <- nrow(filtered())
    paste("Based on your criteria, there were", nn, "results found.")
  })
  
  output$countryOutput <- renderUI({
    checkboxGroupInput("countryInput", "Country", 
                       choices = sort(unique(df$A28_6_rgn)),
                       selected = "orange")
  })
}

shinyApp(ui = ui, server = server)

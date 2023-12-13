library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(shinythemes)
library(shinydashboard)
library(ggrepel)

# Function to read and preprocess data
read_and_preprocess_data <- function(file_path) {
  df <- read.csv(file_path)
  df$IncidentDate1 <- as.Date(df$IncidentDate1, format = "%d-%b-%y")
  df$year <- as.integer(format(df$IncidentDate1, "%Y"))
  df$incidence_count <- 1
  df_subset <- df[c("year", "County2", "agency_name3", "Officer_Name2", "incidence_count")]
  return(df_subset)
}

# Function to create UI
create_ui <- function() {
  fluidPage(
    titlePanel("Use of Force by Police Officers by County and Year"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput("counties", "Select County", 
                    choices = names(sort(table(df_ind_subset$County2), decreasing = TRUE)),
                    selected = NULL),
        selectInput("agencies", "Select a Department", choices = c("ALL", NULL)),
        checkboxGroupInput("years", "Select Year", choices = unique(df_ind_subset$year), selected = unique(df_ind_subset$year))
      ),
      
      mainPanel(
        plotOutput("bubblePlot")
      )
    )
  )
}

# Function to create server
create_server <- function(df_subset) {
  function(input, output, session) {
    observe({
      # Update choices for counties only if there's no current selection
      if (is.null(input$counties)) {
        updateSelectInput(session, "counties", choices = sort(unique(df_subset$County2[df_subset$year %in% input$years])))
      }
    })
    
    observe({
      # Update choices for agencies based on selected counties
      agencies_choices <- c("ALL", unique(df_subset$agency_name3[df_subset$County2 %in% input$counties]))
      updateSelectInput(session, "agencies", choices = sort(agencies_choices))
    })
    
    # Reactive function to filter data based on user inputs
    filtered_data <- reactive({
      df_filtered <- df_subset %>%
        filter(County2 %in% input$counties)
      
      if ("ALL" %in% input$agencies) {
        # If "ALL" is selected, include all agencies under the selected county
        df_filtered <- df_filtered %>%
          filter(agency_name3 %in% unique(df_subset$agency_name3[df_subset$County2 %in% input$counties]))
      } else {
        # Filter by selected agency
        df_filtered <- df_filtered %>%
          filter(agency_name3 %in% input$agencies)
      }
      
      # Filter by selected years
      df_filtered <- df_filtered %>%
        filter(year %in% input$years) %>%
        filter(!is.na(Officer_Name2) & Officer_Name2 != "")
      
      return(df_filtered)
    })
    
    output$bubblePlot <- renderPlot({
      df_filtered <- filtered_data()
      if (is.null(df_filtered)) {
        # No records to plot, return an empty plot or display a message
        plot(NULL, xlim = c(0, 1), ylim = c(0, 1), main = "No incidents reported for the selection. Please try a different combination.", type = "n", xlab = "", ylab = "")
        return()
      }
      
      # Calculate the sum of incidence_count for each Officer_Name2
      officer_counts <- aggregate(incidence_count ~ Officer_Name2, data = df_filtered, sum)
      
      # Sort the officers by record count in descending order
      sorted_officers <- officer_counts[order(-officer_counts$incidence_count), ]
      
      # Select the top 10 officers
      top_10_officers <- head(sorted_officers, 10)
      
      ggplot(top_10_officers, aes(x = Officer_Name2, y = incidence_count, size = incidence_count, label = Officer_Name2)) +
        geom_point(alpha = 0.7, color = "#0072B2", fill = "#0072B2") + 
        geom_text_repel(
          box.padding = 0.5, point.padding = 0.5, force = 5, segment.color = 'grey',
          size = 4, color = "black"
        ) +  
        labs(title = "Forceful Police Incidents: Top 10 Officers and Counts",
             x = NULL,
             y = "Sum of Incidence Count",
             size = "Incidence Count") +
        scale_size(range = c(3, 15)) +
        theme_minimal() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.x = element_blank(),
              plot.background = element_rect(fill = "#E0E0E0"),
              plot.title = element_text(hjust = 0.5, size = 16))  # Use a light gray background
      
    })
  }
}

# Run the Shiny app
ind <- "UOF-BY-INDIVIDUALS_10_1_20-10_31_22.csv"
df_ind_subset <- read_and_preprocess_data(ind)
ui2 <- create_ui()
server2 <- create_server(df_ind_subset)
shinyApp(ui2, server2)

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(shinythemes)
library(shinydashboard)
library(ggrepel)


# Function to create App 1
createApp1 <- function(aggregated_data_incidents, aggregated_data_arrests) {
  # Define UI
  ui <- fluidPage(
    titlePanel("Incidents, Arrests, and Subject Percentages by County"),
    sidebarLayout(
      sidebarPanel(
        selectInput("county", "Select County", choices = unique(aggregated_data_incidents$county)),
        br(),
        selectInput("chart_type", "Select Chart", choices = c("Incidents", "Arrests", "Subject Percentages"))
      ),
      mainPanel(
        conditionalPanel(
          condition = "input.chart_type == 'Incidents'",
          plotOutput("incident_time_series")
        ),
        conditionalPanel(
          condition = "input.chart_type == 'Arrests'",
          plotOutput("arrest_time_series")
        ),
        conditionalPanel(
          condition = "input.chart_type == 'Subject Percentages'",
          plotOutput("subject_pie_chart")
        )
      )
    )
  )
  
  # Define server logic
  server <- function(input, output) {
    # Filter data based on user-selected county for incidents
    selected_data_incidents <- reactive({
      aggregated_data_incidents %>%
        filter(county == input$county)
    })
    # Filter data based on user-selected county for arrests
    selected_data_arrests <- reactive({
      aggregated_data_arrests %>%
        filter(county == input$county)
    })
    # Time series of total incidents for selected county
    output$incident_time_series <- renderPlot({
      req(selected_data_incidents())
      county_data_incidents <- selected_data_incidents()
      county_data_long_incidents <- county_data_incidents %>%
        pivot_longer(cols = starts_with("total_incidents"), names_to = "Year", values_to = "Incidents")
      ggplot(county_data_long_incidents, aes(x = Year, y = Incidents, group = 1, color = "Total Incidents")) +
        geom_line(size = 1.5) +
        geom_point(size = 3) +
        labs(title = paste("Total Incidents between 2012-2016 in", input$county), x = "Year", y = "Total Incidents") +
        scale_x_discrete(labels = c("total_incidents_2012" = "2012", "total_incidents_2013" = "2013", "total_incidents_2014" = "2014", "total_incidents_2015" = "2015", "total_incidents_2016" = "2016")) +
        theme_minimal() +
        theme(plot.title = element_text(size = 18, hjust = 0.5),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 14, face = "bold"))
    })
    # Time series of total arrests for selected county
    output$arrest_time_series <- renderPlot({
      req(selected_data_arrests())
      county_data_arrests <- selected_data_arrests()
      county_data_long_arrests <- county_data_arrests %>%
        pivot_longer(cols = starts_with("total_arrests"), names_to = "Year", values_to = "Arrests")
      ggplot(county_data_long_arrests, aes(x = Year, y = Arrests, group = 1, color = "Total Arrests")) +
        geom_line(size = 1.5) +
        geom_point(size = 3) +
        labs(title = paste("Total Arrests between 2012-2016 in", input$county), x = "Year", y = "Total Arrests") +
        scale_x_discrete(labels = c("total_arrests_2012" = "2012", "total_arrests_2013" = "2013", "total_arrests_2014" = "2014", "total_arrests_2015" = "2015", "total_arrests_2016" = "2016")) +
        theme_minimal() +
        theme(plot.title = element_text(size = 18, hjust = 0.5),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 14, face = "bold"))
    })
    # Pie chart for subject percentages for selected county
    output$subject_pie_chart <- renderPlot({
      req(selected_data_incidents())
      county_data_incidents <- selected_data_incidents()
      pie_data <- county_data_incidents %>%
        select(white_pct_subjects, black_pct_subjects, hispanic_pct_subjects, asian_pacific_islander_pct_subjects)
      pie_data_long <- stack(pie_data)
      names(pie_data_long) <- c("Percentage", "Subject")
      ggplot(pie_data_long, aes(x = "", y = Percentage, fill = Subject)) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start = 0) +
        labs(title = paste("Subject Percentages in", input$county), fill = "Subject") +
        theme_void() +
        theme(legend.position = "bottom")
    })
  }
  
  # Return UI and server functions
  return(list(ui = ui, server = server))
}

# Read the CSV file
file_path <- "UOF_BY_DEPARTMENTS.csv"
data <- read.csv(file_path)

# Handling missing values by replacing NA with 0 in the columns for each year
data[, grep("^total_incidents", names(data))] <- lapply(data[, grep("^total_incidents", names(data))], function(x) ifelse(is.na(x), 0, x))
data[, grep("^total_arrests", names(data))] <- lapply(data[, grep("^total_arrests", names(data))], function(x) ifelse(is.na(x), 0, x))

# Aggregate total incidents by year and county
aggregated_data_incidents <- data %>%
  group_by(county) %>%
  summarise(
    total_incidents_2012 = sum(total_incidents_2012),
    total_incidents_2013 = sum(total_incidents_2013),
    total_incidents_2014 = sum(total_incidents_2014),
    total_incidents_2015 = sum(total_incidents_2015),
    total_incidents_2016 = sum(total_incidents_2016),
    white_pct_subjects = mean(white_pct_subjects),
    black_pct_subjects = mean(black_pct_subjects),
    hispanic_pct_subjects = mean(hispanic_pct_subjects),
    asian_pacific_islander_pct_subjects = mean(asian_pacific_islander_pct_subjects)
  )

# Aggregate total arrests by year and county
aggregated_data_arrests <- data %>%
  group_by(county) %>%
  summarise(
    total_arrests_2012 = sum(total_arrests_2012),
    total_arrests_2013 = sum(total_arrests_2013),
    total_arrests_2014 = sum(total_arrests_2014),
    total_arrests_2015 = sum(total_arrests_2015),
    total_arrests_2016 = sum(total_arrests_2016)
  )

# Create App 1
app1 <- createApp1(aggregated_data_incidents, aggregated_data_arrests)

# Run the Shiny app
shinyApp(ui = app1$ui, server = app1$server)


library(shiny)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(sjPlot)
library(sjmisc)
library(ggfortify)
library(urca)
library(tseries)
library(knitr)
library(kableExtra)
library(car)
library(gridExtra)

df <- read.csv("forex_difference.csv")

# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("Real Effective Forex Rates and Stock Prices:Stationary"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("company", "Select Company:", choices = unique(df$company_name))
    ),
    mainPanel(
      uiOutput("scatterPlotTitle"),
      plotOutput("scatterPlot"),
      uiOutput("regressionTableTitle"),
      uiOutput("regressionTable"),
      uiOutput("residualPlotTitle"),
      plotOutput("residualPlot"),
      uiOutput("finalRegressionTableTitle"),
      uiOutput("finalRegressionTable"),
      uiOutput("stationarityCheckTitle"),
      tableOutput("stationarityCheck"),
      uiOutput("VIFTitle"),
      tableOutput("VIF"),
      uiOutput("cointegrationCheckTitle"),
      tableOutput("cointegrationCheck")
      
    )
  )
)

# Define server logic for the Shiny app
server <- function(input, output) {
  # Filtered data based on user inputs
  filtered_df <- reactive({
    df %>%
      filter(company_name == input$company)
  })
  
  output$scatterPlotTitle <- renderUI({
    HTML('<strong style="font-size: 18px;">Stock Prices and Exchange Rates Bahavior </strong>')
  })
  
  output$scatterPlot <- renderPlot({
    x_vars <- c(
      "USA_Dollar", "Brazil_Real", "China_Yuan",
      "Germany_Euro", "UK_Pound", "India_Rupee",
      "Japan_Yen", "Mexico_Peso"
    )
    
    ggplots <- lapply(x_vars, function(x_var) {
      ggplot(filtered_df(), aes_string(x = x_var, y = "Real_Stock_Price")) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE, color = "blue") +
        theme_minimal() +
        theme(plot.margin = margin(10, 10, 10, 10))  # Adjust margins as needed
    })
    
    num_plots <- length(ggplots)
    num_cols <- 2
    num_rows <- ceiling(num_plots / num_cols)
    
    scatter_arranged <- ggarrange(plotlist = ggplots, ncol = num_cols, nrow = num_rows)
    
    scatter_arranged
  })
  
  # Generate regression table based on user inputs
  output$regressionTableTitle <- renderUI({
    HTML('<strong style="font-size: 18px;">Base Regression Model </strong>')
  })
  output$regressionTable <- renderUI({
    multi.fit <- lm(Real_Stock_Price ~ USA_Dollar + Brazil_Real + China_Yuan +
                      Germany_Euro + UK_Pound + India_Rupee + Japan_Yen + Mexico_Peso,
                    data = filtered_df())
    
    tidy_results <- broom::tidy(multi.fit)
    tidy_results
    tidy_results[, -c(1, 5)] <- lapply(tidy_results[, -c(1, 5)], round, digits = 2)
    
    table_html <- knitr::kable(tidy_results, format = "html", caption = "Initial Regression Model ") %>%
      kable_styling(full_width = FALSE, position = "left") %>%
      column_spec(1:ncol(tidy_results), width = "100px")
    
    HTML(table_html)
  })
  
  
  # Generate residuals plot based on user inputs
  output$residualPlotTitle <- renderUI({
    HTML('<strong style="font-size: 18px;">Residual Analysis:Linear/Normal/Homoscedastic</strong>')
  })
  
  output$residualPlot <- renderPlot({
    multi.fit <- lm(Real_Stock_Price ~ USA_Dollar + Brazil_Real + China_Yuan +
                      Germany_Euro + UK_Pound + India_Rupee + Japan_Yen + Mexico_Peso,
                    data = filtered_df())
    autoplot(multi.fit)
  })
  
  output$finalRegressionTableTitle <- renderUI({
    HTML('<strong style="font-size: 18px;">Final Regression Model:Stepwise Regression</strong>')
  })
  
  
  output$finalRegressionTable <- renderUI({
    multi.fit <- lm(Real_Stock_Price ~ USA_Dollar + Brazil_Real + China_Yuan +
                      Germany_Euro + UK_Pound + India_Rupee + Japan_Yen + Mexico_Peso,
                    data = filtered_df())
    final_model <- step(multi.fit, direction = "both")
    final_model_formula <- formula(final_model)
    final.fit <<- lm(final_model_formula, data=filtered_df())
    tidy_results <- broom::tidy(final.fit)
    tidy_results[, -c(1, 5)] <- lapply(tidy_results[, -c(1, 5)], round, digits = 2)
    
    table_html <- knitr::kable(tidy_results, format = "html", caption = "Selection based on lowest AIC") %>%
      kable_styling(full_width = FALSE, position = "left") %>%
      column_spec(1:ncol(tidy_results), width = "100px")
    
    HTML(table_html)
  })
  
  
  # Output for ADF test results
  
  output$stationarityCheckTitle <- renderUI({
    HTML('<strong style="font-size: 18px;">Test of Stationarity: Dickey-Fuller Test Results</strong>')
  })
  
  
  output$stationarityCheck <- renderTable({
    filtered_df <- filtered_df()
    time_series_data <- ts(filtered_df[, c("Real_Stock_Price", "USA_Dollar", "Brazil_Real", "China_Yuan", 
                                           "Germany_Euro", "UK_Pound", "India_Rupee", "Japan_Yen", "Mexico_Peso")], 
                           start = 1, frequency = 1)
    
    extract_adf_results <- function(series) {
      adf_result <- as.numeric(adf.test(series)$statistic)
      p_value <- adf.test(series)$p.value
      return(c(ADF_Statistic = adf_result, P_Value = p_value))
    }
    
    adf_results <- lapply(time_series_data, extract_adf_results)
    adf_results_df <- data.frame(do.call(rbind, adf_results), row.names = names(adf_results))
    
    # Add a new column for row names
    adf_results_df$RowName <- row.names(adf_results_df)
    
    # Move the new column to the front
    adf_results_df <- adf_results_df[, c("RowName", setdiff(names(adf_results_df), "RowName"))]
    
    adf_results_df 
  })
  
  output$VIFTitle <- renderUI({
    HTML('<strong style="font-size: 18px;">Variance Inflation Factor</strong>')
  })
  
  
  output$VIF <- renderTable({
    multi.fit <- lm(Real_Stock_Price ~ USA_Dollar + Brazil_Real + China_Yuan +
                      Germany_Euro + UK_Pound + India_Rupee + Japan_Yen + Mexico_Peso,
                    data = filtered_df())
    final_model <- step(multi.fit, direction = "both")
    final_model_formula <- formula(final_model)
    final.fit <- lm(final_model_formula, data = filtered_df())
    
    tidy_results <- data.frame(
      Variable = names(car::vif(final.fit)),
      VIF = as.numeric(car::vif(final.fit))
    )
    
    tidy_results
  })
  
  # Output for Johansen Cointegarion
  
  output$cointegrationCheckTitle <- renderUI({
    HTML('<strong style="font-size: 18px;">Johansen Cointegration Test for Long-term equillibrium</strong>')
  })
  
  
  output$cointegrationCheck <- renderTable({
    filtered_df <- filtered_df()
    time_series_data <- ts(filtered_df[, c("Real_Stock_Price", "USA_Dollar", "Brazil_Real", "China_Yuan", 
                                           "Germany_Euro", "UK_Pound", "India_Rupee", "Japan_Yen", "Mexico_Peso")], 
                           start = 1, frequency = 1)
    
    johansen_result <- ca.jo(time_series_data, type = "eigen", K = 2)
    critical_values_df <- as.data.frame(slot(johansen_result, "cval"))
    test_statistics_df <- as.data.frame(slot(johansen_result, "teststat"))
    combined_df <- cbind(critical_values_df, test_statistics_df)
    combined_df$nvectors <- rownames(critical_values_df)
    colnames(combined_df)[ncol(critical_values_df) + 1] <- "teststat"
    combined_df$nvectors <- gsub("\\|", "", rownames(critical_values_df))
    combined_df <- combined_df[, c(ncol(combined_df), 1:(ncol(combined_df)-1))]
    combined_df$significance <- ifelse(combined_df$teststat > combined_df$`1pct` , 'Cointegrated', 'Not Cointegrated')
    combined_df 
  })
  
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
#https://f2l4iu-francis0jeomoan-kurian.shinyapps.io/difference/


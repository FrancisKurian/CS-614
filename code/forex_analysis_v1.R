# this code does the data manipulations and finally output two files used throughout the analytics process

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

# Set the current working directory
setwd("/Users/francis/Library/CloudStorage/OneDrive-Personal/Documents/chapman/CS 614/FinalProject/code/")

#read exchange rates

file_path1 <- "REER_database_ver19Nov2023.xls"
sheet_name <- "REER_MONTHLY_120"
df1 <- read_excel(file_path1, sheet = sheet_name)
df1$year <- substr(df1$dstamp, 1, 4)
df1$month <- substr(df1$dstamp, 6, 7)

df1$Date <- as.Date(paste0(df1$month, "/01/", df1$year), format = "%m/%d/%Y")

selected_columns <- grep("Date|_BR$|_CN$|_IN$|_JP$|_MX$|_GB$|_DE$|_US$", names(df1), value = TRUE)
df_ex <- df1[selected_columns]

# Assuming df_forex is your data frame
library(dplyr)

# Mapping of old names to new names
rename_mapping <- c(
  "REER_120_BR" = "Brazil_Real",
  "REER_120_CN" = "China_Yuan",
  "REER_120_DE" = "Germany_Euro",
  "REER_120_GB" = "UK_Pound",
  "REER_120_IN" = "India_Rupee",
  "REER_120_JP" = "Japan_Yen",
  "REER_120_MX" = "Mexico_Peso",
  "REER_120_US" = "USA_Dollar",
  "Date" ="Date"
)

colnames(df_ex) <- rename_mapping[match(colnames(df_ex), names(rename_mapping))]

# Read the CompanyNames.csv file
company_names_df <- read.csv("CompanyNames.csv")

# Initialize an empty list to store data frames for each stock
stock_data_list <- list()
for (i in seq_len(nrow(company_names_df))) {
  current_ticker <- company_names_df$ticker[i]
  current_name <- company_names_df$name[i]
  
  file_path <- paste0(current_ticker, ".csv")
  
  current_stock_data <- read.csv(file_path)
  
  current_stock_data$company_name <- current_name
  
  stock_data_list[[i]] <- current_stock_data
}

df_stock <- do.call(rbind, stock_data_list)

# Read Inflation data for US
file_path1 <- "CPIAUCSL.xls"
sheet_name <- "FED"
df_cpi <- read_excel(file_path1, sheet = sheet_name)

# Left join to keep only the rows from df_stock
df_merged <- merge(merge(df_ex, df_stock, by = 'Date', all.x = FALSE), df_cpi, by = 'Date', all.y = FALSE)
# Compute Real Stock Price
df_merged$Real_Stock_Price <- df_merged$Close / df_merged$CPI * 100
# Remove records after 2023-09-01
df_merged <- subset(df_merged, as.Date(Date) <= as.Date("2023-09-01"))


selected_columns <- c("Date", "company_name", "USA_Dollar", "Brazil_Real", "China_Yuan",
                      "Germany_Euro", "UK_Pound", "India_Rupee", "Japan_Yen", "Mexico_Peso",
                      "Close", "CPI", "Real_Stock_Price")

selected_df <- df_merged[selected_columns]
selected_df <- selected_df %>%
  arrange(company_name, Date)

write.csv(selected_df, "forex_level.csv", row.names = FALSE)

# create a stationary data frame

selected_df2 <- selected_df %>%
  arrange(company_name, Date) %>%  # Ensure data is sorted by company_name and Date
  group_by(company_name) %>%  # Group by company_name
  mutate(across(where(is.numeric), ~ c(NA, diff(.)), .names = "diff_{.col}")) %>%
  ungroup() %>%
  na.omit()  # Remove rows with any NA or missing values

selected_columns <- c("Date", "company_name", "diff_USA_Dollar", "diff_Brazil_Real", "diff_China_Yuan",
                      "diff_Germany_Euro", "diff_UK_Pound", "diff_India_Rupee", "diff_Japan_Yen", "diff_Mexico_Peso","diff_Real_Stock_Price")

selected_df2 <- selected_df2[selected_columns]
selected_df2 <- selected_df2 %>%
  rename_all(~ gsub("diff_", "", .))

write.csv(selected_df2, "forex_difference.csv", row.names = FALSE)


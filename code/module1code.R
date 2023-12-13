# Load the necessary library
library(lubridate)
library(ggplot2)
library(dplyr)

file_path <- "/Users/francis/Library/CloudStorage/OneDrive-Personal/Documents/chapman/CS 614/docs/module1/Module1Data.RData"
load(file_path)

df <- appdata

#a i What proportion of total sessions were associated with this feature? : 1%
table(df[['selected_mood']])
freq_table <- table(df$selected_mood, useNA = "ifany")
freq_df <- as.data.frame(freq_table)
colnames(freq_df) <- c("Value", "Frequency")
total <- sum(freq_df$Frequency, na.rm = TRUE)
freq_df$Percentage <- freq_df$Frequency / total * 100
print(freq_df)

#a ii) How many users used this feature at least once?

filtered_df <- df[df$selected_mood != '0', ]
unique_records <- filtered_df[!duplicated(filtered_df$ID), ]
# Print the count of unique records
cat("How many users used this feature at least once:", nrow(unique_records), "\n")

#b For those users who have used the Mood Tracking feature at least once, create a histogram showing the number of mood check-ins for each user
ggplot(filtered_df, aes(x = factor(ID))) +
  geom_bar(col = 'red', fill = 'blue') +
  labs(x = "ID", y = "Count") +
  theme_minimal()

#b Also include the mean, median, and standard deviation of your values

count_df <- filtered_df %>%
  count(ID)
mean_count <- mean(count_df$n)
median_count <- median(count_df$n)
sd_count <- sd(count_df$n)
cat("Mean: ", mean_count, "\n")
cat("Median: ", median_count, "\n")
cat("Standard Deviation: ", sd_count, "\n")



# Load the necessary libraries

df <- appdata
filtered_df <- df[df$selected_mood != '0', ]

# Filter the data frame to exclude 'no data' and 'uncertain' gender estimates
filtered_df_filtered <- filtered_df %>%
  filter(model_estimated_gender %in% c("male", "female"))

# Get unique values of selected_mood
mood_values <- unique(filtered_df_filtered$selected_mood)

# Create an empty list to store individual plots
plots_list <- list()

# Loop through each value in selected_mood and create proportion analysis plots
for (mood_value in mood_values) {
  proportion_male <- filtered_df_filtered %>%
    filter(selected_mood == mood_value & model_estimated_gender == "male") %>%
    summarise(Count = n())
  
  proportion_female <- filtered_df_filtered %>%
    filter(selected_mood == mood_value & model_estimated_gender == "female") %>%
    summarise(Count = n())
  
  proportion_df <- data.frame(
    Gender = c("Male", "Female"),
    Proportion = c(proportion_male$Count / sum(filtered_df_filtered$model_estimated_gender == "male") * 100,
                   proportion_female$Count / sum(filtered_df_filtered$model_estimated_gender == "female") * 100)
  )
  
  comparison_bar <- ggplot(proportion_df, aes(x = Gender, y = Proportion, fill = Gender)) +
    geom_bar(stat = "identity", col = 'red') +
    labs(x = "Gender", y = "Percentage") +
    scale_fill_manual(values = c("Male" = "blue", "Female" = "pink")) +
    theme_minimal() +
    ggtitle(paste("Selected Mood: ", mood_value))  # Add a title
  
  plots_list[[mood_value]] <- comparison_bar
}

# Display the  proportion analysis plots
for (plot in plots_list) {
  print(plot)
}



#  regroup moods into two broad categores

mapping <- c(
  "angry" = "bad",
  "anxious" = "bad",
  "bored" = "bad",
  "sad" = "bad",
  "stressed" = "bad",
  "tired" = "bad",
  "unsure" = "bad",
  "content" = "good",
  "ecstatic" = "good",
  "excited" = "good",
  "grateful" = "good",
  "happy" = "good",
  "relaxed" = "good"
)

# Create the new field "mood_group" based on the mapping
mood_df <- filtered_df_filtered  %>%
  mutate(mood_group = ifelse(selected_mood %in% names(mapping), mapping[selected_mood], "Other"))

mood_df <- mood_df %>%
  mutate(start_date = ymd_hms(start_date),
         date = ymd_hms(date),
         months_on_books = round(as.numeric(interval(start_date, date) / months(1)))
  )


# Create a side-by-side bar graph for males and females
bar_plot <- ggplot(mood_df, aes(x = months_on_books, fill = mood_group)) +
  geom_bar(position = "dodge", color = "black") +
  labs(x = "Months on Books", y = "Count") +
  scale_fill_manual(values = c("bad" = "red", "good" = "green", "Other" = "gray")) +
  theme_minimal() +
  scale_x_continuous(breaks = unique(mood_df$months_on_books)) +
  facet_grid(model_estimated_gender ~ .)  # Separate graphs for male and female

print(bar_plot)




# Load the necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
# Assuming you have a dataframe 'df' with 'months_on_books', 'mood_group', and 'model_estimated_gender'

# Calculate counts and the ratio of good/bad for males
male_data <- mood_df %>%
  filter(model_estimated_gender == "male") %>%
  group_by(months_on_books, mood_group) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = mood_group, values_from = count, values_fill = 0) %>%
  mutate(ratio = good / bad)

# Calculate counts and the ratio of good/bad for females
female_data <- mood_df %>%
  filter(model_estimated_gender == "female") %>%
  group_by(months_on_books, mood_group) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = mood_group, values_from = count, values_fill = 0) %>%
  mutate(ratio = good / bad)

# Combine the male and female data into one dataframe
# Create separate graphs for Male and Female
male_plot <- ggplot(male_data, aes(x = months_on_books, y = ratio)) +
  geom_line(color = "blue") +
  labs(x = "Months on Books", y = "Good/Bad Ratio") +
  ggtitle("Good/Bad Ratio Over Time for Males") +
  theme_minimal() +
  geom_smooth(method = "lm", color = "black") +
  scale_x_continuous(breaks = unique(mood_df$months_on_books))

female_plot <- ggplot(male_data, aes(x = months_on_books, y = ratio)) +
  geom_line(color = "pink") +
  labs(x = "Months on Books", y = "Good/Bad Ratio") +
  ggtitle("Good/Bad Ratio Over Time for Females") +
  theme_minimal() +
  geom_smooth(method = "lm", color = "black") +
  scale_x_continuous(breaks = unique(mood_df$months_on_books))

# Display the two graphs side by side
library(gridExtra)
grid.arrange(male_plot, female_plot, ncol = 2)


model <- lm(ratio ~ months_on_books, data = female_data)

# Print a summary of the regression model
summary(model)



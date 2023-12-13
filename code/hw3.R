#directory <- "/Users/francis/Library/CloudStorage/OneDrive-Personal/Documents/chapman/CS 614/docs/Hw2/"
#ccA <- "countrycharsA-1.csv"
#df_ccA <- read.csv(file.path(directory, ccA))


library(dplyr)
library(mice)
library(caret)
library(naniar)

  #We imputed continuous variables using linear regression models and binary variables using logistic regression models. 
  #Although some of the continuous variables were skewed, they were imputed on the raw scale (i.e. without transformation) 
  #irrespective of their distribution [10, 28]. MI was implemented using the mi impute chained command in 
  #Stata software version 14.1 [29]. We generated 40 imputed (“completed”) datasets based on the 
  #rule of thumb that the number of imputations should be at least equal to the percentage of incomplete cases
  #(which was 38% in this case) [19].


  load("/Users/francis/Library/CloudStorage/OneDrive-Personal/Documents/chapman/CS 614/docs/Hw3/HW3.RData")

# Important to search for these anomalous values using table()  and then manually set to NA



cat_vars <- names(df)[sapply(df, is.factor)]
for (var in cat_vars) {
  freq_table <- table(df[[var]])
  print(paste("Frequency table for", var))
  print(freq_table)
}

# Calculate the summary statistics for the numeric field
num_variables <- df %>% select(where(is.numeric))
summary(num_variables)


impute_df <- df
# Replace "unknown" with NA in the "gender" and "Race" variables

impute_df$Gender[impute_df$Gender == "unknown"] <- NA
impute_df$Race[impute_df$Race == "unknown"] <- NA


missing_values <- colSums(is.na(impute_df))
print(missing_values)



# Perform Little's MCAR test on the data frame
mcar_test(impute_df)
#If the p-value is less than 0.05, the missing values are not MCAR. So they are MAR.

missing_data_matrix <- aggr(impute_df, col = c("blue", "red"), numbers = TRUE, plot = TRUE)


impute_df <- impute_df %>% select(-Smile)

# Focus on area variable

imp <- mice(impute_df, m = 20, method = c("pmm","","","","pmm","","","","pmm", "norm"),  print=T)
warnings()

missing_values <- colSums(is.na(impute_df))
print(missing_values)
missing_values <- colSums(is.na(complete(imp, action = 20)))
print(missing_values)


imp2 <- complete(imp, action = 20)


# Define the formula for the GLM model
formula <- as.formula("Area ~ Gender + Adult + Face_Angle + Image_Color + Image_Quality + Image_Type + Context + Multiface + Race ")


# Create a list to store the fitted GLM models
glm_models <- list()
aic_values <- numeric()
# Loop through each imputed dataset
for (i in 1:20) {  
  imputed_data <- complete(imp, action = i)
  
  # Fit a GLM model to the imputed data
  glm_model <- glm(formula, data = imputed_data, family = gaussian)  # Adjust family and formula as needed
  glm_models[[i]] <- glm_model  # Store the fitted model
  }

# Pool the results using 'pool' function from 'mice'
pooled_model <- pool(glm_models)
summary(pooled_model)




# Fit a linear regression model prior to imputation
model <- lm(Area ~ Gender + Adult + Face_Angle + Image_Color + Image_Quality + Image_Type + Context + Multiface + Race , data = impute_df)
summary(model)


# Fit your linear regression model
model <- lm(Area ~ Gender + Adult + Face_Angle + Image_Color + Image_Quality + Image_Type + Context + Multiface + Race, data = impute_df)

# Calculate residuals
residuals <- residuals(model)

# Create a scatterplot of residuals vs. fitted values
plot(fitted(model), residuals)

# Identify observations with large residuals
outliers <- which(abs(residuals) > 2 * sd(residuals))  # Adjust the threshold as needed

# Print or analyze the outliers
print(outliers)

impute_df2 <- impute_df[-outliers, ]
                        
model <- lm(Area ~ Gender + Adult + Face_Angle + Image_Color + Image_Quality + Image_Type + Context + Multiface + Race, data = impute_df2)
summary(model)
# fit a regression for one of the extracts

model <- lm(Area ~ Gender + Adult + Face_Angle + Image_Color + Image_Quality + Image_Type + Context + Multiface + Race , data = imp2)
summary(model)


# Kernel Density Plot- Data from a single imputed dataset have been presented

imp2 <- complete(imp, action = 20)

# Assuming impute_df contains observed data and imp2 contains imputed data

# Add a grouping variable to distinguish observed and imputed values
impute_df$Group <- "Observed"
imp2$Group <- "Imputed"

# Combine the observed and imputed data into a single data frame
combined_df <- rbind(impute_df, imp2)

# Load the ggplot2 package if it's not already loaded
library(ggplot2)

# Create a Kernel density plot
ggplot(combined_df, aes(x = Area, linetype = Group, color = Group)) +
  geom_density(alpha = 0.5) +
  scale_linetype_manual(values = c("Observed" = "dashed", "Imputed" = "solid")) +
  scale_color_manual(values = c("Observed" = "black", "Imputed" = "red")) + 
  labs(title = "Kernel Density Plot of 'Area'",
       x = "Area",
       y = "Density") +
  theme(legend.position = "bottom")



# Create a box plot of the imputed datasets
imputed_datasets <- list()
for (i in 1:20) {
  imputed_data <- complete(imp, action = i)
  # Add an 'action' field to the imputed dataset and store the action number
  imputed_data$action <- i
  imputed_datasets[[i]] <- imputed_data
}

imputed_data <- do.call(rbind, imputed_datasets)
impute_df2 <- impute_df %>% select(-Group)
impute_df2$action <- 0
df_imp <- rbind(imputed_data, impute_df2)

df_l = complete(imp, action="long", include = TRUE)
mids_df_l = as.mids(dflong)
bwplot(mids_df_l, Area)


## LOOCV 
# creates 5% sample across the 20 imputation
# fit a model, LOOCV
# compare actual vs imputed
# plot


# Create a sample of 5% of the training data
library(dplyr)
library(tidyr)

sampled_data <- impute_df %>%
  drop_na() %>%
  sample_frac(0.05)


#Follow the steps below and develop R code for performing LOOCV :
  
#1. use data frame: sampled_data
#2. use the model: Area ~ Gender + Adult + Face_Angle + Image_Color + Image_Quality + Image_Type + Context + Multiface + Race
#3. fit the model and perform loocv 
#4. generate Average MSE and Average R Square statistics to compare observed vs predicted outcome
#5. scatter plot observed vs predicted values with a trendline
# Install and load the necessary packages
#install.packages("mice")
l# Install and load the necessary packages
#install.packages("mice")




# Initialize vectors to store results
mse_values <- numeric()
rsq_values <- numeric()
observed_values <- numeric()
predicted_values <- numeric()

# Perform LOOCV
for (i in 1:nrow(sampled_data)) {
  # Create training set without the i-th observation
  training_data <- sampled_data[-i, ]
  
  # Create a test set with only the i-th observation
  test_data <- sampled_data[i, ]
  
  # Perform regression on the imputed dataset
  model <- with(training_data, lm(Area ~ Gender + Adult + Face_Angle + Image_Color + Image_Quality + Image_Type + Context + Multiface + Race))
  
  
  # Predict on the test set
  predictions <- predict(model, newdata = test_data)
  
  # Calculate Mean Squared Error (MSE)
  mse <- mean((test_data$Area - predictions)^2)
  mse_values <- c(mse_values, mse)
  
  # Calculate R-Square
  rsq <- 1 - (sum((test_data$Area - predictions)^2) / sum((test_data$Area - mean(test_data$Area))^2))
  rsq_values <- c(rsq_values, rsq)
  
  # Store observed and predicted values
  observed_values <- c(observed_values, test_data$Area)
  predicted_values <- c(predicted_values, predictions)
}

# Calculate average MSE and average R-Square
average_mse <- mean(mse_values)
average_rsq <- mean(rsq_values)

cat("Average MSE:", average_mse, "\n")
cat("Average R-Square:", average_rsq, "\n")

# Create a scatter plot of observed vs. predicted values with a trendline
scatter_data <- data.frame(Observed = observed_values, Predicted = predicted_values)

ggplot(scatter_data, aes(x = Observed, y = Predicted)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Observed vs. Predicted Values", x = "Observed", y = "Predicted")

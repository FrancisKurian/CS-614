# Define the directory where the CSV files are located
directory <- "/Users/francis/Documents/CS614/Hw2/"

#load R Dataset provided for coords

load("/Users/francis/Documents/CS614/Hw2/map.coords.RData")

# assign the files to read 

ccA <- "countrycharsA-1.csv"
ccB <- "countrycharsB-1.csv"
gdp <- "gdp-1.csv"

# Create data frames 
df_ccA <- read.csv(file.path(directory, ccA))
df_ccB <- read.csv(file.path(directory, ccB))
df_gdp <- read.csv(file.path(directory, gdp))
df_coords <- coords

# Use the str()command to examine the characteristics of each of the four databases. Then append ccA to ccB to make one large data frame and reexamine the output with str().

cat("Data Frame Name: df_ccA\n\n")
str(df_ccA)
cat("Data Frame Name: df_ccB\n\n")
str(df_ccB)
cat("Data Frame Name: df_gdp\n\n")
str(df_gdp)
cat("**Data Frame Name: df_coords**\n\n")
str(df_coords)

# apped A&B 

df_ccAB <- rbind(df_ccA, df_ccB)
cat("**Data Frame Name: df_ccAB**\n\n")
str(df_ccAB)

# Append GDP dataset
df_ccAB_gdp <- cbind(df_ccAB, df_gdp)
cat("**Data Frame Name: df_ccAB_gdp**\n\n")
str(df_ccAB_gdp)

#graph one country as a sanity check for merging
barplot(df_ccAB_gdp[df_ccAB_gdp$country == 'United States', "gdp"], 
        names.arg = df_ccAB_gdp[df_ccAB_gdp$country == 'United States', "year"],
        xlab = "Year", 
        ylab = "GDP")


# Load the dplyr library
library(dplyr)

# Filter for the records with gdp<20000 after 1980
df_filtered <- df_ccAB_gdp %>%
  filter(year >= 1980 & gdp < 20000)

print(nrow(df_filtered))


#4. Merge your data frame from 1 &2 above with the coords data via three different techniques: inner, right, and left merge. Compare the dimensions 

df_inner <- merge(df_ccAB_gdp, df_coords, by = "country")
df_right <- merge(df_ccAB_gdp, df_coords, by = "country", all.y = T)
df_left  <- merge(df_ccAB_gdp, df_coords, by = "country", all.x = T)

str(df_inner)
str(df_right)
str(df_left)


# These countries are missing in Coordinates data.
not_matching_ccAB_gdp <- anti_join(df_ccAB_gdp, df_inner, by = "country")
head(unique(not_matching_ccAB_gdp$country), 20)

# These countries are missing in GDP data.
not_matching_coords <- anti_join(df_coords, df_inner, by = "country")
head(unique(not_matching_coords$country), 20)


#5

#5A Keep most recent record of GDP to simplify the analysis as objective is to establish the GDP vs latitude link.

df_recent_gdp <- df_inner%>%
  group_by(country) %>%
  filter(year == max(year)) %>%
  ungroup()

#5B Create a look up list for Q1 Q2 and Q3 break points 

gdp_quartiles <- quantile(df_recent_gdp$gdp, probs = c(0.25, 0.5, 0.75),na.rm = TRUE)
print(gdp_quartiles)

#5C Create a new variable 'gdp.q' based on quartiles and assign a value to represent the quarter 
df_recent_gdp$gdp.q <- findInterval(df_recent_gdp$gdp, gdp_quartiles)+1

# Print a frequency table of the 'gdp.q' variable
table(df_recent_gdp$gdp.q)


# Group by 'gdp.q' and calculate the mean absolute value of 'lat' for each group
result <- df_recent_gdp %>%
  group_by(gdp.q) %>%
  summarize(mean_abs_lat = mean(abs(lat)))

print(result)

#write.csv(df_ccAB_gdp, "df_ccAB_gdp.csv")

gdp_quartiles <- quantile(df_inner$gdp, probs = c(0.25, 0.5, 0.75),na.rm = TRUE)
print(gdp_quartiles)

df_inner$gdp.q <- findInterval(df_inner$gdp, gdp_quartiles)+1

# Print a frequency table of the 'gdp.q' variable
table(df_inner$gdp.q)

result <- df_inner %>%
  group_by(gdp.q) %>%
  summarize(mean_abs_lat = mean(abs(lat)))

print(result)

print(result)
library(ggplot2)

ggplot(result, aes(x = gdp.q, y = mean_abs_lat)) +
  geom_line() +
  labs(x = "GDP Quartile", y = "Mean Absolute Latitude") +
  ggtitle("GDP Quartile vs. Mean Absolute Latitude")



require(DAAG)
require(ggplot2)
require(lme4)
require(dplyr)
require(lmerTest)
library(MASS)

directory <- "/Users/francis/Library/CloudStorage/OneDrive-Personal/Documents/chapman/CS 614/docs/Hw6/"
pol <- "politeness_data-1.csv"
df <- read.csv(file.path(directory, pol))

boxplot(df$frequency ~ df$scenario, xlab = "Scenario", ylab = "Frequency", main = "Boxplot of Frequency by Scenario")
boxplot(df$frequency ~ df$subject, xlab = "Subject", ylab = "Frequency", main = "Boxplot of Frequency by Subject")
boxplot(df$frequency ~ df$attitude, xlab = "Attittude", ylab = "Frequency", main = "Boxplot of Frequency by Attittude")


# Build Model 1 - Random intercept model# Add fixed effect
model1 <- lmer(frequency ~ gender + attitude + (1 | subject), data = df)
# Summarize the model
summary(model1)
AIC(model1)

#Plot density of random effects
r1 = unlist(ranef(model1)$subject)
plot(density(r1))


# 1. Summarize the fixed effects of Model 1. 

# gender coefficient is -108.205 when it is Male, which means while controling for attitude, the frequency is lower by 108.205 units for male participants compared to female participants. And its significant at 99% confidence intrval'
# Attitude  estimate is -19.410, meaning voice frequency decreases by 19.410 units when it is polite , controlling for 'gender.' Again a signficant variable at 99% confidence interval level based on t values.
# Both these confirm the visual shifts we noticed in the Box Plot diagram

# In summary, the fixed effects analysis indicates that gender and attitude both have significant effects on the frequency of the dependent variable. Specifically, being male is associated with a decrease in frequency, and as participants' attitudes deviate from neutrality (become more positive or negative), the frequency also decreases. These findings provide valuable insights into how gender and attitude are related to the frequency of the observed behavior.

# Model 2
#Model 2: Random slope model with subject as the random effect, but with attitude as the slope effect. Still
#have gender + attitude as predictor variables.


model2 <- lmer(frequency ~ gender + attitude + (1 + attitude | subject), data = df)

#In this code, the (1 + attitude | subject) term specifies that you want to include a random intercept and a random slope for 'attitude' for each subject. This random slope allows the effect of 'attitude' to vary across subjects.

summary(model2)
#In your specific case, you included a random slope for 'attitudepol,' but the effect of 'attitudepol' remains similar to that in Model 1, which only includes a random intercept. This suggests that the subject-specific variation in the relationship between 'attitudepol' and 'frequency' may not be substantial.

#Random Intercept for Each Participant: The (1 | subject) part of the model structure in both Model 1 and Model 2 allows each participant to have their own frequency intercept. This means that the baseline frequency can vary from one participant to another, capturing the individual differences in the outcome variable.
#Random Slope for Attitude: Model 2 goes a step further by including a random slope for the 'attitude' variable within each participant (subject). This means that in Model 2, not only do participants have their own baseline frequency, but they also have their own unique effect of 'attitude' on the frequency. This acknowledges that the relationship between 'attitude' and 'frequency' can differ from one participant to another.
#In summary, Model 2 offers a more detailed and personalized approach to modeling the data. It recognizes that participants may not only differ in their baseline frequency but also in how they respond to changes in their attitude. This is particularly useful when there is reason to believe that individual responses to 'attitude' may vary, and it allows for a more accurate representation of the data when such variability exists.

# Extract random effects

###Random slope model

library(MASS)

random_effects_model2 <- ranef(model2)
random_intercepts <- random_effects_model2$subject$`(Intercept)`
random_slopes <- random_effects_model2$subject$attitude

# Create a KDE plot for random intercepts
ggplot(data = data.frame(RandomIntercept = random_intercepts), aes(x = RandomIntercept)) +
  geom_density() +
  labs(title = "Kernel Density Estimate of Random Intercepts in Model 2") +
  theme_minimal()

# Create a KDE plot for random slopes
ggplot(data = data.frame(RandomSlope = random_slopes), aes(x = RandomSlope)) +
  geom_density() +
  labs(title = "Kernel Density Estimate of Random Slopes in Model 2") +
  theme_minimal()


# Model 3:
# Build Model 3 - Random intercept model with subject and scenario as random effects
model3 <- lmer(frequency ~ gender + attitude + (1 | subject) + (1 | scenario), data = df)

# Summarize the model
summary(model3)
r3 = unlist(ranef(model3)$subject)
plot(density(r3))



AIC(model1,model2,model3)
anova(model1,model2,model3)


summary(model1)
summary(model2)
summary(model3)


model2 <- lmer(frequency ~ gender + attitude + (1 + attitude | subject), data = df)

###Calculation of variance partition coef function
u0sq = data.frame(VarCorr(model2))[1,4]
u1sq = data.frame(VarCorr(model2))[2,4]
u01 = data.frame(VarCorr(model2))[3,4]
uwsq = data.frame(VarCorr(model2))[4,4]

# Calculate the VPC
VPC = (u0sq + u1sq) / (u0sq + u1sq + uwsq)




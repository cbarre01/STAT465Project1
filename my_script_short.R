# Packages Required
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(aod) # Logistic Regression
library(car) # VIF
library(InformationValue) # Concordance Rates and ROC
library(lme4) # Random Slopes
library(multcomp) # Pairwise Comparisons

# Turn Scientific Notation Off
#options(scipen = 999)

### Dataset Manipulation
# Import Dataset
data <- read_excel("~/Stat465_C1/client_data.xlsx")

# Rename/Drop Columns For Convience
names(data) <- c("id", "school", "gender_identity", "sexual_orientation", "sexual_orientation_other", "race_ethnicity", 
                 "race_ethnicity_other", "year", "first_generation_college_student", "greek_life", "athlete",
                 "out_of_state", "transfer_student", "empty", "sexual_harassment", "alcohol", "hate_speech", "hazing")
data <- data[, !names(data) %in% c("empty", "sexual_harassment", "alcohol")] # drop empty collumn

# Remove Observations With Other Or Decline To State Responses
data <- data[which(!data$gender_identity %in% c("Decline to state", "Another Identity (please specify):")),]
data <- data[which(!data$sexual_orientation %in% c("Decline to state", "Other (please specify)")),]
data <- data[which(!data$race_ethnicity %in% c("Decline to state", "Other")),]



## Recoding Values
# Recode Indicators
data$first_generation_college_student_ind <- replace_na(as.numeric(dplyr::recode(data$first_generation_college_student, 
                                                                                 "First-Generation College Student" = 1)), 0)
data$greek_life_ind <- replace_na(as.numeric(dplyr::recode(data$greek_life, "Greek Affiliated" = 1)), 0)
data$athlete_ind <- replace_na(as.numeric(dplyr::recode(data$athlete, "Intercollegiate Student-Athlete" = 1)), 0)
data$out_of_state_ind <- replace_na(as.numeric(dplyr::recode(data$out_of_state, "Out of State Student" = 1)), 0)
data$transfer_student_ind <- replace_na(as.numeric(dplyr::recode(data$transfer_student, "Transfer Student" = 1)), 0)

# Recode Responses
data$hate_speech_ind <- replace_na(as.numeric(ifelse(data$hate_speech == "Never", 0, 1)), 0)
data$hazing_ind <- replace_na(as.numeric(ifelse(data$hazing == "Never", 0, 1)), 0)



##### Logistic Regression
# Modelling Dataset
my_data <- data[c("id", "school", "gender_identity", "sexual_orientation", "race_ethnicity", "year", "first_generation_college_student_ind",
                  "greek_life_ind", "athlete_ind", "out_of_state_ind", "transfer_student_ind", "hate_speech_ind", "hazing_ind")]

# Variables as Factor
my_data$gender_identity <- factor(my_data$gender_identity)
my_data$sexual_orientation <- factor(my_data$sexual_orientation)
my_data$race_ethnicity <- factor(my_data$race_ethnicity)
my_data$year <- factor(my_data$year)

# Alternate Codings For Categorical Variables
my_data$first_yr_ind <- dplyr::recode(my_data$year, "Fifth-year" = 0, "First-year" = 1, "Fourth-year" = 0,
                                      "More than Five Years" = 0, "Second-year" = 0, "Third-year" = 0)



####### FINAL MODEL #1 #######
## Model 4 - Removed First Gen College Student 
model4 <- glm(hate_speech_ind ~ gender_identity + sexual_orientation + year
              + greek_life_ind + athlete_ind + transfer_student_ind, data = my_data, family = "binomial")
# VIF check
vif(model4)
# Wald Tests For Fixed Effects
wald.test(b = coef(model4), Sigma = vcov(model4), Terms = 2:5) # Gender Identity p-value
wald.test(b = coef(model4), Sigma = vcov(model4), Terms = 6:12) # Sexual Orientation p-value
wald.test(b = coef(model4), Sigma = vcov(model4), Terms = 13:17) # Year Identity p-value
summary(model4)
# Concordance
pred4 <- plogis(predict(model4, my_data))  # Predicted Scores
cord4 <- Concordance(my_data$hate_speech_ind, pred4)
# Coeficients
exp(cbind(OR = coef(model4), confint(model4, level = 0.80)))
# Pairwise
# summary(glht(model4, linfct = mcp(sexual_orientation = "Tukey"))) # if pairwise comparisons are necessary



####### FINAL MODEL #1R #######
## Model 10 - Remove Sexual Orientation 
model10 <- glmer(hate_speech_ind ~ gender_identity + first_yr_ind
                + greek_life_ind + (1 | school), data = my_data, family = "binomial")
# VIF check
vif(model10)
# Wald Tests For Fixed Effects
wald.test(b = fixef(model10), Sigma = vcov(model10), Terms = 2:5) # Gender Identity p-value
summary(model10)
# Concordance
pred10 <- plogis(predict(model10, my_data))  # Predicted Scores
cord10 <- Concordance(my_data$hate_speech_ind, pred10)
# Coeficients And Confidence Intervals
se <- sqrt(diag(vcov(model10)))
tab <- cbind(Est = fixef(model10), LL = fixef(model10) - 1.645 * se, UL = fixef(model10) + 1.645 *
                se) # table of estimates with 90% CI
exp(tab)



####### FINAL MODEL #2 #######
## Model 8b - Add Indicator For First Year
model8b <- glm(hazing_ind ~ first_yr_ind + first_generation_college_student_ind
               + greek_life_ind + athlete_ind, data = my_data, family = binomial(link = "logit"))
# VIF check
vif(model8b)
# Wald Tests For Fixed Effects
summary(model8b)
# Concordance
pred8b <- plogis(predict(model8b, my_data, type="response"))  # Predicted Scores
cord8b <- Concordance(my_data$hate_speech_ind, pred8b)
# Coeficients
exp(cbind(OR = coef(model8b), confint(model8b, level = 0.80)))



####### FINAL MODEL #2R #######
## Model11b - Back To First Year Indicator, Add Sexual Orientation
model11b <- glmer(hazing_ind ~ first_yr_ind + first_generation_college_student_ind
                  + greek_life_ind + athlete_ind + (1 | school), data = my_data, family = binomial(link = "logit"))
# VIF check
vif(model11b)
# Wald Tests For Fixed Effects
wald.test(b = fixef(model11b), Sigma = vcov(model11b), Terms = 3:9) # Year p-value
summary(model11b)
# Concordance
pred11b <- plogis(predict(model11b, my_data, type="response"))  # Predicted Scores
cord11b <- Concordance(my_data$hate_speech_ind, pred11b) 
# Coeficients And Confidence Intervals
se2 <- sqrt(diag(vcov(model11b)))
tab2 <- cbind(Est = fixef(model11b), LL = fixef(model11b) - 1.645 * se2, UL = fixef(model11b) + 1.645 *
               se2) # table of estimates with 90% CI
options(scipen = 0)
exp(tab2)


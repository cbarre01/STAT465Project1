# Packages Required
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(aod) # Logistic Regression
library(car) # VIF
library(InformationValue) # Concordance Rates and ROC
library(lme4) # Random Slopes

### Dataset Manipulation
# Import Dataset
data <- read_excel("client_data.xlsx")

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



### Hate Speech
# Modelling is done using backwards elimination at alpha .1 significance level

## Model 1 - All Explanatory (First Order Only)
model <- glm(hate_speech_ind ~ gender_identity + sexual_orientation + race_ethnicity + year + first_generation_college_student_ind
             + greek_life_ind + athlete_ind + out_of_state_ind + transfer_student_ind, data = my_data, family = binomial(link = "logit"))
# VIF check
vif(model)
# Wald Tests For Fixed Effects
wald.test(b = coef(model), Sigma = vcov(model), Terms = 2:5) # Gender Identity p-value
wald.test(b = coef(model), Sigma = vcov(model), Terms = 6:12) # Sexual Orientation p-value
wald.test(b = coef(model), Sigma = vcov(model), Terms = 13:18) # Race/Ethnicity p-value
wald.test(b = coef(model), Sigma = vcov(model), Terms = 19:23) # Year p-value
summary(model)

# Concordance
pred <- plogis(predict(model, my_data, type="response"))  # Predicted Scores
cord <- Concordance(my_data$hate_speech_ind, pred)


## Model 2 - Removed Out Of State Student
model2 <- glm(hate_speech_ind ~ gender_identity + sexual_orientation + race_ethnicity + year + first_generation_college_student_ind
             + greek_life_ind + athlete_ind + transfer_student_ind, data = my_data, family = "binomial")
# VIF check
vif(model2)
# Wald Tests For Fixed Effects
wald.test(b = coef(model2), Sigma = vcov(model2), Terms = 2:5) # Gender Identity p-value
wald.test(b = coef(model2), Sigma = vcov(model2), Terms = 6:12) # Sexual Orientation p-value
wald.test(b = coef(model2), Sigma = vcov(model2), Terms = 13:18) # Race/Ethnicity p-value
wald.test(b = coef(model2), Sigma = vcov(model2), Terms = 19:23) # Year p-value
summary(model2)


## Model 3 - Removed Race/Ethnicity
model3 <- glm(hate_speech_ind ~ gender_identity + sexual_orientation + year + first_generation_college_student_ind
              + greek_life_ind + athlete_ind + transfer_student_ind, data = my_data, family = "binomial")
# VIF check
vif(model3)
# Wald Tests For Fixed Effects
wald.test(b = coef(model3), Sigma = vcov(model3), Terms = 2:5) # Gender Identity p-value
wald.test(b = coef(model3), Sigma = vcov(model3), Terms = 6:12) # Sexual Orientation p-valuewald.test(b = coef(model2), Sigma = vcov(model2), Terms = 13:18) # Race/Ethnicity p-value
wald.test(b = coef(model3), Sigma = vcov(model3), Terms = 13:17) # Year p-value
summary(model3)


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
cord4 <- Concordance(my_data$hate_speech_ind, pred)

##### I AM HERE #####
## Model 5 - Random Intercepts For School
model5 <- glmer(hate_speech_ind ~ gender_identity + sexual_orientation + year
              + greek_life_ind + athlete_ind + transfer_student_ind + (1 | school), data = my_data, family = "binomial")
# VIF check
vif(model5)
# Wald Tests For Fixed Effects
wald.test(b = fixef(model5), Sigma = vcov(model5), Terms = 2:5) # Gender Identity p-value
wald.test(b = fixef(model5), Sigma = vcov(model5), Terms = 6:12) # Sexual Orientation p-value
wald.test(b = fixef(model5), Sigma = vcov(model5), Terms = 13:17) # Year Identity p-value
summary(model5)

# Concordance
pred4 <- plogis(predict(model5, my_data))  # Predicted Scores
cord4 <- Concordance(my_data$hate_speech_ind, pred)

### Hazing
# Modelling is done using backwards elimination at alpha .1 significance level

## Model 1b
modelb <- glm(hazing_ind ~ gender_identity + sexual_orientation + race_ethnicity + year + first_generation_college_student_ind
             + greek_life_ind + athlete_ind + out_of_state_ind + transfer_student_ind, data = my_data, family = binomial(link = "logit"))
# VIF check
vif(modelb)
# Wald Tests For Fixed Effects
wald.test(b = coef(modelb), Sigma = vcov(modelb), Terms = 2:5) # Gender Identity p-value
wald.test(b = coef(modelb), Sigma = vcov(modelb), Terms = 6:12) # Sexual Orientation p-value
wald.test(b = coef(modelb), Sigma = vcov(modelb), Terms = 13:18) # Race/Ethnicity p-value
wald.test(b = coef(modelb), Sigma = vcov(modelb), Terms = 19:23) # Year p-value
summary(modelb)

# Concordance
predb <- plogis(predict(modelb, my_data, type="response"))  # Predicted Scores
cordb <- Concordance(my_data$hate_speech_ind, pred)


## Model 2b - Removed Gender Identity
model2b <- glm(hazing_ind ~ sexual_orientation + race_ethnicity + year + first_generation_college_student_ind
              + greek_life_ind + athlete_ind + out_of_state_ind + transfer_student_ind, data = my_data, family = binomial(link = "logit"))
# VIF check
vif(model2b)
# Wald Tests For Fixed Effects
wald.test(b = coef(model2b), Sigma = vcov(model2b), Terms = 2:8) # Sexual Orientation p-value
wald.test(b = coef(model2b), Sigma = vcov(model2b), Terms = 9:14) # Race/Ethnicity p-value
wald.test(b = coef(model2b), Sigma = vcov(model2b), Terms = 15:19) # Year p-value
summary(model2b)



## Model 3b - Removed Sexual Orientation
model3b <- glm(hazing_ind ~ race_ethnicity + year + first_generation_college_student_ind
               + greek_life_ind + athlete_ind + out_of_state_ind + transfer_student_ind, data = my_data, family = binomial(link = "logit"))
# VIF check
vif(model3b)
# Wald Tests For Fixed Effects
wald.test(b = coef(model3b), Sigma = vcov(model3b), Terms = 2:7) # Race/Ethnicity p-value
wald.test(b = coef(model3b), Sigma = vcov(model3b), Terms = 8:12) # Year p-value
summary(model3b)


## Model 4b - Removed Transfer Student
model4b <- glm(hazing_ind ~ race_ethnicity + year + first_generation_college_student_ind
               + greek_life_ind + athlete_ind + out_of_state_ind, data = my_data, family = binomial(link = "logit"))
# VIF check
vif(model4b)
# Wald Tests For Fixed Effects
wald.test(b = coef(model4b), Sigma = vcov(model4b), Terms = 2:7) # Race/Ethnicity p-value
wald.test(b = coef(model4b), Sigma = vcov(model4b), Terms = 8:12) # Year p-value
summary(model4b)


## Model 5b - Removed Out Of State
model5b <- glm(hazing_ind ~ race_ethnicity + year + first_generation_college_student_ind
               + greek_life_ind + athlete_ind, data = my_data, family = binomial(link = "logit"))
# VIF check
vif(model5b)
# Wald Tests For Fixed Effects
wald.test(b = coef(model5b), Sigma = vcov(model5b), Terms = 2:7) # Race/Ethnicity p-value
wald.test(b = coef(model5b), Sigma = vcov(model5b), Terms = 8:12) # Year p-value
summary(model5b)


## Model 6b - Removed Year
model6b <- glm(hazing_ind ~ race_ethnicity + first_generation_college_student_ind
               + greek_life_ind + athlete_ind, data = my_data, family = binomial(link = "logit"))
# VIF check
vif(model6b)
# Wald Tests For Fixed Effects
wald.test(b = coef(model6b), Sigma = vcov(model6b), Terms = 2:7) # Race/Ethnicity p-value
summary(model6b)


## Model 7b - Removed Race/Ethnicity
model7b <- glm(hazing_ind ~ first_generation_college_student_ind
               + greek_life_ind + athlete_ind, data = my_data, family = binomial(link = "logit"))
# VIF check
vif(model7b)
# Wald Tests For Fixed Effects
summary(model7b)

# Concordance
pred7b <- plogis(predict(model7b, my_data, type="response"))  # Predicted Scores
cord7b <- Concordance(my_data$hate_speech_ind, pred)


## Model 8b - Add Indicator For First Year
model8b <- glm(hazing_ind ~ first_yr_ind + first_generation_college_student_ind
               + greek_life_ind + athlete_ind, data = my_data, family = binomial(link = "logit"))
# VIF check
vif(model8b)
# Wald Tests For Fixed Effects
summary(model8b)

# Concordance
pred8b <- plogis(predict(model8b, my_data, type="response"))  # Predicted Scores
cord8b <- Concordance(my_data$hate_speech_ind, pred)

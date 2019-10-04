# Packages Required
library(readxl)
library(ggplot2)
library(tidyr)
library(aod) # logistic regression

### Dataset Manipulation
# Import Dataset
data <- read_excel("Stat465_C1/STAT 465 Fall 2019 P1 Benchmark Aggregate File PROPRIETARY AND CONFIDENTIAL.xlsx")

# Rename/Drop Columns For Convience
names(data) <- c("id", "school", "gender_identity", "sexual_orientation", "sexual_orientation_other", "race_ethnicity", 
                 "race_ethnicity_other", "year", "first_generation_college_student", "greek_life", "athlete",
                 "out_of_state", "transfer_student", "empty", "sexual_harassment", "alcohol", "hate_speech", "hazing")
data <- data[, !names(data) %in% c("empty", "sexual_harassment", "alcohol")] # drop empty collumn


# Remove Observations With Other Or Decline To State Responses
data <- data[which(!data$gender_identity %in% c("Decline to state", "Another Identity (please specify):")),]
data <- data[which(!data$sexual_orientation %in% c("Decline to state", "Other (please specify)")),]
data <- data[which(!data$race_ethnicity %in% c("Decline to state", "Other")),]
sum(is.na(data$first_generation_college_student))

## Recoding Values
# Recode Indicators
data$first_generation_college_student_ind <- replace_na(replace_na(as.numeric(recode(data$first_generation_college_student, 
                                                "First-Generation College Student" = 1)), 0))
data$greek_life_ind <- replace_na(as.numeric(recode(data$greek_life, "Greek Affiliated" = 1)), 0)
data$athlete_ind <- replace_na(as.numeric(recode(data$athlete, "Intercollegiate Student-Athlete" = 1)), 0)
data$out_of_state_ind <- replace_na(as.numeric(recode(data$out_of_state, "Out of State Student" = 1)), 0)
data$transfer_student_ind <- replace_na(as.numeric(recode(data$transfer_student, "Transfer Student" = 1)), 0)

# Recode Responses
data$hate_speech_ind <- replace_na(as.numeric(ifelse(data$hate_speech == "Never", 0, 1)), 0)
data$hazing_ind <- replace_na(as.numeric(ifelse(data$hazing == "Never", 0, 1)), 0)

# Recode Indicator NA's As 0


### Logistic Regression
# Modelling Dataset
my_data <- data[c("id", "school", "gender_identity", "sexual_orientation", "race_ethnicity", "year", "first_generation_college_student_ind",
                  "greek_life_ind", "athlete_ind", "out_of_state_ind", "transfer_student_ind", "hate_speech_ind", "hazing_ind")]

## Hate Speech
model <- glm(hate_speech_ind ~ gender_identity + sexual_orientation + race_ethnicity + year + first_generation_college_student_ind
             + greek_life_ind + athlete_ind + out_of_state_ind + transfer_student_ind, data = my_data, family = "binomial")


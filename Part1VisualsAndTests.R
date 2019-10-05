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
data <- read_excel("C:/Users/Colin.000/Desktop/STAT465/Project1/STAT 465 Fall 2019 P1 Benchmark Aggregate File PROPRIETARY AND CONFIDENTIAL.xlsx")

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


###Visualizations###

options(scipen=999)  # turn off scientific notation like 1e+06
library(ggplot2)


intervention_means = aggregate(my_data[, 12:13], list(my_data$school), mean)

colnames(intervention_means) = c("school", "hazing_prop", "hate_prop")

intervention_means$school <- as.factor(intervention_means$school)


#seperating dataframe by school (Didn't need to do this but left it in case we want it for later)
schools = sort(unique(my_data$school))

berkeley_data = my_data[which(my_data$school ==schools[1]),]
slo_data = my_data[which(my_data$school ==schools[2]),]
pomona_data = my_data[which(my_data$school ==schools[3]),]
csuf_data = my_data[which(my_data$school ==schools[4]),]
fresno_data = my_data[which(my_data$school ==schools[5]),]
maritime_data = my_data[which(my_data$school ==schools[6]),]
sac_data = my_data[which(my_data$school ==schools[7]),]
sj_data = my_data[which(my_data$school ==schools[8]),]




#create empty plots for each response
g_hate = ggplot(intervention_means, aes(x = school, y = hate_prop, fill = school))
g_haze = ggplot(intervention_means, aes(x = school, y = hazing_prop, fill = school))
       

#creating labeled bar chart plots for each response
g_hate + geom_bar(stat = "identity") + labs(title = "Bar Chart",
                                            subtitle = "Proportion of Students who Intervened in Hateful Speech/Racism Incidents, Grouped by University",
                                            caption = "Source: 2019 National College Student Bystander Benchmark Survey",
                                            x = "University",
                                            y = "Intervention Rate") + scale_colour_brewer() + ylim(0,1) +
                                            geom_text(aes(label=round(hate_prop, digits = 2)), vjust=-0.3, size=3.5) + guides(fill=guide_legend(title="University"))


g_haze + geom_bar(stat = "identity") + labs(title = "Bar Chart",
                                            subtitle = "Proportion of Students who Intervened in Hazing Incidents, Grouped by University",
                                            caption = "Source: 2019 National College Student Bystander Benchmark Survey",
                                            x = "University",
                                            y = "Intervention Rate")+ ylim(0,1) +
  geom_text(aes(label=round(hazing_prop, digits = 2)), vjust=-0.3, size=3.5) + guides(fill=guide_legend(title="University"))

       

#chi-sq test for independence (hateful speech)
tblHate = table(my_data$school, my_data$hate_speech_ind)
cHate = chisq.test(tblHate)
cHate

#Chisq residuals
round(cHate$residuals, 3)

#chi-sq test for independence (hazing)
tblHaze = table(my_data$school, my_data$hazing_ind)
cHaze = chisq.test(tblHaze)
cHaze

#chisq residuals
round(cHaze$residuals, 3)


      
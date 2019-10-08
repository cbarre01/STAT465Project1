# Packages Required
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(aod) # Logistic Regression
library(car) # VIF
library(InformationValue) # Concordance Rates and ROC
library(lme4) # Random Slopes
library(RColorBrewer)

### Dataset Manipulation
# Import Dataset
data = STAT_465_Fall_2019_P1_Benchmark_Aggregate_File_PROPRIETARY_AND_CONFIDENTIAL
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





SE = function(n, p)
{
  return(sqrt(p * (1-p)/n))
}

#Creating table with proportions for plotting
school_means = aggregate(my_data[, 13:12], list(my_data$school), mean)

colnames(school_means) = c("school", "hazing_prop", "hate_prop")

school_means$school <- as.factor(school_means$school)
school_means$hazingSE = -1
school_means$hateSE = -1

for (i in c(1:length(school_means[,1])))
{
  school_means$hazingSE[i] = SE(sum(my_data$school == school_means[i,1]), school_means[i,2])
  school_means$hateSE[i] = SE(sum(my_data$school == school_means[i,1]), school_means[i,3])
}



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


colors = brewer.pal(8,"Blues")

#create empty plots for each response
g_hate = ggplot(school_means, aes(x = school, y = hate_prop, fill = school))
g_haze = ggplot(school_means, aes(x = school, y = hazing_prop, fill = school))


#creating labeled bar chart plots for each response
g_hate + geom_bar(stat = "identity") + labs(title = "Bar Chart",
                                            subtitle = "Proportion of Students who Intervened in Hateful Speech/Racism Incidents, Grouped by University",
                                            caption = "Source: 2019 National College Student Bystander Benchmark Survey",
                                            x = "University",
                                            y = "Intervention Rate") + scale_colour_brewer() + ylim(0,1)  + scale_fill_brewer(palette="Blues") + 
  geom_text(aes(label=round(hate_prop, digits = 2)), vjust=-0.3, size=3.5) + guides(fill=guide_legend(title="University")) +
  geom_errorbar(aes(ymin=hate_prop-hateSE, ymax=hate_prop+hateSE), width=.2,
                position=position_dodge(.9)) 


g_haze + geom_bar(stat = "identity") + labs(title = "Bar Chart",
                                            subtitle = "Proportion of Students who Intervened in Hazing Incidents, Grouped by University",
                                            caption = "Source: 2019 National College Student Bystander Benchmark Survey",
                                            x = "University",
                                            y = "Intervention Rate")+ ylim(0,1) +
  geom_text(aes(label=round(hazing_prop, digits = 2)), vjust=-0.3, size=3.5) + guides(fill=guide_legend(title="University"))  + scale_fill_brewer(palette="Blues") + 
  geom_errorbar(aes(ymin=hazing_prop-hazingSE, ymax=hazing_prop+hazingSE), width=.2,
                position=position_dodge(.9)) 



#chi-sq test for independence (hateful speech)
tblHate = table(my_data$school, my_data$hate_speech_ind)
chiHate = chisq.test(tblHate)
chiHate

#Chisq residuals
round(chiHate$residuals, 3)

#chi-sq test for independence (hazing)
tblHaze = table(my_data$school, my_data$hazing_ind)
chiHaze = chisq.test(tblHaze)
chiHaze

#chisq residuals
round(chiHaze$residuals, 3)

#Regression models
modelHate = glm(hate_speech_ind ~ school, data = my_data, family = binomial(link = "logit"))
summary(modelHate)

modelHaze = glm(hazing_ind ~ school, data = my_data, family = binomial(link = "logit"))
summary(modelHaze)

###QUestion 2### (Take 2)


#Creating tables with proportions and SE's for Demographics

#Firstyr
firstyr_means = aggregate(my_data[, 13:12], list(my_data$first_yr_ind), mean)
firstyr_means$hazingSE = -1
firstyr_means$hateSE = -1
firstyr_means$Group.1 <- as.factor(firstyr_means$Group.1)

for (i in c(1:length(firstyr_means[,1])))
{
  firstyr_means$hazingSE[i] = SE(sum(my_data$first_yr_ind == firstyr_means[i,1],na.rm = TRUE), firstyr_means[i,2])
  firstyr_means$hateSE[i] = SE(sum(my_data$first_yr_ind == firstyr_means[i,1],na.rm = TRUE), firstyr_means[i,3])
}
colnames(firstyr_means)[1] = "firstYr"
#GenderID

genderID_means = aggregate(my_data[, 13:12], list(my_data$gender_identity), mean)
genderID_means$hazingSE = -1
genderID_means$hateSE = -1
genderID_means$Group.1 <- as.factor(genderID_means$Group.1)

for (i in c(1:length(genderID_means[,1])))
{
  genderID_means$hazingSE[i] = SE(sum(my_data$gender_identity == genderID_means[i,1],na.rm = TRUE), genderID_means[i,2])
  genderID_means$hateSE[i] = SE(sum(my_data$gender_identity == genderID_means[i,1],na.rm = TRUE), genderID_means[i,3])
}
colnames(genderID_means)[1] = "gender"
#Greek Life

greek_means = aggregate(my_data[, 13:12], list(my_data$greek_life_ind), mean)
greek_means$hazingSE = -1
greek_means$hateSE = -1
greek_means$Group.1 <- as.factor(greek_means$Group.1)

for (i in c(1:length(greek_means[,1])))
{
  greek_means$hazingSE[i] = SE(sum(my_data$greek_life_ind == greek_means[i,1],na.rm = TRUE), greek_means[i,2])
  greek_means$hateSE[i] = SE(sum(my_data$greek_life_ind == greek_means[i,1],na.rm = TRUE), greek_means[i,3])
}
colnames(greek_means)[1] = "greek"

#FirstGen

FirstGen_means = aggregate(my_data[, 13:12], list(my_data$first_generation_college_student_ind), mean)
FirstGen_means$hazingSE = -1
FirstGen_means$hateSE = -1
FirstGen_means$Group.1 <- as.factor(FirstGen_means$Group.1)

for (i in c(1:length(FirstGen_means[,1])))
{
  FirstGen_means$hazingSE[i] = SE(sum(my_data$first_generation_college_student_ind == FirstGen_means[i,1],na.rm = TRUE), FirstGen_means[i,2])
  FirstGen_means$hateSE[i] = SE(sum(my_data$first_generation_college_student_ind == FirstGen_means[i,1],na.rm = TRUE), FirstGen_means[i,3])
}

colnames(FirstGen_means)[1] = "firstGen"

#Athlete

athlete_means = aggregate(my_data[, 13:12], list(my_data$athlete_ind), mean)
athlete_means$hazingSE = -1
athlete_means$hateSE = -1
athlete_means$Group.1 <- as.factor(athlete_means$Group.1)

for (i in c(1:length(athlete_means[,1])))
{
  athlete_means$hazingSE[i] = SE(sum(my_data$athlete_ind == athlete_means[i,1],na.rm = TRUE), athlete_means[i,2])
  athlete_means$hateSE[i] = SE(sum(my_data$athlete_ind == athlete_means[i,1],na.rm = TRUE), athlete_means[i,3])
}

colnames(athlete_means)[1] = "athlete"

#SexOri

sexual_ori_means = aggregate(my_data[, 13:12], list(my_data$sexual_orientation), mean)
sexual_ori_means$hazingSE = -1
sexual_ori_means$hateSE = -1
sexual_ori_means$Group.1 <- as.factor(sexual_ori_means$Group.1)

for (i in c(1:length(sexual_ori_means[,1])))
{
  sexual_ori_means$hazingSE[i] = SE(sum(my_data$sexual_orientation == sexual_ori_means[i,1],na.rm = TRUE), sexual_ori_means[i,2])
  sexual_ori_means$hateSE[i] = SE(sum(my_data$sexual_orientation == sexual_ori_means[i,1],na.rm = TRUE), sexual_ori_means[i,3])
}
colnames(sexual_ori_means)[1] = "sexOri"

##Creating Graphs##

#Hateful speech vs First Yr Student
g_hate_first_yr = ggplot(firstyr_means, aes(x = firstYr, y = hate_speech_ind, fill = firstYr))

g_hate_first_yr + geom_bar(stat = "identity") + labs(title = "Bar Chart",
                                                     subtitle = "Proportion of Students who Intervened in Hateful Speech/Racism Incidents\n by First Year Student",
                                                     caption = "Source: 2019 National College Student Bystander Benchmark Survey",
                                                     x = "First Year Student",
                                                     y = "Intervention Rate") + scale_colour_brewer() + ylim(0,1) +
  geom_text(aes(label=round(hate_speech_ind, digits = 3)), vjust=-0.3, size=3.5) + theme(legend.position = "none")+ guides(fill=guide_legend(title="University"))  + scale_fill_brewer(palette="Blues") + 
  geom_errorbar(aes(ymin=hate_speech_ind-hateSE, ymax=hate_speech_ind+hateSE), width=.2,
                position=position_dodge(.9)) + coord_cartesian(ylim=c(0, 0.5)) + scale_x_discrete(labels = c("Second Year or Beyond", "First Year"))

#Hateful speech vs Gender ID
g_hate_gender = ggplot(genderID_means, aes(x = gender, y = hate_speech_ind, fill = gender))

g_hate_gender + geom_bar(stat = "identity") + labs(title = "Bar Chart",
                                                   subtitle = "Proportion of Students who Intervened in Hateful Speech/Racism Incidents \nGrouped by Gender Identity",
                                                   caption = "Source: 2019 National College Student Bystander Benchmark Survey",
                                                   x = "Gender Identity",
                                                   y = "Intervention Rate") + scale_colour_brewer() + ylim(0,1) +
  geom_text(aes(label=round(hate_speech_ind, digits = 3)), vjust=-0.3, size=3.5) + theme(legend.position = "none")+ guides(fill=guide_legend(title="University"))  + scale_fill_brewer(palette="Blues") + 
  geom_errorbar(aes(ymin=hate_speech_ind-hateSE, ymax=hate_speech_ind+hateSE), width=.2,
                position=position_dodge(.9))+ scale_x_discrete(labels = c("Female", "Gender Non-conforming", "Male", "Trans female", "Trans male"))

#Hateful speech vs Greek System Participation
g_hate_greek = ggplot(greek_means, aes(x = greek, y = hate_speech_ind, fill = greek))

g_hate_greek + geom_bar(stat = "identity") + labs(title = "Bar Chart",
                                                  subtitle = "Proportion of Students who Intervened in Hateful Speech/Racism Incidents\nGrouped by Greek Affiliation",
                                                  caption = "Source: 2019 National College Student Bystander Benchmark Survey",
                                                  x = "Active in Greek Life",
                                                  y = "Intervention Rate") + scale_colour_brewer() + ylim(0,1) +
  geom_text(aes(label=round(hate_speech_ind, digits = 3)), vjust=-0.3, size=3.5) + guides(fill=guide_legend(title="University"))  + scale_fill_brewer(palette="Blues") + 
  geom_errorbar(aes(ymin=hate_speech_ind-hateSE, ymax=hate_speech_ind+hateSE), width=.2,
                position=position_dodge(.9)) + coord_cartesian(ylim=c(0, 0.5)) + theme(legend.position = "none")+ scale_x_discrete(labels = c("Non Participant", "Greek Life Participant"))




#Hazing vs First Year Student
g_haze_first_yr = ggplot(firstyr_means, aes(x = firstYr, y = hazing_ind, fill = firstYr))

g_haze_first_yr + geom_bar(stat = "identity") + labs(title = "Bar Chart",
                                                     subtitle = "Proportion of Students who Intervened in Hazing Incidents\nby First Year Student",
                                                     caption = "Source: 2019 National College Student Bystander Benchmark Survey",
                                                     x = "First Year Student",
                                                     y = "Intervention Rate") + theme(legend.position = "none")+ scale_colour_brewer() + ylim(0,1) +
  geom_text(aes(label=round(hazing_ind, digits = 3)), vjust=-0.3, size=3.5) + guides(fill=guide_legend(title="University"))  + scale_fill_brewer(palette="Blues") + 
  geom_errorbar(aes(ymin=hazing_ind-hazingSE, ymax=hazing_ind+hazingSE), width=.2,
                position=position_dodge(.9)) + coord_cartesian(ylim=c(0, 0.5))+ scale_x_discrete(labels = c("Not First Year", "First Year"))

#Hazing vs Greek Life Participant
g_haze_greek = ggplot(greek_means, aes(x = greek, y = hazing_ind, fill = greek))

g_haze_greek + geom_bar(stat = "identity") + labs(title = "Bar Chart",
                                                  subtitle = "Proportion of Students who Intervened in Hazing Incidents\nGrouped by Greek Life Participation",
                                                  caption = "Source: 2019 National College Student Bystander Benchmark Survey",
                                                  x = "Greek Life Participant",
                                                  y = "Intervention Rate") + theme(legend.position = "none")+ scale_colour_brewer() + ylim(0,1)  + scale_fill_brewer(palette="Blues") + 
  geom_text(aes(label=round(hazing_ind, digits = 3)), vjust=-0.3, size=3.5) + guides(fill=guide_legend(title="University")) +
  geom_errorbar(aes(ymin=hazing_ind-hazingSE, ymax=hazing_ind+hazingSE), width=.2,
                position=position_dodge(.9)) + coord_cartesian(ylim=c(0, 0.5))+ scale_x_discrete(labels = c("Non Participant", "Greek Life Participant"))

#Hazing vs Athlete
g_haze_athlete = ggplot(athlete_means, aes(x = athlete, y = hazing_ind, fill = athlete))

g_haze_athlete + geom_bar(stat = "identity") + labs(title = "Bar Chart",
                                                    subtitle = "Proportion of Students who Intervened in Hazing Incidents\nGrouped by Athletic Participation",
                                                    caption = "Source: 2019 National College Student Bystander Benchmark Survey",
                                                    x = "Athlete",
                                                    y = "Intervention Rate") + theme(legend.position = "none")+ scale_colour_brewer() + ylim(0,1)  + scale_fill_brewer(palette="Blues") + 
  geom_text(aes(label=round(hazing_ind, digits = 3)), vjust=-0.3, size=3.5) + guides(fill=guide_legend(title="University")) +
  geom_errorbar(aes(ymin=hazing_ind-hazingSE, ymax=hazing_ind+hazingSE), width=.2,
                position=position_dodge(.9)) + coord_cartesian(ylim=c(0, 0.5))+ scale_x_discrete(labels = c("Non Athlete", "Athlete"))

#Hazing vs Sexual Orientation
g_haze_sexual_orientation = ggplot(sexual_ori_means, aes(x = sexOri, y = hazing_ind, fill = sexOri))

g_haze_sexual_orientation + geom_bar(stat = "identity") + labs(title = "Bar Chart",
                                                               subtitle = "Proportion of Students who Intervened in Hazing Incidents\nGrouped by Sexual Orientation",
                                                               caption = "Source: 2019 National College Student Bystander Benchmark Survey",
                                                               x = "First Year Student",
                                                               y = "Intervention Rate") + scale_colour_brewer() + theme(legend.position = "none")+ ylim(0,1)  + scale_fill_brewer(palette="Blues") + 
  geom_text(aes(label=round(hazing_ind, digits = 3)), vjust=-0.3, size=3.5) + guides(fill=guide_legend(title="University")) +
  geom_errorbar(aes(ymin=hazing_ind-hazingSE, ymax=hazing_ind+hazingSE), width=.2,
                position=position_dodge(.9)) + coord_cartesian(ylim=c(0, 0.5))




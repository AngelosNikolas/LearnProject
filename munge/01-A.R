#Enrollments run 1 pre-processing.
library(dplyr)
library(tidyverse)

#Checking and remove NAs
sum(is.na(cyber.security.1_enrolments))
cyber.security.1_enrolments= cyber.security.1_enrolments[rowSums(is.na(cyber.security.1_enrolments)) == 0,]
sum(is.na(cyber.security.1_enrolments))

# Remove rows with only NAs
cyber.security.1_enrolments[rowSums(is.na(cyber.security.1_enrolments)) != ncol(cyber.security.1_enrolments), ] 

#Checking for duplicates
cyber.security.1_enrolments$learner_id[duplicated(cyber.security.1_enrolments$learner_id)]

#############DATA CONSTRUCTION##############
Fully_finished1 = cyber.security.1_enrolments [!(!is.na(cyber.security.1_enrolments$fully_participated_at) & cyber.security.1_enrolments$fully_participated_at==""), ]
Fully_finished2 = cyber.security.2_enrolments [!(!is.na(cyber.security.2_enrolments$fully_participated_at) & cyber.security.2_enrolments$fully_participated_at==""), ]
Fully_finished3 = cyber.security.3_enrolments [!(!is.na(cyber.security.3_enrolments$fully_participated_at) & cyber.security.3_enrolments$fully_participated_at==""), ]
Fully_finished4 = cyber.security.4_enrolments [!(!is.na(cyber.security.4_enrolments$fully_participated_at) & cyber.security.4_enrolments$fully_participated_at==""), ]
Fully_finished5 = cyber.security.5_enrolments [!(!is.na(cyber.security.5_enrolments$fully_participated_at) & cyber.security.5_enrolments$fully_participated_at==""), ]
Fully_finished6 = cyber.security.6_enrolments [!(!is.na(cyber.security.6_enrolments$fully_participated_at) & cyber.security.6_enrolments$fully_participated_at==""), ]
Fully_finished7 = cyber.security.7_enrolments [!(!is.na(cyber.security.7_enrolments$fully_participated_at) & cyber.security.7_enrolments$fully_participated_at==""), ]


dim(cyber.security.7_enrolments)
fully_participated1 = (1803*100)/14840 
fully_participated2 = (33*100)/6488
fully_participated3 = (56*100)/3361
fully_participated4 = (166*100)/3992
fully_participated5 = (22*100)/3544
fully_participated6 = (31*100)/3175
fully_participated7 = (43*100)/2342

#Allocating genders for fully participated learners
MaleLearners1 = filter(Fully_finished1, gender == "male")
FemaleLearners1 = filter(Fully_finished1, gender == "female")
UnknownGenderLearners1= filter(Fully_finished1, gender == "Unknown")

#Allocating education levels
univercityDegree = filter(cyber.security.1_enrolments, highest_education_level == "university_degree")
univercitydoctorate  = filter(cyber.security.1_enrolments, highest_education_level == "university_doctorate")
professional  = filter(cyber.security.1_enrolments, highest_education_level == "professional")
UnknownEducation = filter(cyber.security.1_enrolments, highest_education_level == "Unknown")   

#Allocating employment status
distinct(cyber.security.1_enrolments, employment_status)
UnknownEmployment = filter(cyber.security.1_enrolments, employment_status == "Unknown")   

########################################################################################################################
#Question.response data set prepossessing 

#Remove empty column
cyber.security.1_question.response = select(cyber.security.1_question.response, -c(cloze_response))

# Check for NAs 
sum(is.na(cyber.security.1_question.response))

#############DATA CONSTRUCTION##############
str(cyber.security.1_question.response)


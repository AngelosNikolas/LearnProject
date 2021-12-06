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
# Creation of new set containing learner that fully finished the course.
Fully_finished1 = cyber.security.1_enrolments [!(!is.na(cyber.security.1_enrolments$fully_participated_at) & cyber.security.1_enrolments$fully_participated_at==""), ]


#Calculating the percentage of fully participation.
fully_participated1 = (1803*100)/14840 


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
unique(cyber.security.1_question.response$quiz_question)
str(cyber.security.1_question.response$quiz_question)

# Extracting the correct answers regarding the 1st week
Week1Correct = filter(cyber.security.1_question.response, step_number == 7)
Week1Correct =  filter(cyber.security.1_question.response, correct == "true" )

# Extracting the correct answers regarding the 2st week
Week2Correct = filter(cyber.security.1_question.response, step_number == 8)
Week2Correct =  filter(cyber.security.1_question.response, correct == "true" )

# Extracting the correct answers regarding the 3st week
Week3Correct = filter(cyber.security.1_question.response, step_number == 11)
Week3Correct =  filter(cyber.security.1_question.response, correct == "true" )

######################################################################
#Step activity pre-processing

#Changed the step numbers
cyber.security.1_step.activity$step = cyber.security.1_step.activity$week_number*100 + cyber.security.1_step.activity$step_number

#Checking for NAs
sum(is.na(cyber.security.1_step.activity$learner_id))


#####################DATA CONSTRUCTION######################################


#Filter by week 1
Week1Steps1 = filter(cyber.security.1_step.activity, week_number == 1)

#Extract the number of how many didnt completed the steps
filter(Week1Steps1 , last_completed_at == "")
#Extract the count of observations that completed step 1
filter(Week1Steps1, step_number == 1 , !last_completed_at == "" )
filter(Week1Steps1, step_number == 1 , last_completed_at == "" )

#Filter by week 2
Week2Steps1 = filter(cyber.security.1_step.activity, week_number == 2)
#Extract the number of how many did not completed the steps
filter(Week2Steps1 , last_completed_at == "")

#Filter by week 3
Week3Steps1 = filter(cyber.security.1_step.activity, week_number == 3)
#Extract the number of how many didnt completed the steps
filter(Week3Steps1 , last_completed_at == "")





    
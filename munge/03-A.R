# Run 3
# Pre-processing script
#Enrollments run pre-processing.
library(dplyr)
library(tidyverse)

#Checking and remove NAs
sum(is.na(cyber.security.3_enrolments))
cyber.security.3_enrolments= cyber.security.3_enrolments[rowSums(is.na(cyber.security.3_enrolments)) == 0,]
sum(is.na(cyber.security.3_enrolments))

# Remove rows with only NAs
cyber.security.3_enrolments[rowSums(is.na(cyber.security.3_enrolments)) != ncol(cyber.security.3_enrolments), ] 

#Checking for duplicates
cyber.security.3_enrolments$learner_id[duplicated(cyber.security.3_enrolments$learner_id)]

#############DATA CONSTRUCTION##############
# Creation of new set containing learner that fully finished the course.

Fully_finished3 = cyber.security.3_enrolments [!(!is.na(cyber.security.3_enrolments$fully_participated_at) & cyber.security.3_enrolments$fully_participated_at==""), ]

#Calculating the percentage of fully participation.

fully_participated3 = (56*100)/3361


#Allocating genders for fully participated learners
MaleLearners3 = filter(Fully_finished3, gender == "male")
FemaleLearners3 = filter(Fully_finished3, gender == "female")
UnknownGenderLearners3= filter(Fully_finished3, gender == "Unknown")

#Allocating education levels
univercityDegree3 = filter(cyber.security.3_enrolments, highest_education_level == "university_degree")
univercitydoctorate3  = filter(cyber.security.3_enrolments, highest_education_level == "university_doctorate")
professional3  = filter(cyber.security.3_enrolments, highest_education_level == "professional")
UnknownEducation3 = filter(cyber.security.3_enrolments, highest_education_level == "Unknown")   

#Allocating employment status
distinct(cyber.security.3_enrolments, employment_status)
UnknownEmployment4 = filter(cyber.security.3_enrolments, employment_status == "Unknown")   

########################################################################################################################
#Question.response data set prepossessing 

#Remove empty column
cyber.security.3_question.response = select(cyber.security.3_question.response, -c(cloze_response))

# Check for NAs 
sum(is.na(cyber.security.3_question.response))

#############DATA CONSTRUCTION##############
str(cyber.security.3_question.response)
unique(cyber.security.3_question.response$quiz_question)
str(cyber.security.3_question.response$quiz_question)

# Extracting the correct answers regarding the 1st week
Week1Correct3 = filter(cyber.security.3_question.response, step_number == 7)
Week1Correct3 =  filter(cyber.security.3_question.response, correct == "true" )

# Extracting the correct answers regarding the 2st week
Week2Correct3 = filter(cyber.security.3_question.response, step_number == 8)
Week2Correct3 =  filter(cyber.security.3_question.response, correct == "true" )

# Extracting the correct answers regarding the 3st week
Week3Correct3 = filter(cyber.security.3_question.response, step_number == 11)
Week3Correct3 =  filter(cyber.security.3_question.response, correct == "true" )

######################################################################
#Step activity pre-processing

#Changed the step numbers
cyber.security.3_step.activity$step = cyber.security.3_step.activity$week_number*100 + cyber.security.3_step.activity$step_number

#Checking for NAs
sum(is.na(cyber.security.3_step.activity$learner_id))


#####################DATA CONSTRUCTION######################################


#Filter by week 1
Week1Steps3 = filter(cyber.security.3_step.activity, week_number == 1)

#Extract the number of how many didnt completed the steps
filter(Week1Steps3 , last_completed_at == "")
#Extract the count of observations that completed step 1
filter(Week1Steps3, step_number == 1 , !last_completed_at == "" )
filter(Week1Steps3, step_number == 1 , last_completed_at == "" )

#Filter by week 2
Week2Steps3 = filter(cyber.security.3_step.activity, week_number == 2)
#Extract the number of how many did not completed the steps
filter(Week2Steps3 , last_completed_at == "")

#Filter by week 3
Week3Steps3 = filter(cyber.security.3_step.activity, week_number == 3)
#Extract the number of how many didnt completed the steps
filter(Week3Steps3 , last_completed_at == "")







##################################################################
#Video stats pre-processing 
sum(is.na(cyber.security.3_video.stats))

############## DATA CONSTRUCTION##################################3
video_views1 = select(cyber.security.3_video.stats, c(1:15))
Video_devices1 = select(cyber.security.3_video.stats, c(16:21))
video_location1 = select(cyber.security.3_video.stats, c(22:28))

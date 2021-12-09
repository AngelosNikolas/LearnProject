#Run 5
#Pre-processing script 


#Enrollments run pre-processing.
library(dplyr)
library(tidyverse)

#Checking and remove NAs
sum(is.na(cyber.security.5_enrolments))
cyber.security.5_enrolments= cyber.security.5_enrolments[rowSums(is.na(cyber.security.5_enrolments)) == 0,]
sum(is.na(cyber.security.5_enrolments))

# Remove rows with only NAs
cyber.security.5_enrolments[rowSums(is.na(cyber.security.5_enrolments)) != ncol(cyber.security.5_enrolments), ] 

#Checking for duplicates
cyber.security.5_enrolments$learner_id[duplicated(cyber.security.5_enrolments$learner_id)]

#############DATA CONSTRUCTION##############
# Creation of new set containing learner that fully finished the course.
Fully_finished5 = cyber.security.5_enrolments [!(!is.na(cyber.security.5_enrolments$fully_participated_at) & cyber.security.5_enrolments$fully_participated_at==""), ]


#Calculating the percentage of fully participation.
fully_participated5 = (22*100)/3544

#Allocating genders for fully participated learners
MaleLearners5 = filter(Fully_finished5, gender == "male")
FemaleLearners5 = filter(Fully_finished5, gender == "female")
UnknownGenderLearners5= filter(Fully_finished5, gender == "Unknown")

#Allocating education levels
univercityDegree5 = filter(cyber.security.5_enrolments, highest_education_level == "university_degree")
univercitydoctorate5  = filter(cyber.security.5_enrolments, highest_education_level == "university_doctorate")
professional5  = filter(cyber.security.5_enrolments, highest_education_level == "professional")
UnknownEducation5 = filter(cyber.security.5_enrolments, highest_education_level == "Unknown")   

#Allocating employment status
distinct(cyber.security.5_enrolments, employment_status)
UnknownEmployment5 = filter(cyber.security.5_enrolments, employment_status == "Unknown")   

########################################################################################################################

######################################################################
#Step activity pre-processing

#Changed the step numbers
cyber.security.5_step.activity$step = cyber.security.5_step.activity$week_number*100 + cyber.security.5_step.activity$step_number

#Checking for NAs
sum(is.na(cyber.security.5_step.activity$learner_id))


#####################DATA CONSTRUCTION######################################


#Filter by week 1
Week1Steps5 = filter(cyber.security.5_step.activity, week_number == 1)

#Extract the number of how many didnt completed the steps
filter(Week1Steps5 , last_completed_at == "")
#Extract the count of observations that completed step 1
filter(Week1Steps5, step_number == 1 , !last_completed_at == "" )
filter(Week1Steps5, step_number == 1 , last_completed_at == "" )

#Filter by week 2
Week2Steps5 = filter(cyber.security.5_step.activity, week_number == 2)
#Extract the number of how many did not completed the steps
filter(Week2Steps5 , last_completed_at == "")

#Filter by week 3
Week3Steps5 = filter(cyber.security.5_step.activity, week_number == 3)
#Extract the number of how many didnt completed the steps
filter(Week3Steps5 , last_completed_at == "")

##################################################################
#Video stats pre-processing 
sum(is.na(cyber.security.5_video.stats))

############## DATA CONSTRUCTION##################################3
video_views5 = select(cyber.security.5_video.stats, c(1:15))
Video_devices5 = select(cyber.security.5_video.stats, c(16:21))
video_location5 = select(cyber.security.5_video.stats, c(22:28))





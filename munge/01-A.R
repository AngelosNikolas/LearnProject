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
univercityDegree1 = filter(Fully_finished1, highest_education_level == "university_degree")
univercitydoctorate1  = filter(Fully_finished1, highest_education_level == "university_doctorate")
univercityMasters1 = filter(Fully_finished1, highest_education_level == "university_masters")
professional1  = filter(Fully_finished1, highest_education_level == "professional")
UnknownEducation1 = filter(Fully_finished1, highest_education_level == "Unknown")   

#Allocating employment status
unique(Fully_finished1$employment_status)
part_time1 = filter(Fully_finished1, employment_status == "working_part_time")
full_time1  = filter(Fully_finished1, employment_status == "working_full_time")
fulltime_Student1 = filter(Fully_finished1, employment_status == "full_time_student")
unemployed1  = filter(Fully_finished1, employment_status == "unemployed")
looking_for_work1  = filter(Fully_finished1, employment_status == "looking_for_work")
Unknown_Status1 = filter(Fully_finished1, employment_status == "Unknown")     

#Constructing a data frame with the enrollment counts for all the runs.
RunNumber = c('Run1','Run2','Run3','Run4','Run5','Run6','Run7')
EnrolCount = c(14840,6488,3361,3992,3544,3175,2342 )
Enroldata = data.frame(RunNumber,EnrolCount)


#Construction for the Employment status 
Employment_Count = c(nrow(part_time1),nrow(full_time1),nrow(fulltime_Student1),nrow(unemployed1),nrow(looking_for_work1),nrow(Unknown_Status1))
Employment_Levels = c('Part-time','Full_Time','Student','Unemployed','LFwork','Unknown')
Employment_data = data.frame(Employment_Levels,Employment_Count)

#Construction for the Education status
Education_Count = c(nrow(univercityDegree1),nrow(univercitydoctorate1),nrow(univercityMasters1),nrow(professional1),nrow(UnknownEducation1))
Education_Levels = c('Bachelors','PHD','Masters','Professional','Unknown')
Education_data = data.frame(Education_Levels,Education_Count)


########################################################################################################################
#Question.response data set prepossessing 

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





    
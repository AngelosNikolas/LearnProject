#Run 2 pre processing script 
#Enrollments run  pre-processing.

#Checking and remove NAs
sum(is.na(cyber.security.2_enrolments))
cyber.security.2_enrolments= cyber.security.2_enrolments[rowSums(is.na(cyber.security.2_enrolments)) == 0,]
sum(is.na(cyber.security.2_enrolments))

# Remove rows with only NAs
cyber.security.2_enrolments[rowSums(is.na(cyber.security.2_enrolments)) != ncol(cyber.security.2_enrolments), ] 

#Checking for duplicates
cyber.security.2_enrolments$learner_id[duplicated(cyber.security.2_enrolments$learner_id)]

#############DATA CONSTRUCTION##############
# Creation of new set containing learner that fully finished the course.
Fully_finished2 = cyber.security.2_enrolments [!(!is.na(cyber.security.2_enrolments$fully_participated_at) & cyber.security.2_enrolments$fully_participated_at==""), ]


#Calculating the percentage of fully participation.
fully_participated2 = (33*100)/6488


#Allocating genders for fully participated learners
MaleLearners2 = filter(Fully_finished1, gender == "male")
FemaleLearners2 = filter(Fully_finished1, gender == "female")
UnknownGenderLearners2= filter(Fully_finished1, gender == "Unknown")

#Allocating education levels
univercityDegree2 = filter(cyber.security.2_enrolments, highest_education_level == "university_degree")
univercitydoctorate2  = filter(cyber.security.2_enrolments, highest_education_level == "university_doctorate")
professional2  = filter(cyber.security.2_enrolments, highest_education_level == "professional")
UnknownEducation2 = filter(cyber.security.2_enrolments, highest_education_level == "Unknown")   

#Allocating employment status
distinct(cyber.security.2_enrolments, employment_status)
UnknownEmployment2 = filter(cyber.security.2_enrolments, employment_status == "Unknown")   

########################################################################################################################
#Question.response data set prepossessing 

#Remove empty column
cyber.security.2_question.response = select(cyber.security.2_question.response, -c(cloze_response))

# Check for NAs 
sum(is.na(cyber.security.2_question.response))

#############DATA CONSTRUCTION##############
str(cyber.security.2_question.response)
unique(cyber.security.2_question.response$quiz_question)
str(cyber.security.2_question.response$quiz_question)

# Extracting the correct answers regarding the 1st week
Week1Correct2 = filter(cyber.security.2_question.response, step_number == 7)
Week1Correc2 =  filter(cyber.security.2_question.response, correct == "true" )

# Extracting the correct answers regarding the 2st week
Week2Correct2= filter(cyber.security.2_question.response, step_number == 8)
Week2Correct2 =  filter(cyber.security.2_question.response, correct == "true" )

# Extracting the correct answers regarding the 3st week
Week3Correct2 = filter(cyber.security.2_question.response, step_number == 11)
Week3Correct2 =  filter(cyber.security.2_question.response, correct == "true" )

######################################################################
#Step activity pre-processing

#Changed the step numbers
cyber.security.2_step.activity$step = cyber.security.2_step.activity$week_number*100 + cyber.security.2_step.activity$step_number

#Checking for NAs
sum(is.na(cyber.security.2_step.activity$learner_id))


#####################DATA CONSTRUCTION######################################


#Filter by week 1
Week1Steps2 = filter(cyber.security.2_step.activity, week_number == 1)

#Extract the number of how many didnt completed the steps
filter(Week1Steps2 , last_completed_at == "")
#Extract the count of observations that completed step 1
filter(Week1Steps2, step_number == 1 , !last_completed_at == "" )
filter(Week1Steps2, step_number == 1 , last_completed_at == "" )

#Filter by week 2
Week2Steps2 = filter(cyber.security.2_step.activity, week_number == 2)
#Extract the number of how many did not completed the steps
filter(Week2Steps2 , last_completed_at == "")

#Filter by week 3
Week3Steps2 = filter(cyber.security.2_step.activity, week_number == 3)
#Extract the number of how many didnt completed the steps
filter(Week3Steps2 , last_completed_at == "")






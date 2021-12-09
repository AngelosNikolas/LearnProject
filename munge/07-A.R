#Run 7
#Pre-processing script 

#Enrollments run pre-processing.

#Checking and remove NAs
sum(is.na(cyber.security.7_enrolments))
cyber.security.7_enrolments= cyber.security.7_enrolments[rowSums(is.na(cyber.security.7_enrolments)) == 0,]
sum(is.na(cyber.security.7_enrolments))

# Remove rows with only NAs
cyber.security.7_enrolments[rowSums(is.na(cyber.security.7_enrolments)) != ncol(cyber.security.7_enrolments), ] 

#Checking for duplicates
cyber.security.7_enrolments$learner_id[duplicated(cyber.security.7_enrolments$learner_id)]

#############DATA CONSTRUCTION##############
# Creation of new set containing learner that fully finished the course.

Fully_finished7 = cyber.security.7_enrolments [!(!is.na(cyber.security.7_enrolments$fully_participated_at) & cyber.security.7_enrolments$fully_participated_at==""), ]

#Calculating the percentage of fully participation.

fully_participated7 = (43*100)/2342

#Allocating genders for fully participated learners
MaleLearners7 = filter(Fully_finished7, gender == "male")
FemaleLearners7 = filter(Fully_finished7, gender == "female")
UnknownGenderLearners7= filter(Fully_finished7, gender == "Unknown")

#Allocating education levels
univercityDegree7 = filter(cyber.security.7_enrolments, highest_education_level == "university_degree")
univercitydoctorate7  = filter(cyber.security.7_enrolments, highest_education_level == "university_doctorate")
professional7  = filter(cyber.security.7_enrolments, highest_education_level == "professional")
UnknownEducation7 = filter(cyber.security.7_enrolments, highest_education_level == "Unknown")   

#Allocating employment status
distinct(cyber.security.7_enrolments, employment_status)
UnknownEmployment7 = filter(cyber.security.7_enrolments, employment_status == "Unknown")   

########################################################################################################################


######################################################################
#Step activity pre-processing

#Changed the step numbers
cyber.security.7_step.activity$step = cyber.security.7_step.activity$week_number*100 + cyber.security.7_step.activity$step_number

#Checking for NAs
sum(is.na(cyber.security.7_step.activity$learner_id))


#####################DATA CONSTRUCTION######################################


#Filter by week 1
Week1Steps7 = filter(cyber.security.7_step.activity, week_number == 1)

#Extract the number of how many didnt completed the steps
filter(Week1Steps7 , last_completed_at == "")
#Extract the count of observations that completed step 1
filter(Week1Steps7, step_number == 1 , !last_completed_at == "" )
filter(Week1Steps7, step_number == 1 , last_completed_at == "" )

#Filter by week 2
Week2Steps7 = filter(cyber.security.7_step.activity, week_number == 2)
#Extract the number of how many did not completed the steps
filter(Week2Steps7 , last_completed_at == "")

#Filter by week 3
Week3Steps7 = filter(cyber.security.7_step.activity, week_number == 3)
#Extract the number of how many didnt completed the steps
filter(Week3Steps7 , last_completed_at == "")

##################################################################
#Video stats pre-processing 
sum(is.na(cyber.security.7_video.stats))

############## DATA CONSTRUCTION##################################3
video_views7 = select(cyber.security.7_video.stats, c(1:15))
Video_devices7 = select(cyber.security.7_video.stats, c(16:21))
video_location7 = select(cyber.security.7_video.stats, c(22:28))





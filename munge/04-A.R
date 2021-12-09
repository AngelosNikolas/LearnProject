# Run 4
#Pre-processing script 


#Enrollments run  pre-processing.

#Checking and remove NAs
sum(is.na(cyber.security.4_enrolments))
cyber.security.4_enrolments= cyber.security.4_enrolments[rowSums(is.na(cyber.security.4_enrolments)) == 0,]
sum(is.na(cyber.security.4_enrolments))

# Remove rows with only NAs
cyber.security.4_enrolments[rowSums(is.na(cyber.security.4_enrolments)) != ncol(cyber.security.4_enrolments), ] 

#Checking for duplicates
cyber.security.4_enrolments$learner_id[duplicated(cyber.security.4_enrolments$learner_id)]

#############DATA CONSTRUCTION##############
# Creation of new set containing learner that fully finished the course.
Fully_finished4 = cyber.security.4_enrolments [!(!is.na(cyber.security.4_enrolments$fully_participated_at) & cyber.security.4_enrolments$fully_participated_at==""), ]



#Calculating the percentage of fully participation.
fully_participated4 = (166*100)/3992

#Allocating genders for fully participated learners
MaleLearners4 = filter(Fully_finished4, gender == "male")
FemaleLearners4 = filter(Fully_finished4, gender == "female")
UnknownGenderLearners4= filter(Fully_finished4, gender == "Unknown")

#Allocating education levels
univercityDegree4 = filter(cyber.security.4_enrolments, highest_education_level == "university_degree")
univercitydoctorate4  = filter(cyber.security.4_enrolments, highest_education_level == "university_doctorate")
professional4  = filter(cyber.security.4_enrolments, highest_education_level == "professional")
UnknownEducation4 = filter(cyber.security.4_enrolments, highest_education_level == "Unknown")   

#Allocating employment status
distinct(cyber.security.4_enrolments, employment_status)
UnknownEmployment4 = filter(cyber.security.4_enrolments, employment_status == "Unknown")   

########################################################################################################################
######################################################################
#Step activity pre-processing

#Changed the step numbers
cyber.security.4_step.activity$step = cyber.security.4_step.activity$week_number*100 + cyber.security.4_step.activity$step_number

#Checking for NAs
sum(is.na(cyber.security.4_step.activity$learner_id))


#####################DATA CONSTRUCTION######################################


#Filter by week 1
Week1Steps4 = filter(cyber.security.4_step.activity, week_number == 1)

#Extract the number of how many didnt completed the steps
filter(Week1Steps4 , last_completed_at == "")
#Extract the count of observations that completed step 1
filter(Week1Steps4, step_number == 1 , !last_completed_at == "" )
filter(Week1Steps4, step_number == 1 , last_completed_at == "" )

#Filter by week 2
Week2Steps4 = filter(cyber.security.4_step.activity, week_number == 2)
#Extract the number of how many did not completed the steps
filter(Week2Steps4 , last_completed_at == "")

#Filter by week 3
Week3Steps4 = filter(cyber.security.4_step.activity, week_number == 3)
#Extract the number of how many didnt completed the steps
filter(Week3Steps4 , last_completed_at == "")

##################################################################
#Video stats pre-processing 
sum(is.na(cyber.security.4_video.stats))

############## DATA CONSTRUCTION##################################3
video_views4 = select(cyber.security.4_video.stats, c(1:15))
Video_devices4 = select(cyber.security.4_video.stats, c(16:21))
video_location4 = select(cyber.security.4_video.stats, c(22:28))





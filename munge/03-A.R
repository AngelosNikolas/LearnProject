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

# Extracting the correct answers regarding the 1st week
Week1Correct3 = filter(cyber.security.3_question.response, week_number == 1)
Week1Correct3 =  filter(Week1Correct3, correct == "true" )

# Extracting all the answers for week 1
filter(cyber.security.3_question.response, week_number == 1)

#Calculating percentage of completion
Week1TruePercentage3 = (5783*0.1)/8.691

# Extracting the correct answers regarding the 2st week
Week2Correct2 = filter(cyber.security.3_question.response, week_number == 2)
Week2Correct2 =  filter(Week2Correct2, correct == "true" )

# Extracting all the answers for week 1
filter(cyber.security.3_question.response, week_number == 2)

#Calculating percentage of completion
Week2TruePercentage3 = (2170*100)/3969

# Extracting the correct answers regarding the 3st week
Week3Correct2 = filter(cyber.security.3_question.response, week_number == 3)
Week3Correct2 =  filter(Week3Correct2, correct == "true" )

# Extracting all the answers for week 1
filter(cyber.security.3_question.response, week_number == 3)

#Calculating percentage of completion
Week3TruePercentage3 = (1625*0.1)/3.860

#Construction for the completion rates
QuestionPercent3 = c(Week1TruePercentage3,Week2TruePercentage3,Week3TruePercentage3) 
Question_Data3= data.frame(Weeks,QuestionPercent3)
######################################################################
#Step activity pre-processing

#Changed the step numbers
cyber.security.3_step.activity$step = cyber.security.3_step.activity$week_number*100 + cyber.security.3_step.activity$step_number

#Checking for NAs
sum(is.na(cyber.security.3_step.activity$learner_id))


#####################DATA CONSTRUCTION######################################




#Filter by week 1
Week1Steps3 = filter(cyber.security.3_step.activity, week_number == 1)
#Extract the number of how many didn't completed the steps
filter(Week1Steps3 , last_completed_at == "")
#Extract the number of how many did not completed the steps and how many did
filter(Week1Steps3, week_number == 1 , !last_completed_at == "" )
filter(Week1Steps3, week_number == 1 , last_completed_at == "" )
Week1Step3Perc = (19464*0.1)/22.307

#Filter by week 2
Week2Steps2 = filter(cyber.security.3_step.activity, week_number == 2)
#Extract the number of how many did not completed the steps and how many did
filter(Week2Steps2, week_number == 2 , !last_completed_at == "" )
filter(Week2Steps2, week_number == 2 , last_completed_at == "" )
Week2Step3Perc = (13.835*100000)/14692

#Filter by week 3
Week3Steps2 = filter(cyber.security.3_step.activity, week_number == 3)
#Extract the number of how many didn't completed the steps and how many did
filter(Week3Steps2, week_number == 3 , !last_completed_at == "" )
filter(Week3Steps2, week_number == 3 , last_completed_at == "" )
Week3Step3Perc = (9.137*100000)/9615

#Creation of new data frames for analysis
Step_percentages3 = c(Week1Step3Perc,Week2Step3Perc,Week3Step3Perc)
Step_data3 = data.frame(Step_Weeks,Step_percentages3)

#Categorizing the step categories and calculating percentages of completion 
Videos3 = filter(cyber.security.3_step.activity, step %in% c("101","104","113","116","118","201","204","216","301","302","314","315"))
filter(Videos3, !last_completed_at == "" )
VideosPerc3 = (9.218*100000)/10590

Articles3 = filter(cyber.security.3_step.activity, step %in% c("102","105","106","108","111","112","114","115","117","202","205","207","208","210","211","212","213","214","215","217","218","220","305","306","307","308","309","310","313","317","320","321"))
filter(Articles3 , !last_completed_at == "" )
ArticlesPerc3 = (21.092*100000)/22880

Discussions3 = filter(cyber.security.3_step.activity, step %in% c("109","110","206","221","304","312","316","319"))
filter(Discussions3 , !last_completed_at == "" )
DiscussionsPerc3 = (4987*100)/5336

Quizes3 = filter(cyber.security.3_step.activity, step %in% c("107","208","219","311","318"))
filter(Quizes3, !last_completed_at == "" )
QuizesPerc3 = (2.778*100000)/2930

Exercises3 = filter(cyber.security.3_step.activity, step %in% c("103","203","303"))
filter(Exercises3, !last_completed_at == "" )
ExercisesPerc3 = (2555*100)/2894

#Construction for the step types
Step_Completion3 = c(VideosPerc3,ArticlesPerc3,DiscussionsPerc3,QuizesPerc3,ExercisesPerc3)
Step_Types_data3 = data.frame(Step_Types,Step_Completion3)







##################################################################
#Video stats pre-processing 
sum(is.na(cyber.security.3_video.stats))

############## DATA CONSTRUCTION##################################3
video_views3 = select(cyber.security.3_video.stats, c(1:15))

Video_devices3 = select(cyber.security.3_video.stats, c(16:21))
video_location3 = select(cyber.security.3_video.stats, c(22:28))

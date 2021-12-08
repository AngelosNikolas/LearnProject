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


#############DATA CONSTRUCTION##############
str(cyber.security.1_question.response)
unique(cyber.security.1_question.response$quiz_question)


# Extracting the correct answers regarding the 1st week
Week1Correct1 = filter(cyber.security.1_question.response, week_number == 1)
Week1Correct1 =  filter(Week1Correct1, correct == "true" )

# Extracting all the answers for week 1
filter(cyber.security.1_question.response, week_number == 1)

#Calculating percentage of completion
Week1TruePercentage = (19105*0.1)/28.471

# Extracting the correct answers regarding the 2st week
Week2Correct1 = filter(cyber.security.1_question.response, week_number == 2)
Week2Correct1 =  filter(Week2Correct1, correct == "true" )

# Extracting all the answers for week 1
filter(cyber.security.1_question.response, week_number == 2)

#Calculating percentage of completion
Week2TruePercentage = (5672*0.1)/11.362

# Extracting the correct answers regarding the 3st week
Week3Correct1 = filter(cyber.security.1_question.response, week_number == 3)
Week3Correct1 =  filter(Week3Correct1, correct == "true" )

# Extracting all the answers for week 1
filter(cyber.security.1_question.response, week_number == 3)

#Calculating percentage of completion
Week3TruePercentage = (4490*0.1)/10.118

#Construction for the completion rates
Weeks = c('Week1','Week2','Week3')
QuestionPercent = c(Week1TruePercentage,Week2TruePercentage,Week3TruePercentage) 
Question_Data = data.frame(Weeks,QuestionPercent)
######################################################################
#Step activity pre-processing

#Changed the step numbers
cyber.security.1_step.activity$step = cyber.security.1_step.activity$week_number*100 + cyber.security.1_step.activity$step_number

#Checking for NAs
sum(is.na(cyber.security.1_step.activity$learner_id))


#####################DATA CONSTRUCTION######################################


#Filter by week 1
Week1Steps1 = filter(cyber.security.1_step.activity, week_number == 1)
#Extract the number of how many didn't completed the steps
filter(Week1Steps1 , last_completed_at == "")
#Extract the number of how many did not completed the steps and how many did
filter(Week1Steps1, week_number == 1 , !last_completed_at == "" )
Week1Step1Perc = (57.308*100000)/63577

#Filter by week 2
Week2Steps1 = filter(cyber.security.1_step.activity, week_number == 2)
#Extract the number of how many did not completed the steps and how many did
filter(Week2Steps1, week_number == 2 , !last_completed_at == "" )
Week2Step1Perc = (41.710*100)/43.505

#Filter by week 3
Week3Steps1 = filter(cyber.security.1_step.activity, week_number == 3)
#Extract the number of how many didn't completed the steps and how many did
filter(Week3Steps1, week_number == 3 , !last_completed_at == "" )
Week3Step1Perc = (33.749*100)/35.990

#Creation of new data frames for analysis
Step_Weeks = c('Week1','Week2','Week3')
Step_percentages1 = c(Week1Step1Perc,Week2Step1Perc,Week3Step1Perc)
Step_data1 = data.frame(Step_Weeks,Step_percentages1)

#Categorizing the step categories and calculating percentages of completion 
Videos1 = filter(cyber.security.1_step.activity, step %in% c("101","104","113","116","118","201","204","216","301","302","314","315"))
filter(Videos1, !last_completed_at == "" )
VideosPerc1 = (28.796*100000)/32039

Articles1 = filter(cyber.security.1_step.activity, step %in% c("102","105","106","108","111","112","114","115","117","202","205","207","208","210","211","212","213","214","215","217","218","220","305","306","307","308","309","310","313","317","320","321"))
filter(Articles1 , !last_completed_at == "" )
ArticlesPerc1 = (69.661*100000)/74345

Discussions1 = filter(cyber.security.1_step.activity, step %in% c("109","110","206","221","304","312","316","319"))
filter(Discussions1 , !last_completed_at == "" )
DiscussionsPerc1 = (16.258*100000)/17000

Quizes1 = filter(cyber.security.1_step.activity, step %in% c("107","208","219","311","318"))
filter(Quizes1, !last_completed_at == "" )
QuizesPerc1 = (10.095*100000)/11048

Exercises1 = filter(cyber.security.1_step.activity, step %in% c("103","203","303"))
filter(Exercises1, !last_completed_at == "" )
ExercisesPerc1 = (7.901*100000)/8678

#Construction for the step types
Step_Types = c("Videos","Articles","Discussions","Quizes","Exercises")
Step_Completion1 = c(VideosPerc1,ArticlesPerc1,DiscussionsPerc1,QuizesPerc1,ExercisesPerc1)
Step_Types_data1 = data.frame(Step_Types,Step_Completion1)



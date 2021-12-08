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


#############DATA CONSTRUCTION##############
str(cyber.security.2_question.response)
unique(cyber.security.2_question.response$quiz_question)


# Extracting the correct answers regarding the 1st week
Week1Correct2 = filter(cyber.security.2_question.response, week_number == 1)
Week1Correct2 =  filter(Week1Correct2, correct == "true" )

# Extracting all the answers for week 1
filter(cyber.security.2_question.response, week_number == 1)

#Calculating percentage of completion
Week1TruePercentage2 = (7796*0.1)/11.629

# Extracting the correct answers regarding the 2st week
Week2Correct2 = filter(cyber.security.2_question.response, week_number == 2)
Week2Correct2 =  filter(Week2Correct2, correct == "true" )

# Extracting all the answers for week 1
filter(cyber.security.2_question.response, week_number == 2)

#Calculating percentage of completion
Week2TruePercentage2 = (3247*0.1)/5.901

# Extracting the correct answers regarding the 3st week
Week3Correct2 = filter(cyber.security.2_question.response, week_number == 3)
Week3Correct2 =  filter(Week3Correct2, correct == "true" )

# Extracting all the answers for week 1
filter(cyber.security.2_question.response, week_number == 3)

#Calculating percentage of completion
Week3TruePercentage2 = (2135*0.1)/4.933

#Construction for the completion rates
QuestionPercent2 = c(Week1TruePercentage2,Week2TruePercentage2,Week3TruePercentage2) 
Question_Data2= data.frame(Weeks,QuestionPercent)






######################################################################
#Step activity pre-processing

#Changed the step numbers
cyber.security.2_step.activity$step = cyber.security.2_step.activity$week_number*100 + cyber.security.2_step.activity$step_number

#Checking for NAs
sum(is.na(cyber.security.2_step.activity$learner_id))


#####################DATA CONSTRUCTION######################################


#Filter by week 1
Week1Steps2 = filter(cyber.security.2_step.activity, week_number == 1)
#Extract the number of how many didn't completed the steps
filter(Week1Steps2 , last_completed_at == "")
#Extract the number of how many did not completed the steps and how many did
filter(Week1Steps2, week_number == 1 , !last_completed_at == "" )
filter(Week1Steps2, week_number == 1 , last_completed_at == "" )
Week1Step2Perc = (25.860*100)/28.820

#Filter by week 2
Week2Steps2 = filter(cyber.security.2_step.activity, week_number == 2)
#Extract the number of how many did not completed the steps and how many did
filter(Week2Steps2, week_number == 2 , !last_completed_at == "" )
filter(Week2Steps2, week_number == 2 , last_completed_at == "" )
Week2Step2Perc = (20.135*100)/21.152

#Filter by week 3
Week3Steps2 = filter(cyber.security.2_step.activity, week_number == 3)
#Extract the number of how many didn't completed the steps and how many did
filter(Week3Steps2, week_number == 3 , !last_completed_at == "" )
filter(Week3Steps2, week_number == 3 , last_completed_at == "" )
Week3Step2Perc = (13.849*100000)/14837

#Creation of new data frames for analysis
Step_percentages2 = c(Week1Step2Perc,Week2Step2Perc,Week3Step2Perc)
Step_data2 = data.frame(Step_Weeks,Step_percentages2)

#Categorizing the step categories and calculating percentages of completion 
Videos2 = filter(cyber.security.2_step.activity, step %in% c("101","105","114","117","119","201","204","211","217","301","302","314","315"))
filter(Videos2, !last_completed_at == "" )
VideosPerc2 = (13.356*100000)/14917

Articles2 = filter(cyber.security.2_step.activity, step %in% c("103","106","107","109","112","113","115","116","118","202","205","207","209","210","212","213","214","215","216","218","219","221","222","305","306","307","308","309","310","313","317","320","321"))
filter(Articles2 , !last_completed_at == "" )
ArticlesPerc2 = (30.704*100)/32.795

Discussions2 = filter(cyber.security.2_step.activity, step %in% c("102","110","111","206","223","304","312","316","319"))
filter(Discussions2 , !last_completed_at == "" )
DiscussionsPerc2 = (8.751*100000)/9394

Quizes2 = filter(cyber.security.2_step.activity, step %in% c("108","208","220","311","318"))
filter(Quizes2, !last_completed_at == "" )
QuizesPerc2 = (3.641*100000)/4010

Exercises2 = filter(cyber.security.2_step.activity, step %in% c("104","203","303"))
filter(Exercises2, !last_completed_at == "" )
ExercisesPerc2 = (3.382*100000)/3693

#Construction for the step types
Step_Types = c("Videos","Articles","Discussions","Quizes","Exercises")
Step_Completion2 = c(VideosPerc2,ArticlesPerc2,DiscussionsPerc2,QuizesPerc2,ExercisesPerc2)
Step_Types_data2 = data.frame(Step_Types,Step_Completion2)






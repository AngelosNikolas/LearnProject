library("ProjectTemplate")
load.project()

# Construct a data frame containing the fully participated learners
Fully_Part=c(fully_participated1,fully_participated2,fully_participated3,fully_participated4,fully_participated5,fully_participated6,fully_participated7)
Participation_percent= data.frame(RunNumber,Fully_Part)
View(Participation_percent)

#Plot the enrollment counts 
EnrollCountPlot=ggplot(data=Enroldata, aes(x=RunNumber, y=EnrolCount)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=EnrolCount), vjust=1.6, color="white", size=3.5)+
  theme_minimal()

EnrollCountPlot
#Plot the participation percentages
bp1<- ggplot(Participation_percent, aes(x="", y=Fully_Part, fill=RunNumber))+
  geom_bar(width = 1, stat = "identity")
pie1 <- bp1 + coord_polar("y", start=0)
pie1

# Investigation on the learners that fully participated in the most successful run 
Gender = c('Male','Female','Unknown')
Gender_count = c(nrow(MaleLearners1),nrow(FemaleLearners1), nrow(UnknownGenderLearners1))
Run1Gender = data.frame(Gender,Gender_count)


#Plot education for run 1
Education_Plot=ggplot(data=Education_data, aes(x=Education_Levels, y=Education_Count)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Education_Count), vjust=1.6, color="white", size=3.5)+
  theme_minimal()
Education_Plot
View(Education_data)

#Plot employment status for run 1
Employment_Plot=ggplot(data=Employment_data, aes(x=Employment_Levels, y=Employment_Count)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Employment_Count), vjust=1.6, color="white", size=3.5)+
  theme_minimal()
Employment_Plot

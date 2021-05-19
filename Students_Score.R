# Loading necessary libraries
library(tidyverse)
library(plyr)


######################################################################### loading and preparing Data ###########################################################

score_data <- as.data.frame(read.csv("./Dataset/StudentsPerformance.csv"))
print( head(score_data))

namesOfColumns <- c("Sex","Race","Parent_Education","Lunch","Test_Prep","Math_Score","Reading_Score","Writing_Score")
colnames(score_data) <- namesOfColumns
names(score_data)

# Defining a new variable (Average_Score wich is the average of all scores)
score_data$Average_Score <- (score_data$Math_Score + score_data$Reading_Score + score_data$Writing_Score)/3

# Reordering Parent Education levels
score_data$Parent_Education <- factor(score_data$Parent_Education , levels=c("some high school", "high school", "some college", "associate's degree","bachelor's degree","master's degree"))

# Defining Color blind palette

cbPalette <- c("#999999","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7")

########################################################### comparsion based on Economic Situation ############################################################

# Calculating the averge score based on different Lunch groups

Lunch_Average_Score <- ddply(score_data, "Lunch" , summarise, Score.mean=mean(Average_Score))

## Average Score distribution based on lunch groups

ggplot(score_data)+
  geom_density(aes(x=Average_Score,color=Lunch),size=1.2)+
  geom_vline(data =Lunch_Average_Score, aes(xintercept=Score.mean,  colour=Lunch), linetype="dashed", size=1) +
  geom_text(data =Lunch_Average_Score,aes(x=Score.mean,y=0,label=format(round(Lunch_Average_Score$Score.mean,2), nsmall = 2)), size=4, angle=90, vjust=-0.4, hjust=0)+
  theme_bw() +
  ggtitle("Average SCORE Based on Lunch Type \n With Vertical Average Lines") +
  theme(plot.title = element_text(color = "black",hjust = 0.5,face="bold.italic",size=12),
        axis.title.x = element_text(color="black", size=12, face="bold"),
        axis.title.y = element_text(color="black", size=12, face="bold")) +
        theme(legend.title = element_text(size=10, face="bold"))+theme(legend.position = c(0.2, 0.9))+
         scale_color_manual(values=c("#E69F00","#999999"))

### Average Score Boxplot based on Parents Education Levels

# Calculating Average scores based on different Parents Education Levels & Test preparation status

Parent_Education_Average_Score <- ddply(score_data, "Parent_Education" , summarise, Score.mean=mean(Average_Score))


## Boxplot
ggplot(score_data)+
  geom_boxplot(aes(x=Parent_Education,y=Average_Score,fill=Test_Prep))+labs(fill="Test Prepration Course")+
  ggtitle("Students performance based on parents edjucation level and \n completing test prepration cource")+ scale_fill_manual(values = cbPalette,labels = c("Completed", "Not completed"))+
  theme_bw()+theme(plot.title = element_text(color = "black",hjust = 0.5,face="bold.italic",size=12),
                   axis.title.x = element_text(color="black", size=12, face="bold"),
                   axis.title.y = element_text(color="black", size=12, face="bold"))+xlab('')+ylab('Score')+theme(legend.position = c(0.85,0.15))+
  theme(axis.text.x = element_text(face = "bold", size = 8, angle = 90))




### Average Score Boxplot based on Completing/Not Completing a test prepration course

# Calculating Average scores based on Completing/Not Completing a test prepration course
Test_Prep_Average_Score <- ddply(score_data, "Test_Prep" , summarise, Score.mean=mean(Average_Score))

ggplot(score_data)+
  geom_density(aes(x=Average_Score,color=Test_Prep),size=1.2)+
  
  geom_vline(data =Test_Prep_Average_Score, aes(xintercept=Score.mean,  colour=Test_Prep), linetype="dashed", size=1) +
  geom_text(data =Test_Prep_Average_Score,aes(x=Score.mean,y=0,label=format(round(Score.mean,2), nsmall = 2)), size=4, angle=90, vjust=-0.4, hjust=0)+
  theme_bw() +
  labs(color="Test Prepration Course",size=6)+
  ggtitle("Average SCORE Based on Attending in The Test Prepation Course \n With Vertical Average Lines") +
  theme(plot.title = element_text(color = "black",hjust = 0.5,face="bold.italic",size=11),
        axis.title.x = element_text(color="black", size=12, face="bold"),
        axis.title.y = element_text(color="black", size=12, face="bold") ,
        legend.title = element_text(size=10, face="bold"))+theme(legend.position = c(0.2, 0.9))+
  scale_color_manual(labels = c("Completed", "Not completed"),values=cbPalette)

######################################################################### Sex based Analyze  #################################################################

Sex_Average_Score <- ddply(score_data, "Sex" , summarise, Score.mean=mean(Average_Score))

ggplot(score_data)+
  geom_density(aes(x=Average_Score,color=Sex),size=1.2)+
  
  geom_vline(data =Sex_Average_Score, aes(xintercept=Score.mean,colour=Sex), linetype="dashed", size=1) +
  geom_text(data =Sex_Average_Score,aes(x=Score.mean,y=0,label=format(round(Sex_Average_Score$Score.mean,2), nsmall = 2)), size=4, angle=90, vjust=-0.4, hjust=0)+
  theme_bw() +
  ggtitle("Average SCORE Based on Sex \n With Vertical Average Lines") +
  theme(plot.title = element_text(color = "black",hjust = 0.5,face="bold.italic",size=12),
        axis.title.x = element_text(color="black", size=12, face="bold"),
        axis.title.y = element_text(color="black", size=12, face="bold")) +
        theme(legend.title = element_text(size=10, face="bold"))+
  scale_color_manual(values=c("#D55E00","#56B4E9"))+theme(legend.position = c(0.2, 0.9))

### Math and Writing Score comparsion based on sex

ggplot(score_data)+
  geom_point(aes(x=Math_Score,y=Writing_Score,color=Sex),size=1.2,alpha=0.8)+theme_bw()+ggtitle("Math and Writing Score Comparsion \n Based on Sex") +
  theme(plot.title = element_text(color = "black",hjust = 0.5,face="bold.italic",size=12),
        axis.title.x = element_text(color="black", size=12, face="bold"),
        axis.title.y = element_text(color="black", size=12, face="bold")) +
  theme(legend.title = element_text(size=10, face="bold"))+
  scale_color_manual(values=c("#D55E00","#56B4E9"))+theme(legend.position = c(0.2, 0.9))

### Math and Reading Score comparsion based on sex
ggplot(score_data)+
  geom_point(aes(x=Math_Score,y=Reading_Score,color=Sex),size=1.2,alpha=0.8)+theme_bw()+ggtitle("Math and Reading Score Comparsion \n Based on Sex") +
  theme(plot.title = element_text(color = "black",hjust = 0.5,face="bold.italic",size=12),
        axis.title.x = element_text(color="black", size=12, face="bold"),
        axis.title.y = element_text(color="black", size=12, face="bold")) +
  theme(legend.title = element_text(size=10, face="bold"))+
  scale_color_manual(values=c("#D55E00","#56B4E9"))+theme(legend.position = c(0.2, 0.9))


### Boxplot comparsion of all 3 scores based on participants sex
ggplot(score_data,aes(color=Sex))+
  geom_boxplot(aes(x='Math',y=Math_Score))+
  geom_boxplot(aes(x='Writing',y=Writing_Score))+
  geom_boxplot(aes(x="Reading",y=Reading_Score))+
  ggtitle("Students performance on each catagory \n Based on their Sex")+
  theme_bw()+theme(plot.title = element_text(color = "black",hjust = 0.5,face="bold.italic",size=12),
                   axis.title.x = element_text(color="black", size=12, face="bold"),
                   axis.title.y = element_text(color="black", size=12, face="bold"))+xlab('')+ylab('Score')+theme(legend.position = c(0.1, 0.9))+
  theme(axis.text.x = element_text(face = "bold"))+scale_color_manual(values=c("#D55E00","#56B4E9"))



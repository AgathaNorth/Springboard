library(tidyverse)
library(lubridate)
library(randomForest)
library(caTools)
library(plotly)
library(rfUtilities)


#Questions for the data;
#1: How many users that were actively engaged by the App, either with frequent comments or likes/dislikes, churned?
#2: Was negative user engagement, lots of dislikes and low mood votes, a significant factor for churning?
#3: What was the most common trend between users that did churn?


#Read Data

votes <- read.csv("DHAD/votes.csv")
churn <- read.csv("DHAD/churn.csv")
comments <- read.csv("DHAD/comments_clean_anonimized.csv")
interaction <- read.csv("DHAD/commentInteractions.csv")

#Clean Dates

votes$voteDate <- gsub("CET","",votes$voteDate)
votes$voteDate <- gsub("CEST","",votes$voteDate)
votes$voteDate <- gsub("  "," ",votes$voteDate)
votes$voteDate <- parse_date_time(votes$voteDate, "%a %b %d %H:%M:%S %Y")

churn$lastParticipationDate <- gsub("CET","",churn$lastParticipationDate)
churn$lastParticipationDate <- gsub("CEST","",churn$lastParticipationDate)
churn$lastParticipationDate <- gsub("  "," ",churn$lastParticipationDate)
churn$lastParticipationDate <- parse_date_time(churn$lastParticipationDate, "%a %b %d %H:%M:%S %Y")
  
comments$commentDate <- gsub("CET","",comments$commentDate)
comments$commentDate <- gsub("CEST","",comments$commentDate)
comments$commentDate <- gsub("  "," ",comments$commentDate)
comments$commentDate <- parse_date_time(comments$commentDate, "%a %b %d %H:%M:%S %Y")

#Company Names

levels(votes$companyAlias) <- c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C30","C31","C32","C33","C34","C35","C36","C37")
levels(churn$companyAlias) <- c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C30","C31","C32","C33","C34","C35","C36","C37")
levels(comments$companyAlias) <- c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C30","C31","C32","C33","C34","C35","C36")
levels(interaction$companyAlias) <- c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C30","C31","C32","C33","C34","C35")

#Rename for simplicity

colnames(churn)[4] <- c("lastPostDate")

#Group individual employee information

votes$EmpID <- paste0(votes$employee, votes$companyAlias)
comments$EmpID <- paste0(comments$employee, comments$companyAlias)
interaction$EmpID <- paste0(interaction$employee, interaction$companyAlias)
churn$EmpID <- paste0(churn$employee, churn$companyAlias)

comments$txt <- as.character(comments$txt)
comments$LengthTxt <- nchar(comments$txt)

EmpVoteCount <- votes%>%group_by(EmpID)%>%dplyr::summarize(VoteCount=n(),
                                                          MeanVote=mean(vote))

EmpComments <- comments%>%group_by(EmpID)%>%dplyr::summarize(AvgLength=mean(LengthTxt),
                                                                 AvgLikes=mean(likes),
                                                                 AvgDislikes=mean(dislikes))

EmpCombined <- left_join(EmpVoteCount,EmpComments)

EmpCombined[is.na(EmpCombined)] <- 0

churn <- churn%>%dplyr::select(EmpID, stillExists, companyAlias)
EmpCombined <- left_join(EmpCombined,churn)

#Statistical Analysis

#Votes by company

ggplot(votes, aes(vote, fill = companyAlias)) + geom_bar() + facet_grid(~ votes$companyAlias) + labs(title = "Votes by Company")
#Interesting note, a vote of 3 is most typical in the majority of companies with sufficent data... except for 2 and 12 which have
#significantly more 4s. 

ggplot(comments, aes(likes, fill = companyAlias)) + geom_histogram(binwidth = 1)
ggplot(comments, aes(dislikes, fill = companyAlias)) + geom_histogram(binwidth = 1)


ggplot(EmpCombined, aes(EmpID, col = stillExists, x = MeanVote, y = VoteCount)) + geom_point(position = "jitter", alpha=0.6)
#At a quick glance its clear that 3 was the most common vote, and most employees voted between 1 and 200 times.
#Interestingly it seems like there is a roughly even distribution of employees that churned amongst the 4.
#It's also interesting to note that there were employees that had a mean vote of around 1 who voted more than 500 times
#And still worked at their companies.
#This having been said, it is clear that the density of employees with an average of 3 that still exist is higher than
#those at the lower end of the spectrum.

ggplot(EmpCombined, aes(EmpID, col = stillExists, x=VoteCount, y=AvgLength)) + geom_point(position = "jitter", alpha=0.4)
ggplot(EmpCombined, aes(EmpID, col = stillExists, x=VoteCount, y=AvgDislikes)) + geom_point(position = "jitter", alpha=0.4)
ggplot(EmpCombined, aes(EmpID, col = stillExists, x=VoteCount, y=AvgLikes)) + geom_point(position = "jitter", alpha=0.4)
#Having a higher level of engagement with length of comments, likes, and dislikes, RATHER than the bare minimum voting
#seems to relate to less churning overall. 



#find most active day of the week, most active month



#Split the Data

EmpCombMod <- dplyr::select(EmpCombined, -EmpID, -lastPostDate)

set.seed(237)
split <- sample.split(EmpCombMod, SplitRatio = 0.75)
EmpCom_train <- EmpCombMod[split,]
EmpCom_test <- EmpCombMod[!split,]


model1 <- randomForest(stillExists ~ ., data=EmpCom_train,
                      ntree=500,
                      mtry=2,
                      importance=TRUE,
                      na.action=na.roughfix,
                      replace=FALSE)
model1

outcome <- predict(model1, EmpCom_test)
table(EmpCom_test$stillExists,outcome, dnn=c("Actual", "Predicted"))


rn <- round(importance(model1), 2)
rn[order(rn[,3], decreasing=TRUE),]

varImpPlot(model1, main="")


Emp_trainx <- dplyr::select(EmpCom_train, -stillExists)
result <- rfcv(Emp_trainx, EmpCom_train$stillExists, cv.fold=10)
with(result, plot(n.var, error.cv, log="x", type="o", lwd=2))


model2 <- randomForest(stillExists ~ ., data=EmpCom_train,
                          ntree=4000,
                          sampsize=c(500,500),
                          strata=EmpCom_train$stillExists,
                          mtry=2,
                          importance=TRUE,
                          na.action=na.roughfix,
                          replace=FALSE)
model2

rn <- round(importance(model2), 2)
rn[order(rn[,3], decreasing=TRUE),]

varImpPlot(model2, main="")

model3 <- randomForest(stillExists ~ companyAlias + MeanVote + VoteCount + AvgLikes, data=EmpCom_train,
                       ntree=4000,
                       sampsize=c(5,100),
                       strata=EmpCom_train$stillExists,
                       mtry=2,
                       importance=TRUE,
                       na.action=na.roughfix,
                       replace=FALSE)
model3

rn <- round(importance(model3), 2)
rn[order(rn[,3], decreasing=TRUE),]

varImpPlot(model3, main="")


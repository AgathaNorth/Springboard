states.data <- readRDS("dataSets/states.rds")
states.info <- data.frame(attributes(states.data)[c("names","var.labels")])
tail(states.info, 8)


#Linear Regression
#Exercise 0 
#Examine the Data
str(states.data)
Mod1 <- subset(states.data[c("metro","energy")])
summary(Mod1)
plot(Mod1)

#Interpret and Plot
EnMet <- lm(energy ~ metro, data=states.data)
summary(EnMet)
plot(EnMet)

#Add additional predictors
cor(states.data)
Mod2 <- lm(energy ~., data=states.data)
summary(Mod2)

Mod3 <- lm(energy ~ metro + pop + density + waste + miles, data=states.data)
summary(Mod3)

Mod4 <- lm(energy ~ metro + pop + density + waste, data=states.data)
summary(Mod4)
table(states.data$energy, states.data$metro)

#Compare predictors
anova(EnMet, Mod4)


#Exercise 1
#Add interaction
EnMetInter <- lm(energy ~ metro * waste, data=states.data)
summary(EnMetInter)

#Add Regional data
EnMetRegion <- lm(energy ~ metro * waste + region, data=states.data)
summary(EnMetRegion)
anova(EnMetRegion)
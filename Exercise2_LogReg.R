#Read in and examine data

NH11 <- readRDS("dataSets/NatHealth2011.rds")
summary(NH11)

#Subset to the data we want to work with

NH11Sub1 <- subset(NH11[c("everwrk", "age_p", "r_maritl")])
summary(NH11Sub1)
NH11Sub1$everwrk <- as.factor(NH11Sub1$everwrk)

#Model the data using glm

Mod1 <- glm(everwrk ~ age_p + r_maritl, data = NH11Sub1, family = "binomial")
summary(Mod1)

#Load in Effects
install.packages("effects")
library(effects)

data.frame(Effect("r_maritl", Mod1))

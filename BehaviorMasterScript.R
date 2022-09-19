#Load data

library(tidyverse)

data <- read.csv("Behav_data_mas.csv")

#make a column that represents how long each fish spent near shoal

data$shoal_prop <- ifelse(data$Shoal.Location == "L", data$Left, data$Right)

#define how many seconds the fish spent shoaling and not shoaling out of 15 mins.
data$sec.shoal <- round(data$shoal_prop*(60*15))
data$sec.noshoal <- round((1-data$shoal_prop)*(60*15))

data$father <- NULL

for(i in 1:nrow(data)){
  data[i,"male_origin"] <- paste(strsplit(data[i,"Cross.Type"], "")[[1]][c(1,2)], collapse = "")
  data[i,"fem_origin"] <- paste(strsplit(data[i,"Cross.Type"], "")[[1]][c(3,4)], collapse = "")
}




#predictors
#Warm vs cold treatment fix
#cross type fix
#age in degree days fix
#shoal location ran
#tank id ran
#trial number fix
#fish id ran

#check data
hist(data$shoal_prop)
hist(data$sec.shoal)
hist(data$sec.noshoal)

hist(data$Degree.Days)
hist(data$Trial..)
#check if number of trials makes sense
sum(data$Temp..Expiriment. == "C"& data$Cross.Type == "HMHF")

boxplot(data$Degree.Days~data$Cross.Type)
boxplot(data$Degree.Days~data$Temp..Expiriment.)

#note - need to deal with correlated fixed effects

library(lme4)

model1 <- glmer(cbind(sec.shoal, sec.noshoal) ~ Temp..Expiriment. 
                + Cross.Type + scale(Degree.Days) + Trial.. + (1|Shoal.Location) 
                + (1|Tank_ID) + (1|Fish_ID), data = data, family = "binomial")

summary(model1)

#try to model the interaction btwn cross type and temperature
#star between temp and cross type instead of plus

model2 <- glmer(cbind(sec.shoal, sec.noshoal) ~ Temp..Expiriment. 
                * Cross.Type + scale(Degree.Days) + Trial.. + (1|Shoal.Location) 
                + (1|Tank_ID) + (1|Fish_ID), data = data, family = "binomial")

summary(model2)

plot(model1)

boxplot(sec.shoal~Cross.Type, data = data)

plot(model2)

barplot(sec.shoal~Temp..Expiriment.*Cross.Type, data = data)

#Look at how to assess model fit
#refit model with interaction btwn temp and cross type and assess fit

#how to deal with correlated fixed effects?

#AIC compare models 
#check whether degree days or trial has a stronger correlated effect

model3 <- glmer(cbind(sec.shoal, sec.noshoal) ~ Temp..Expiriment. 
                + Cross.Type + Trial.. + (1|Shoal.Location) 
                + (1|Tank_ID) + (1|Fish_ID), data = data, family = "binomial")

model4 <- glmer(cbind(sec.shoal, sec.noshoal) ~ Cross.Type + scale(Degree.Days) + Trial.. + (1|Shoal.Location) 
                + (1|Tank_ID) + (1|Fish_ID), data = data, family = "binomial")

AIC(model3, model4)

summary(model3)

model5 <- glmer(cbind(sec.shoal, sec.noshoal) ~ Temp..Expiriment. 
      + fem_origin + male_origin + Trial.. + (1|Shoal.Location) 
      + (1|Tank_ID) + (1|Fish_ID), data = data, family = "binomial")



boxplot(data$shoal_prop~data$Temp..Expiriment.)
boxplot(data$shoal_prop~data$Cross.Type)
boxplot(data$shoal_prop~data$Temp..Expiriment.*data$Cross.Type)
boxplot(data$shoal_prop~data$Cross.Type*data$Temp..Expiriment.)

sum(data$Cross.Type== "HMHF" & data$Temp..Expiriment. == "C")

data %>%
  ggplot(aes(x = Trial.., y = shoal_prop)) + geom_point() + geom_smooth(
    method="glm",
    method.args=list(family="binomial"),
  )

library(PNWColors)
pal <- pnw_palette("Sunset",100)

#MAIN PLOT TO USE

data %>% 
  ggplot(aes(x = Temp..Expiriment., y = shoal_prop, colour = Cross.Type)) + geom_boxplot() + labs(x = "Experiment Temperature", y = "Proportion of Time Spent Shoaling") +
  theme_classic()

data %>% 
  ggplot(aes(x = Temp..Expiriment., y = shoal_prop, colour = fem_origin)) + geom_boxplot() + labs(x = "Experiment Temperature", y = "Proportion of Time Spent Shoaling") +
  theme_classic()
data %>% 
  ggplot(aes(x = Temp..Expiriment., y = shoal_prop, colour = male_origin)) + geom_boxplot() + labs(x = "Experiment Temperature", y = "Proportion of Time Spent Shoaling") +
  theme_classic()

summary(model5)



model6 <- glmer(cbind(sec.shoal, sec.noshoal) ~ Temp..Expiriment. 
                * (fem_origin + male_origin) + Trial.. + (1|Shoal.Location) 
                + (1|Tank_ID) + (1|Fish_ID), data = data, family = "binomial")

model7 <- glmer(cbind(sec.shoal, sec.noshoal) ~ Temp..Expiriment. 
                * fem_origin + male_origin + Trial.. + (1|Shoal.Location) 
                + (1|Tank_ID) + (1|Fish_ID), data = data, family = "binomial")

model8 <- glmer(cbind(sec.shoal, sec.noshoal) ~ Temp..Expiriment. 
                + fem_origin * male_origin + Trial.. + (1|Shoal.Location) 
                + (1|Tank_ID) + (1|Fish_ID), data = data, family = "binomial")

model9 <- glmer(cbind(sec.shoal, sec.noshoal) ~ Temp..Expiriment. 
                 * male_origin + fem_origin + Trial.. + (1|Shoal.Location) 
                + (1|Tank_ID) + (1|Fish_ID), data = data, family = "binomial")

summary(model7)
summary(model8)
summary(model9)

# repeatability

library(rptR)
rpt(sec.shoal ~ Temp..Expiriment. 
     + Cross.Type + Trial.. + (1|Shoal.Location) 
     + (1|Tank_ID) + (1|Fish_ID), data = data, grname = c("Fish_ID", "Shoal.Location", "Tank_ID"), datatype = "Gaussian", nboot = 0, npermut = 0)


rpt(sec.shoal ~ (1|Fish_ID), grname = "Fish_ID", data = data, datatype = "Gaussian", nboot = 0, npermut = 0)

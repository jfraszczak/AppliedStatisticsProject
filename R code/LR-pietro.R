
library("ggpubr")

library(sp)           ## Data management
library(lattice)      ## Data management
library(gstat) 
library(MASS)
library(car)
library(rgl)
library(glmnet)
library(nlmeU)
library(corrplot)
library(nlme)
library(lattice)
library(plot.matrix)
library(lme4)
library(insight)


load('~/University Courses/Polimi/Semester 2/Applied Statistics/Project/R code')






#SIMPLE LINEAR REGRESSION





dataset_r <- dataset[,4:25] #I remove also population, as it is not a scaled variable
attach(dataset_r)
n          <- dim(dataset_r)[[1]]

fm11 <- lm(cases_first_wave ~.,data = dataset_r)

summary(fm11) 
#I want to remove several regressors. Instead of removing them one by one, I test at the same time if several of them are = 0

linearHypothesis(fm11, rbind(c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0),
                             c(0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0),
                             c(0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                             c(0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0),
                             c(0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0),
                             c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0),
                             c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0),
                             c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0),
                             c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0),
                             c(0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0),
                             c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0),
                             c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0)), c(0,0,0,0,0,0,0,0,0,0,0,0)) 
#I remove those variable from my model

fm21 <- lm(cases_first_wave ~ students_in_tertiary_education + `%_of_early_leavers_from_education` + 
             `%_of_people_studying_or_training` + students + 
             life_expectancy + death_rate_scaled_on_100k + discharges_after_respiratory_disease + 
             +`unemployement_rate_in_%` + `GVA_in_%` + cases_second_wave)
summary(fm21) 




#Now for the second wave


fm12 <- lm(cases_second_wave ~.,data = dataset_r)

summary(fm12) 
#I want to remove several regressors. Instead of removing them one by one, I test at the same time if several of them are = 0

linearHypothesis(fm12, rbind(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),
                             c(0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0),
                             c(0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                             c(0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                             c(0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0)), c(0,0,0,0,0)) 



fm22 <- lm(cases_second_wave ~ `pop_density_scaled_on_1km^2` + 
             students_in_tertiary_education + 
             `NEET_%` + `%_of_people_studying_or_training` + death_rate_scaled_on_100k + discharges_after_respiratory_disease + 
             avalaible_hospital_beds_scaled_on_100k + 
             air_passengers_x100k + vehicles + farm_labour_force + utilised_agricoltural_area + 
             `unemployement_rate_in_%` + `total_compensation_of_employees_x10^6` + 
             sum_of_hours_worked_x1000 + gdp_x1M + cases_first_wave)
summary(fm22) 





#TO CHECK FINAL RESULTS
summary(fm21) #first wave
summary(fm22)  #second wave

#RESULTS :
# - Regarding first wave, the most significant regressors are: life expectancy, and death rate

# - Regarding second wave, the most significant regressors are: population density, NEET%, %_of_people_studying_or_training
#                                                               avalaible hospital beds,employee compensation, hours worked,gdp and cases in the first wave

# it is interesting to note that life expectancy, that was one of the most significant regressors in the first wave, is one of the worst in the second wave
# interesting to note also the positive correlation in both model for the covid cases: it means that regions with high density of covid cases in one wave, has high cases also in the other wave














# LINEAR MIXED MODEL



dataset_rs <- as.data.frame(scale(dataset_r, center = TRUE, scale = TRUE)) #it told me to scale
dataset_rs$country <- dataset$country 


#Let's remove 5 observations, so we'll be able later on to test the goodness of our model.
test_data <- dataset_rs[c(7,23,78,99,120),]
dataset_rs <- dataset_rs[-c(7,23,78,99,120),]


#For the first wave

Lmm1 <- lmer(cases_first_wave ~ students_in_tertiary_education + `%_of_early_leavers_from_education` + 
               life_expectancy + discharges_after_respiratory_disease + 
               +`unemployement_rate_in_%` + (1|country) ,data = dataset_rs) #I use the same regressors I found out as the best in regression

# I do only on the intercept

summary(Lmm1)

confint(Lmm1,oldNames=TRUE) #I think that if 0 is contained, that feature should be removed, I don't remove now

fixef(Lmm1)


sigma2_eps1 <- as.numeric(get_variance_residual(Lmm1))
sigma2_eps1
sigma2_b1 <- as.numeric(get_variance_random(Lmm1))
sigma2_b1

PVRE1 <- sigma2_b1/(sigma2_b1+sigma2_eps1)
PVRE1                                                 #The value seems incredibly high! 0.63. Though at the end we find out it sucks



dotplot(ranef(Lmm1, condVar=T)) #95% CI
ranef(Lmm1, condVar=T)


#Now let's check the goodness by predicting our observations

predict(Lmm1) #super bad
predict(Lmm1, test_data[1,])
predict(Lmm1, test_data[2,])
predict(Lmm1, test_data[3,])
predict(Lmm1, test_data[4,])
predict(Lmm1, test_data[5,])






#For the second wave


Lmm2 <- lmer(cases_second_wave ~ `pop_density_scaled_on_1km^2` + 
               students_in_tertiary_education + 
               `NEET_%` + `%_of_people_studying_or_training` + death_rate_scaled_on_100k + discharges_after_respiratory_disease + 
               avalaible_hospital_beds_scaled_on_100k + 
               air_passengers_x100k + vehicles + farm_labour_force + utilised_agricoltural_area + 
               `unemployement_rate_in_%` + `total_compensation_of_employees_x10^6` + 
               sum_of_hours_worked_x1000 + gdp_x1M + (1|country),data = dataset_rs)

summary(Lmm2)

confint(Lmm2,oldNames=TRUE) #I think that if 0 is contained, that feature should be removed

sigma2_eps2 <- as.numeric(get_variance_residual(Lmm2))
sigma2_eps2
sigma2_b2 <- as.numeric(get_variance_random(Lmm2))
sigma2_b2

PVRE2 <- sigma2_b2/(sigma2_b2+sigma2_eps2)
PVRE2                                                 #The value seems incredibly high! 0.63



dotplot(ranef(Lmm2, condVar=T)) #95% CI
ranef(Lmm2, condVar=T)
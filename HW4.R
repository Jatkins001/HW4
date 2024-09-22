library(tidyverse)
library(caret)
library(class)

setwd('/users/jermaineatkins/Desktop/R/EconometricsFall24/ecob2000_lecture1')
load("Household_Pulse_data_ph4c2.RData")



Household_Pulse_data$SOCIAL2 <- fct_recode(Household_Pulse_data$SOCIAL2,
                                              "1" = "never",
                                              "2" = "rarely",
                                              "3" = "sometimes",
                                              "4" = "usually",
                                              "5" = "always lonely")


Household_Pulse_data$SOCIAL2 <- as.numeric(levels(Household_Pulse_data$SOCIAL2))[Household_Pulse_data$SOCIAL2]

#2ND VARIABLE

Household_Pulse_data$income <- fct_recode(Household_Pulse_data$INCOME,
                                          "0" = "NA", "1" = "HH income less than $25k",
                                          "1" = "HH income $25k - $34.9k", 
                                          "2" = "HH income $35k - 49.9", 
                                          "3" = "HH income $50k - 74.9",
                                          "4" = "HH income $75 - 99.9", 
                                          "5" = "HH income $100k - 149", 
                                          "6" = "HH income $150 - 199", 
                                          "7" = "HH income $200k +")
na.omit(Household_Pulse_data$income)

Household_Pulse_data$income <- as.numeric(levels(Household_Pulse_data$income))[Household_Pulse_data$income]

xtabs(formula = ~HADCOVIDRV + LONGCOVID, data = Household_Pulse_data)

dat_hadcovid <- Household_Pulse_data %>% filter(HADCOVIDRV == "yes tested + or doc told had Covid")     

na.omit(dat_hadcovid)
                        
summary(dat_hadcovid)        

norm_varb <- function(X_in) {
  (X_in - min(X_in, na.rm = TRUE))/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE)  )
}

data_use_prelim <- data.frame(norm_varb(dat_hadcovid$SOCIAL2),norm_varb(dat_hadcovid$income))

good_obs_data_use <- complete.cases(data_use_prelim,dat_hadcovid$LONGCOVID)
dat_use <- subset(data_use_prelim,good_obs_data_use)
y_use <- subset(dat_hadcovid$LONGCOVID,good_obs_data_use)
                                           
set.seed(1234567)
NN_obs <- sum(good_obs_data_use == 1)
select1 <- (runif(NN_obs) < 0.8)
train_data <- subset(dat_use,select1)
test_data <- subset(dat_use,(!select1))
cl_data <- y_use[select1]
true_data <- y_use[!select1]       


summary(cl_data)
summary(train_data)

for (indx in seq(1, 7, by= 2)) {
  pred_y <- knn3Train(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
  num_correct_labels <- sum(pred_y == true_data)
  correct_rate <- num_correct_labels/length(true_data)
  print(c(indx,correct_rate))
}
                                             
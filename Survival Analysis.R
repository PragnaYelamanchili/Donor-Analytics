setwd("C:/Users/pragn/Desktop/ABA Project")
data <- read.csv("final_dataset_Edited.csv")
head(data)

colnames(data)

data <- data[complete.cases(data),]
install.packages("survival", repos = "https://cran.r-project.org")
devtools::install_github("sachsmc/ggkm") # We need this for plotting
library(survival)

library(ggplot2)
install.packages("prettyunits")
library(prettyunits)
install.packages("ggkm",repos = "https://cran.r-project.org")
library(ggkm)
attach(data)

typeof(data)
data_en <- data.frame(data)
library(caret)
library(data.table)
install.packages("mltools")
library(mltools)


data_enc <- one_hot(as.data.table(data_en))
dmy <- dummyVars(" ~ .", data = data_en)
enc_data <- data.frame(predict(dmy, newdata = data_en))
summary(data)
data$dummy <- 1
colnames(data)
typeof(data)
install.packages("remotes")
remotes::install_github("michaelway/ggkm")
install.packages("ggquickeda")
library(ggquickeda)
data= as.data.frame(data)
typeof(data)
surv_object <-Surv(Ttl_Years_of_Donations,Recurring_Donor__c)
km1 <- survfit(surv_object~1)
summary(km1)

km2 <- survfit(surv_object~Recurring_Donor_Frequency__c)
summary(km2)
plot(km3, xlab = "time", ylab = "Survival Probability")
ggplot(data, aes(time = Ttl_Years_of_Donations, status = Recurring_Donor__c, color = factor(Recurring_Donor_Frequency__c))) + geom_km()
km3 <- survfit(surv_object~Donor_Type__c)
summary(km3)
ggplot(data, aes(time = Ttl_Years_of_Donations, status = Donor_Type__c, color = factor(Donor_Type__c))) + geom_km()


install.packages("ggfortify")
library(ggfortify)
autoplot(km1)
autoplot(km2)
autoplot(km3)
autoplot(km3, surv.linetype = 'dashed', conf.int = FALSE,
         censor.shape = '*', censor.size = 5, facets = TRUE, ncol = 2)
autoplot(km2, surv.linetype = 'dashed', conf.int = FALSE,
         censor.shape = '*', censor.size = 5, facets = TRUE, ncol = 2)
autoplot(aareg(Surv(Ttl_Years_of_Donations, Recurring_Donor__c) ~ Donor_Type__c, data = data))
install.packages("survminer")
library("survminer")
ggsurvplot(km2, data = data)

km4 = survfit(Surv(Ttl_Years_of_Donations, Recurring_Donor__c) ~ Donor_Type__c, data = data)
summary(km4)
ggsurvplot(km4)

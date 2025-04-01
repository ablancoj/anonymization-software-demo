library(readxl)
library(sdcMicro)
library(tidyverse)

data <- read_excel("sdcTutorial_data.xlsx", sheet="settlementdata")

str(data)
table(data$hhh_ethnolinguistic_group,data$hhh_marital_status)
hist(data$hhh_age)

# Choose QIs
selectedKeyVars <- c('district', 'hhh_ethnolinguistic_group','hhh_marital_status', 'hhh_age', 'hhh_gender','hhh_disability', 'total_members')
selectedWeights <- c('weights')

# Categoricals as factors
cols <- c('district', 'hhh_ethnolinguistic_group','hhh_marital_status', 'hhh_gender','hhh_disability')
data[,cols] <- lapply(data[,cols], factor)

## Create Subset of Data File 
subVars <- c(selectedKeyVars, selectedWeights)
subData <- data[,subVars]

## Create sdcMicro object
objSDC <- createSdcObj(dat=subData, keyVars = selectedKeyVars, weightVar = selectedWeights)

## View individual risks
individual_risk <- objSDC@risk$individual
indRisk_table <- cbind(subData,individual_risk)
View(indRisk_table)
hist(objSDC@risk$individual[,"risk"])

## print K-anonimity 
print(objSDC, type="kAnon")

## Print global risk
print(objSDC, "risk")

## Recode Age Variable 
objSDC <- globalRecode(objSDC, column = c('hhh_age'), breaks = 10 * c(0:10))
barplot(table(objSDC@manipKeyVars$hhh_age))

## Recode Total Members Variables
objSDC <- globalRecode(objSDC, column = c('total_members'), breaks = 5 * c(0:5))
barplot(table(objSDC@manipKeyVars$total_members))

## Reevaluate risks
print (objSDC,"risk")

## Local suppression to achieve 3-anonymity
objSDC <- localSuppression(objSDC, k = 3, importance = NULL) 

print(objSDC,"risk")

table(objSDC@origData[, c('hhh_ethnolinguistic_group')])
table(objSDC@manipKeyVars[, c('hhh_ethnolinguistic_group')])

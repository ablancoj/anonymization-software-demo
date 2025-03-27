# loading the sdcMicro package

install.packages("sdcMicro")

library(sdcMicro) 
library(tidyverse)

wd <- "../data"
setwd(wd)
file_name <- "adult.csv"

# Read file, convert strings to Factor
file <- read.csv(file_name, strip.white = TRUE, na.strings = "?", stringsAsFactors = TRUE)
# Remove any rows with NA
file <- file[complete.cases(file),]
str(file)
names(file)

# Drop unwanted columns
selectedVars = c(1,2,3,4,6,7,8,9,10,11,12,13,14,15)
file <- file[,selectedVars]
str(file)

# Choose attribute types
selectedKeyVars <- c("age", "workclass", "education", "marital.status", "occupation", "relationship", "race", "sex", "native.country")
selectedNumVars <- c("capital.gain", "capital.loss", "hours.per.week")
selectedPramVars <- c("native.country")
weightVar <- "fnlwgt"
selectedSensibleVar = c("class")

# Create the SDC Object
?createSdcObj
sdc <- createSdcObj(dat = file,
                    keyVars = selectedKeyVars,
                    numVars = selectedNumVars,
                    pramVars = selectedPramVars,
                    #weightVar = weightVar,
                    sensibleVar = selectedSensibleVar)

##################
# Object summary #
##################

sdc

#####################################
# Risk report according to key vars #
#####################################

sdc@risk

# Global risk
sdc@risk$global

# Individual Risk
hist(sdc@risk$individual[,"risk"])

# Sample frequency
hist(sdc@risk$individual[,"fk"])

# Population frequency
hist(sdc@risk$individual[,"Fk"])

sdc@origData %>% mutate(sdc@risk$individual) %>% View()

# Individuals w/ higher risk than some threshold
sum(sdc@risk$individual[,"risk"] > 0.5)

#############################
# k-anonimity (x | fk(x)<k) #
#############################

print(sdc, "kAnon")

# k-anonymity for k=10
k <- 10
sum(sdc@risk$individual[,"fk"] < k)
sum(sdc@risk$individual[,"fk"] < k)/dim(sdc@origData)[1]


#######################################
# l-diversity for sensitive variables #
#######################################

?ldiversity
sdc <- ldiversity(sdc, ldiv_index = c("class"))
sdc@risk$ldiversity

sdc@origData %>% mutate(sdc@risk$individual[,"fk"]) %>% mutate(sdc@risk$ldiversity[,"class_Distinct_Ldiversity"]) %>% View()

###################
# Special uniques #
###################
sdc <- suda2(sdc)
hist(sdc@risk$suda2$score)
hist(sdc@risk$suda2$disScore)
sdc@risk$suda2
plot(density(sdc@risk$suda2$disScore))

##############################
# Risk for numeric variables #
##############################

# Disclosure risk by distance-based record linkage (for numeric values)
dRisk(sdc@origData[,selectedNumVars], xm=sdc@manipNumVars[,selectedNumVars], k=0.1)

# Outlier detection
perc90 <- quantile(file[,"capital.gain", 0.90])
boxplot(sdc@origData$capital.gain, sdc@origData$capital.loss, sdc@origData$hours.per.week)

# IQR Rule
sum(sdc@origData$capita.gain > mean(sdc@origData$capital.gain) + 1.5 * IQR(sdc@origData$capital.gain))

# Household risk
# sdc@risk$global$hier_risk
# sdc@risk$global$hier_risk_ER

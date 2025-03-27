# loading the sdcMicro package

install.packages("sdcMicro")

library(sdcMicro) 
library(tidyverse)

rm(list = ls()) # to clean out workspace

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

#############################
# Remove direct identifiers #
#############################

?removeDirectID
#sdc <- removeDirectID(sdc, var="age")


############################
# Non perturbative methods #
############################

###########################
## Top and bottom coding ##
###########################
?topBotCoding

summary(sdc@origData$age)
sdc <- topBotCoding(sdc, column = "age", value = 65, replacement = 65, kind = "top")
sdc <- topBotCoding(sdc, column = "age", value = 18, replacement = 18, kind = "bottom")
summary(sdc@manipKeyVars$age)


###################
# Global recoding #
###################

## Numeric variables to ranges
?globalRecode

hist(sdc@manipKeyVars$age)
sdc <- globalRecode(sdc, column=c("age"), breaks=c(0, 18, 25, 35, 45, 55, 65))
barplot(table(sdc@manipKeyVars$age))
sdc@risk

## Global recoding for categorical attributes
?groupAndRename

### Native country
barplot(table(sdc@origData$native.country))
uniqueCountries <- unique(sdc@origData$native.country)

table(sdc@origData$native.country)
sdc <- groupAndRename(sdc, var = c("native.country"),
                      before = uniqueCountries[uniqueCountries != "United-States"],
                      after = c("Non-USA"))
unique(sdc@manipKeyVars$native.country)
barplot(table(sdc@manipKeyVars$native.country))

### Workclass
table(sdc@origData$workclass)
sdc <- groupAndRename(sdc, var = c("workclass"),
                      before = c("Federal-gov", "Local-gov", "State-gov"),
                      after = c("Government"))
sdc <- groupAndRename(sdc, var = c("workclass"),
                      before = c("Private", "Self-emp-inc", "Self-emp-not-inc"),
                      after = c("Private"))
sdc <- groupAndRename(sdc, var = c("workclass"),
                      before = c("Never-worked", "Without-pay"),
                      after = c("Unemployed"))
table(sdc@manipKeyVars$workclass)

### Education
table(sdc@manipKeyVars$education)
sdc <- groupAndRename(sdc, var = c("education"),
                      before = c("Preschool", "1st-4th", "5th-6th"),
                      after = c("Elementary"))
sdc <- groupAndRename(sdc, var = c("education"),
                      before = c("7th-8th", "9th", "10th", "11th", "12th", "HS-grad"),
                      after = c("HS"))
sdc <- groupAndRename(sdc, var = c("education"),
                      before = c("Some-college", "Assoc-acdm", "Assoc-voc", "Bachelors"),
                      after = c("College"))
sdc <- groupAndRename(sdc, var = c("education"),
                      before = c("Doctorate", "Masters", "Prof-school"),
                      after = c("Graduate"))
table(sdc@manipKeyVars$education)

### Marital status
table(sdc@manipKeyVars$marital.status)
sdc <- groupAndRename(sdc, var = c("marital.status"),
                      before = c("Married-AF-spouse", "Married-civ-spouse"),
                      after = c("Married"))
sdc <- groupAndRename(sdc, var = c("marital.status"),
                      before = c("Divorced", "Married-spouse-absent", "Never-married", "Separated", "Widowed"),
                      after = c("Spouse-absent"))
table(sdc@manipKeyVars$marital.status)


## Rounding
### Rounding can be applied directly to the manipXVars

rounding.base <- 5
sdc@manipNumVars[, "hours.per.week"] <- round(sdc@manipNumVars[, "hours.per.week"] / rounding.base) * rounding.base
sdc <- calcRisks(sdc)


#######################
## Local suppression ##
#######################

### Local suppression to achieve k-anonymity, without importance vector
?localSuppression
sdc <- localSuppression(sdc, k=2)
print(sdc, "ls")
sdc <- undolast(sdc)

### Local suppression to achieve k-anonymity, with importance vector
sdc <- localSuppression(sdc, k=2, importance=c(2,4,3,5,1,5,1,1,3))
print(sdc, "ls")
sdc <- undolast(sdc)

### The all-m approach
#### Apply local suppression to achieve 5-anonymity for all subsets of 3
#### QIs, then to all subsets of 9 QIs (all keys)
sdc <- localSuppression(sdc, k=5, combs=c(3, 9))
print(sdc, "ls")
sdc <- undolast(sdc)

#### Apply local suppression to achieve 5-anonymity for all subsets of 3
#### QIs, then 3-anonymity to all subsets of 9 QIs (all keys)
sdc <- localSuppression(sdc, k=c(5,3), combs=c(3, 9))
print(sdc, "ls")
sdc <- undolast(sdc)

#### Local suppression on 1 variable to reduce risk
?localSupp
hist(sdc@risk$individual[,"risk"])
plot(density(sdc@risk$individual[,"risk"]))
sdc <- localSupp(sdc, threshold = 0.1, keyVar = "native.country")
print(sdc, "ls")
sdc
hist(sdc@risk$individual[,"risk"])
plot(density(sdc@risk$individual[,"risk"]))

########################
# Perturbative methods #
########################

##########
## PRAM ##
##########

?pram

## Set a seed for reproducibility
set.seed(5474572467824)

## PRAM Applied to pramKeys
sdc <- pram(sdc)
print(sdc, "pram")

## PRAM w/ minimum probabilities in the diagonal
sdc <- pram(sdc, pd=0.8)
print(sdc, "pram")
sdc <- pram(sdc, pd=c(0.8, 0.5))
print(sdc, "pram")

attr(sdc, "pram_params")

## PRAM within strata (prevent impossible combinations)
sdc <- pram(sdc, strata_variables = ("race"))
print(sdc, "pram")

######################
## Microaggregation ##
######################

?microaggregation

### Univariate microaggregation with aggregation functions mean and median
sdc <- microaggregation(sdc, variables = "capital.gain", aggr = 5, method = mafast, measure = "mean")
sdc <- microaggregation(sdc, variables = "capital.gain", aggr = 5, method = mafast, measure = "median")

### Multivariate microaggregation
sdc <- microaggregation(sdc, variables = selectedNumVars, aggr = 5, method = "mdav", measure = "mean")

### Multivariate microaggregation within strata
sdc <- microaggregation(sdc, variables = selectedNumVars, aggr = 5, method = "mdav", strata_variables = "education", measure = "mean")

####################
## Noise addition ##
####################

?addNoise
### Set seed for reproducibility
set.seed(486135698367)

### Uncorrelated additive noise addition
sdc <- addNoise(sdc, variables = selectedNumVars, noise = 0.1, method = "additive")

### Uncorrelated additive noise addition
sdc <- addNoise(sdc, variables = selectedNumVars, noise = 0.1, method = "multiplicative")

### Correlated additive noise addition
sdc <- addNoise(sdc, variables = selectedNumVars, noise = 0.1, method = "correlated2")

### Noise addition to outliers only
sdc <- addNoise(sdc, variables = selectedNumVars, noise = 0.5, method = "outdect")

###################
## Rank swapping ##
###################

?rankSwap
### Set seed for reproducibility
set.seed(486135698367)

# Univariate rank swapping within 5% range
# Top and Bottom percentiles are Top and Bottom coded
sdc <- rankSwap(sdc, variables = "capital.loss", 
                TopPercent = 5,
                BottomPercent = 5,
                P=0.05)

# Multivariate rank swapping preserves correlations
sdc <- rankSwap(sdc, variables = selectedNumVars, 
                TopPercent = 5,
                BottomPercent = 5,
                P=0.05)




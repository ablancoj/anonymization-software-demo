library(sdcMicro)
library(tidyverse)

#############
# LOAD DATA #
#############

setwd("../data")
file <- read.csv("synthdata.csv")
View(file)

file$sex <- as.factor(file$sex)
file$residence <- as.factor(file$residence)
file$country.birth <- as.factor(file$country.birth)

barplot(file$age)
barplot(table(file$sex))
barplot(table(file$residence))
barplot(table(file$country.birth))
hist(file$income)


####################
# BUILD SDC OBJECT #
####################

keyVars <- c("age", "sex", "residence", "country.birth")
numVars <- c("income")

sdc <- createSdcObj(file,
                    keyVars = keyVars,
                    numVars = numVars)

#summary
print(sdc)

##############
# PRINT RISK #
##############

# histograms of risk and frequency
hist(sdc@risk$individual[,"risk"], breaks=50, main="Risk of unprotected data.")
hist(sdc@risk$individual[,"fk"], breaks=50, main="Frequencies of keys.")

# k-anonymity
print(sdc, 'kAnon')

# suda
sdc <- suda2(sdc)
sdc@risk$suda2

# risk summary
dRiskRMD(sdc)

# Numeric variables risk
summary(sdc@origData$income)
boxplot(sdc@origData$income)

# Check for outliers
incomeMean <- mean(sdc@origData$income)
incomeIQR <- IQR(sdc@origData$income)
sum(sdc@origData$income > incomeMean + 2*incomeIQR)
sum(sdc@origData$income < incomeMean - 2*incomeIQR)

##########################
# GLOBAL RECODING OF AGE #
##########################

barplot(table(sdc@origData$age))
sdc <- globalRecode(sdc, column=c("age"), breaks=c(0, 25, 35, 45, 55, 65, 84))
barplot(table(sdc@manipKeyVars$age))

################################
# GLOBAL RECODING OF RESIDENCE #
################################

ca_prov <- read.csv("ca_prov.csv", header = TRUE, sep = ",")
View(ca_prov)
cas <- unique(ca_prov[,"CA"])

barplot(table(file$residence))
for (ca in cas) {
  provs <- ca_prov[ca_prov[,"CA"]==ca,"Prov"]
  sdc <- groupAndRename(sdc, var = "residence",
                        before = provs,
                        after = ca)
}
barplot(table(sdc@manipKeyVars$residence))

##########################
# GLOBAL RECODING OF COB #
##########################

ctax <- read.csv("country_tax.csv", sep = ";")
View(ctax)

l2 <- unique(ctax[,"L2"])

barplot(table(file$country.birth))
for (region in l2) {
  countries <- ctax[ctax[,"L2"]==region,"L3"]
  countries <- intersect(countries, unique(sdc@origData$country.birth))
  if (length(countries) > 0) {
    sdc <- groupAndRename(sdc, var = "country.birth",
                          before = countries,
                          after = region)
  }
}
barplot(table(sdc@manipKeyVars[,"country.birth"]))

#####################
# LOCAL SUPPRESSION #
#####################

sdc <- localSuppression(sdc, k=c(5,3), combs=c(2, 4))
sdc <- undolast(sdc)
sdc <- localSuppression(sdc, k=5, combs=c(2, 4))
sdc


##############
# TOP CODING #
##############

sdc <- topBotCoding(sdc, column = "income", 
                    value = round(incomeMean + 2*incomeIQR),
                    replacement = round(incomeMean + 2*incomeIQR),
                    kind = "top")

# Numeric variables risk
summary(sdc@manipNumVars$income)
boxplot(sdc@manipNumVars$income)
dRisk(sdc)

#################
# RANK SWAPPING #
#################

set.seed(123123)
sdc <- rankSwap(sdc, variables = c("income"), P=10)


dRisk(sdc)
dUtility(sdc)
print(sdc, "risk")

################
# EXTRACT DATA #
################

fileAnonymized <- extractManipData(sdc,randomizeRecords = "simple")
View(fileAnonymized)
write.csv(fileAnonymized, file = "synthdataAnon.csv", quote = FALSE, sep = ",", row.names = FALSE, fileEncoding = "UTF-8")

fanon <- cbind(sdc@manipKeyVars, sdc@manipNumVars)
View(fanon)

###################
# GENERATE REPORS #
###################

wd <- "C:/Users/Usuario/Dropbox/courses/ISGlobal_Anonymization/contents/synthdataReports"
report(sdc, internal = F, outdir = wd, filename = "External-Report")
report(sdc, internal = T, outdir = wd, filename = "Internal-Report")


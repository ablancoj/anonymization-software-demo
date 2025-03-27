################################
# loading the sdcMicro package #
################################

install.packages("sdcMicro")
rm(list = ls())  # to clean out workspace
library(sdcMicro) 

############################
# Some supported Filetypes #
############################

# Type/software   Extension Package	Function
# SPSS            .sav      haven	  read_sav()
# STATA (v. 5-14) .dta      haven	  read_dta()
# SAS             .sas7bdat haven	  read_sas()
# Excel           .csv      utils   read.csv()
# Excel           .xls(x)   readxl	readxl()
# Weka            .arff     farff   readARFF()
# Argus           .asc      gdata   read.fwf()


#########################
# Set working directory #
#########################

wd <- "../data"
setwd(wd)
file_name <- "adult.csv"

#############
# Load data #
#############

?read.csv
file <- read.csv(file_name, strip.white = TRUE, na.strings = "?")

# Data has to be of data.frame class
# file <- as.data.frame(file)
class(file)

# In case the file uses specific different codes for missing values,
# they should be recoded to NA
# file[file[, "some_attr"] == "some_value", "some_attr"] <- NA

# Categorical attributes need to be converted to factors
for (n in names(file)) {
  if (class(file[,n]) == "character") {
    file[,n] <- as.factor(file[,n])
  }
}

str(file)
View(file)


################################
# Select variables of interest #
################################

selectedVars = c(1,2,3,4,6,7,8,9,10,11,12,13,14,15)
file <- file[,selectedVars]
str(file)

# Remove records with NA in native.country
file <- file[!is.na(file$native.country),]
str(file)

##########################
# Choose attribute types #
##########################

# QIs
selectedKeyVars <- c("age", "workclass", "education", "marital.status", "occupation", "relationship", "race", "sex", "native.country")

# Numerical QIs
selectedNumVars <- c("capital.gain", "capital.loss", "hours.per.week")

# Attributes amenable for PRAM
# Often we want to select attributes which could lead
# to spontaneous re-identification (e.g. hair color)
selectedPramVars <- c("native.country")

# Sample weight
weightVar <- "fnlwgt"

# Sensitive attributes
selectedSensibleVar = c("class")

########################
# Build the SDC Object #
########################

# All methods take an sdc object as input and return
# a modified sdc object

?createSdcObj
sdc <- createSdcObj(dat = file,
                    keyVars = selectedKeyVars,
                    numVars = selectedNumVars,
                    pramVars = selectedPramVars,
                    weightVar = weightVar,
                    sensibleVar = selectedSensibleVar)

##########################
# Explore the sdc object #
##########################

# Names of slots
slotNames(sdc)

# Accessing risk measures
sdc@risk
hist(sdc@risk$individual[,1])

# Printing the sdc object provides a summary of risk
# and utility measures (will be updated when using sdc methods)
print(sdc)

# Print function for relevant measures
print(sdc, "risk")
print(sdc, "kAnon")
print(sdc, "ls")
print(sdc, "pram")

###########################
# Applying an sdc methods #
###########################

?localSuppression
sdc <- localSuppression(sdc, k=2)

# sdc methods recompute risk
# Reevaluate risk
hist(sdc@risk$individual[,1])

# Undo last sdc operation
?undolast
sdc <- undolast(sdc)

############################
# Directly manipulate data #
############################

# The sdc object stores the original data @origData
# And the modified data in @manipKeyVars, @manipNumVars...

hist(sdc@manipKeyVars$age)
sdc@manipKeyVars$age[sdc@manipKeyVars$age >= 0 & sdc@manipKeyVars$age < 1] <- 0
sdc@manipKeyVars$age[sdc@manipKeyVars$age >= 15 & sdc@manipKeyVars$age < 25] <- 20
sdc@manipKeyVars$age[sdc@manipKeyVars$age >= 25 & sdc@manipKeyVars$age < 35] <- 30
sdc@manipKeyVars$age[sdc@manipKeyVars$age >= 35 & sdc@manipKeyVars$age < 45] <- 40
sdc@manipKeyVars$age[sdc@manipKeyVars$age >= 45 & sdc@manipKeyVars$age < 55] <- 50
sdc@manipKeyVars$age[sdc@manipKeyVars$age >= 55 & sdc@manipKeyVars$age < 66] <- 60
sdc@manipKeyVars$age[sdc@manipKeyVars$age >= 65] <- 65
hist(sdc@manipKeyVars$age)

# If data is directly manipulated, risks have to be recomputed
?calcRisks
calcRisks(sdc)

?pram
sdc <- pram(sdc)

###########################
# Extract anonymized data #
###########################

?extractManipData
fileAnonymized <- extractManipData(sdc)


### Household structure
# HHVars <- c(...)
# INDVars <- c(...)
# fileIND <- c(...)
# fileHH <- file[, HHVars]
# fileHH <- unique(fileHH, by = c("HID"))
# sdcHH <- createSdcObj(fileHH, ...)
# HHmanip <- extractManipData(sdcHH)
# fileIND <- file[, INDVars]
# fileCombined <- merge(HHManip, fileIND, by = c("IND"))
# ?selectHouseholdData
# ?mergeHouseholdData


####################
# Generate reports #
####################
?report
report(sdc, internal = F, outdir = wd, filename = "External-Report")
report(sdc, internal = T, outdir = wd, filename = "Internal-Report")


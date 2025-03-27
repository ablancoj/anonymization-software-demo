###########################################################
# Sample synthesis with only a modest number of variables #
###########################################################

install.packages("synthpop")
rm(list = ls())                # to clean out workspace
library(synthpop)              # to load package

######################
# Load observed data #
######################

wd <- "../data"
setwd(wd)
file_name <- "adult.csv"

# Read file, convert strings to Factor
file <- read.csv(file_name, strip.white = TRUE, na.strings = "?", stringsAsFactors = TRUE)

# Choose wanted variables
mydata <- file[, c(1:2, 4, 6:10, 13:15)] 

# Explore data
cb <- codebook.syn(mydata)
cb$tab
cb$labs

###################
# Synthesize data #
###################

?syn
mysyn <- syn(mydata)  # default synthesis 
summary(mysyn)

names(mysyn)
mysyn$method
mysyn$predictor.matrix
mysyn$visit.sequence
mysyn$cont.na


###########################
# Evaluate synthetic data #
###########################

?compare
compare(mysyn, mydata, stat = "counts")

?multi.compare
multi.compare(mysyn, mydata, var = "age", by = "sex", cont.type = "boxplot")
multi.compare(mysyn, mydata, var = "hours.per.week", by = "sex", cont.type = "hist")

multi.compare(mysyn, mydata, var = "marital.status", by = "sex")
multi.compare(mysyn, mydata, var = "class", by = "sex")
multi.compare(mysyn, mydata, var = "class", by = "education", cont.type = "boxplot")
multi.compare(mysyn, mydata, var = "class", by = c("sex", "education"))

?utility.tab
utility.tab(mysyn, mydata, c("age","sex"))
utility.tab(mysyn, mydata, c("sex","class"))

?utility.gen
utility.gen(mysyn, mydata, print.zscores = TRUE)

realfit <- glm(class ~ age + sex + education, data = mydata, family = "binomial")  
summary(realfit)
synthfit <- glm(class ~ age + sex + education, data = mysyn$syn, family = "binomial")  
summary(synthfit)

synfit.synds1 <- glm.synds(class ~ age + sex + education, data = mysyn, family = "binomial")
compare(synfit.synds1, mydata)

# Export to SPSS
write.syn(mysyn, filename = "mysyn", filetype = "SPSS")

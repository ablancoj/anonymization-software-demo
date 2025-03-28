###########################################################
# Sample synthesis with only a modest number of variables #
###########################################################

install.packages("synthpop")
rm(list = ls())                # to clean out workspace
library(synthpop)              # to load package

######################
# Load observed data #
######################

getwd()
wd <- "../data"
setwd(wd)
file_name <- "adult.csv"

# Read file, convert strings to Factor
file <- read.csv(file_name, strip.white = TRUE, na.strings = "?", stringsAsFactors = TRUE)
summary(file)
names(file)

# Choose wanted variables
mydata <- file[, c(1:2, 4, 6:8, 10, 13, 15)] 

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

mysyn2 <- syn(mydata, visit.sequence = c(1, 7, 4, 6, 3, 5, 2, 8, 9))
names(mysyn2)
mysyn2$method
mysyn2$predictor.matrix
mysyn2$visit.sequence
mysyn2$cont.na


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

# 2nd synthesis

?compare
compare(mysyn, mydata, stat = "counts")

?multi.compare
multi.compare(mysyn2, mydata, var = "age", by = "sex", cont.type = "boxplot")
multi.compare(mysyn2, mydata, var = "hours.per.week", by = "sex", cont.type = "hist")

multi.compare(mysyn2, mydata, var = "marital.status", by = "sex")
multi.compare(mysyn2, mydata, var = "class", by = "sex")
multi.compare(mysyn2, mydata, var = "class", by = "education", cont.type = "boxplot")
multi.compare(mysyn2, mydata, var = "class", by = c("sex", "education"))

?utility.tab
utility.tab(mysyn2, mydata, c("age","sex"))
utility.tab(mysyn2, mydata, c("sex","class"))

?utility.gen
utility.gen(mysyn2, mydata, print.zscores = TRUE)

synthfit2 <- glm(class ~ age + sex + education, data = mysyn2$syn, family = "binomial")  
summary(synthfit2)

synfit.synds2 <- glm.synds(class ~ age + sex + education, data = mysyn2, family = "binomial")
compare(synfit.synds2, mydata)



# Export to SPSS
?write.syn
write.syn(mysyn, filename = "mysyn", filetype = "csv")

library(tidyverse)

set.seed(18032025)

setwd("C:/Users/Usuario/Dropbox/courses/ISGlobal_Anonymization/data")
ca_prov <- read.csv("ca_prov.csv", header = TRUE, sep = ",")
cob <- read.csv("cob.csv", header = TRUE, sep = ",")
age_prob <- c(13.84, 15.80, 19.91, 23.69, 16.74, 10.02) / 100
age_prob <- rep(age_prob/14, each = 14)

n_samples <- 20000

age <- sample(18:84, size = n_samples, replace = TRUE, prob = age_prob[18:84])
sex <- sample(c("Male", "Female"), size = n_samples, replace = TRUE)
residence <- sample(ca_prov$Prov, size = n_samples, replace = TRUE, prob = ca_prov$Pob/sum(ca_prov$Pob))
country.birth <- sample(cob$País.de.Nacimiento, size = n_samples, replace = TRUE, prob = cob$Total/sum(cob$Total))
income <- round(rlnorm(n_samples, meanlog = log(22000), sdlog = 0.6)/1000)*1000


data <- data.frame(age, sex, residence, country.birth, income)
data$sex <- as.factor(data$sex)
data$residence <- as.factor(data$residence)
data$country.birth <- as.factor(data$country.birth)
View(data)
write.table(data, "synthdata.csv", sep = ",", row.names = FALSE, quote = FALSE)


# Data preparation for Argus
install.packages("gdata")
library(gdata)

adult <- read.csv("adult.csv", sep = ",", strip.white = TRUE)
write.fwf(adult, "adult.asc", rownames = FALSE, na = ".", colnames = FALSE, justify = "right", sep = "", formatInfo = TRUE)
write.fwf(data, "synthdata.asc", rownames = FALSE, na = ".", colnames = FALSE, justify = "right", sep = "", formatInfo = TRUE)

?read.fwf

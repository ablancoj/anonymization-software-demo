library(sdcMicro)
wd <- "../data"
setwd(wd)
file_name <- "adult.csv"
adult <- read.csv(file_name, na.strings = " ?", stringsAsFactors = TRUE)
x <- sdcApp()

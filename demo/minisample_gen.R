library(gdata)

age <- sample(c(18:65), 10, replace = TRUE)
weight <- round(rnorm(10, mean = 70, sd = 20))
sbp <- round(rnorm(10, mean = 120, sd = 10))
aids <- sample(c(0,1), 10, replace = TRUE)

data <- data.frame(age, weight, sbp, aids)
View(data)

setwd("../data")
write.csv(data, "minisample.csv", 
          quote = FALSE, 
          sep = ",", 
          row.names = FALSE, 
          col.names = TRUE)

info <- write.fwf(data, "minisample.asc", 
                  quote = FALSE, sep = "",  
                  rownames = FALSE, 
                  colnames = FALSE, 
                  justify = "right", 
                  formatInfo = TRUE)

write.table(info, "minisample.info")

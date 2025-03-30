install.packages("DPpack")
library(DPpack)
library(tidyverse)
setwd("../data")

file <- read.csv("synthdata.csv")
View(file)

D <- file[,1]
n <- length(D)
c0 <- min(D)
c1 <- max(D)

#########################
# Generic DP Mechanisms #
#########################

# Queries using the Laplace mechanism
?LaplaceMechanism

# Mean query
private.mean <- LaplaceMechanism(mean(D), eps = 1, sensitivities = (c1-c0)/n)
cat("Privacy preserving mean: ", private.mean, "\nTrue mean: ", mean(D))

# Sum query
private.sum <- LaplaceMechanism(sum(D), eps = 1, sensitivities = (c1-c0))
cat("Privacy preserving sum: ", private.sum, "\nTrue sum: ", sum(D))

# Count query
count <- file %>% filter(age < 30, residence == "Barcelona") %>% count()
private.count <- LaplaceMechanism(count[1,1],
                                  eps = 1,
                                  sensitivities = 1)
cat("Privacy preserving count: ", private.count, "\nTrue sum: ", count[1,1])

# Multiple queries
f <- function(D) c(mean(D), var(D))
# Here, privacy budget is split
private.vals <- LaplaceMechanism(f(D), eps = 1, sensitivities = c((c1-c0)/n, (c1-c0)^2/n))
cat("Privacy preserving values: ", private.vals, "\nTrue values: ", f(D))

# Here, privacy budget is split so that 25% is given to the mean and 75% is given to the variance
private.vals <- LaplaceMechanism(f(D), epsilon, sensitivities, alloc.proportions = c(0.25, 0.75))
cat("Privacy preserving values: ", private.vals, "\nTrue values: ", f(D))

# Queries using the Gaussian mechanism
# Approximate differential privacy
private.approx <- GaussianMechanism(mean(D), eps = 1, delta = 1/n, sensitivities = (c1-c0)/n)
cat("Privacy-preserving mean (approximate): ", private.approx, "\nTrue mean: ", mean(D))


# Exponential mechanism
barplot(table(file$residence))
factors <- names(table(file$residence))
counts <- as.numeric(table(file$residence))
mode <- factors[which.max(counts)]

private.mode <- ExponentialMechanism(utility = counts, eps = 1, sensitivity = 1, candidates = factors)
cat("Privacy-preserving mode (approximate): ", private.mode, "\nTrue mean: ", mode)


#######################
# Built-in DP queries #
#######################

# Mean
?meanDP
private.mean <- meanDP(D, eps = 1, lower.bound = 0, upper.bound = 116, mechanism = "Laplace")
cat("Privacy preserving mean: ", private.mean, "\nTrue mean: ", mean(D))

private.mean <- meanDP(D, eps = 1, lower.bound = min(D), upper.bound = max(D), mechanism = "Laplace")
cat("Privacy preserving mean: ", private.mean, "\nTrue mean: ", mean(D))

private.mean <- meanDP(D, eps = 0.5, delta = 1e-5, lower.bound = 0, upper.bound = 116, mechanism = "Gaussian")
cat("Privacy preserving mean: ", private.mean, "\nTrue mean: ", mean(D))

# Variance 
?varDP
private.var <- varDP(D, eps = 1, lower.bound = 0, upper.bound = 116, mechanism = "Laplace")
cat("Privacy preserving variance: ", private.var, "\nTrue variance: ", var(D))

# Standard deviation
?sdDP
private.sd <- sdDP(D, eps = 0.5, delta = 1e-5, lower.bound = 0, upper.bound = 116, mechanism = "Gaussian")
cat("Privacy preserving standard deviation: ", private.sd, "\nTrue standard deviation: ", sd(D))

# Covariance
?covDP
private.cov <- covDP(file$age, file$income, eps = 0.5,
                     lower.bound1 = min(file$age),
                     upper.bound1 = max(file$age),
                     lower.bound2 = min(file$income),
                     upper.bound2 = max(file$income),
                     mechanism = "Laplace")

cat("Privacy preserving covariance: ", private.cov, "\nTrue covariance: ", cov(file$age, file$income))


# Histogram
hist(D, main = "Non-private histogram")

?histogramDP
private.hist <- histogramDP(D, eps = 0.5, delta = 1e-5, 
                            lower.bound = min(D), upper.bound = max(D),
                            mechanism = "Gaussian") # Satisfies (0.5,10^-5)-DP
plot(private.hist, main = "Private histogram")

private.hist <- histogramDP(D, eps = 0.5,
                            lower.bound = min(D), upper.bound = max(D),
                            mechanism = "Laplace") # Satisfies (0.5,10^-5)-DP
plot(private.hist, main = "Private histogram")


# Quantiles
# Get 25th quantile satisfying 1-differential privacy
private.quantile <- quantileDP(D, quant = 0.25, eps = 1, lower.bound = min(D), upper.bound = max(D))
cat("Privacy preserving quantile: ", private.quantile, "\nTrue quantile: ", quantile(D, 0.25))

# Median
private.median <- medianDP(D, eps = 1, lower.bound = 0, upper.bound = 116)
cat("Privacy preserving median: ", private.median, "\nTrue median: ", median(D))




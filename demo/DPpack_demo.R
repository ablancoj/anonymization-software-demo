install.packages("DPpack")
library(DPpack)
??DPpack
setwd("../data")

file <- read.csv("synthdata.csv")
View(file)

D <- file[,1]
n <- length(D)
c0 <- min(D)
c1 <- max(D)

epsilon <- (c1-c0)/n # Privacy budget
sensitivity <- 116/n

private.mean <- LaplaceMechanism(mean(D), epsilon, sensitivity)
cat("Privacy preserving mean: ", private.mean, "\nTrue mean: ", mean(D))


f <- function(D) c(mean(D), var(D))
sensitivities <- c((c1-c0)/n, (c1-c0)^2/n)
epsilon <- 1 # Total privacy budget for f
# Here, privacy budget is split relative to the individual sensitivities
# of the sample mean and sample variance. Collectively, the computation
# satisfies 1-differential privacy.
private.vals <- LaplaceMechanism(f(D), epsilon, sensitivities)
cat("Privacy preserving values: ", private.vals, "\nTrue values: ", f(D))

# Here, privacy budget is split so that 25% is given to the mean
# and 75% is given to the variance
private.vals <- LaplaceMechanism(f(D), epsilon, sensitivities,
                                 alloc.proportions = c(0.25, 0.75))
cat("Privacy preserving values: ", private.vals, "\nTrue values: ", f(D))




delta <- 0.01
sensitivity <- (c1-c0)/n
# Approximate differential privacy
private.approx <- GaussianMechanism(mean(D), epsilon,
                                    delta, sensitivity)
cat("Privacy-preserving mean (approximate): ", private.approx,
    "\nTrue mean: ", mean(D))



candidates <- c("a", "b", "c", "d", "e") # Range of f
# Utility function values in same order as corresponding candidates
utility <- c(0, 1, 2, 1, 0)
epsilon <- 1 # Privacy budget
sensitivity <- 1
# Release privacy-preserving index of chosen candidate
idx <- ExponentialMechanism(utility, epsilon, sensitivity)
candidates[idx]

ExponentialMechanism(utility, epsilon, sensitivity,
                     candidates = candidates)





# Simulate a dataset
D <- rnorm(500, mean=3, sd=2)
lower.bound <- -3 # 3 standard deviations below mean
upper.bound <- 9 # 3 standard deviations above mean
# Get mean satisfying bounded 1-differential privacy
private.mean <- meanDP(D, 1, lower.bound, upper.bound)
cat("Privacy preserving mean: ", private.mean, "\nTrue mean: ", mean(D))
#> Privacy preserving mean: 2.872637
#> True mean: 2.857334
# Get variance satisfying unbounded approximate (0.5, 0.01)-DP
private.var <- varDP(D, 0.5, lower.bound, upper.bound,
                     which.sensitivity = "unbounded",
                     mechanism = "Gaussian", delta = 0.01)
cat("Privacy preserving variance: ", private.var,
    "\nTrue variance: ", var(D))

# Get std dev satisfying (0.5, 0.01)-DP
private.sd <- sdDP(D, 0.5, lower.bound, upper.bound,
                   mechanism="Gaussian", delta=0.01)
cat("Privacy preserving standard deviation: ", private.sd,
    "\nTrue standard deviation: ", sd(D))




# Simulate datasets
D1 <- sort(rnorm(500, mean=3, sd=2))
D2 <- sort(rnorm(500, mean=-1, sd=0.5))
lb1 <- -3 # 3 std devs below mean
lb2 <- -2.5 # 3 std devs below mean
ub1 <- 9 # 3 std devs above mean
ub2 <- .5 # 3 std devs above mean
# Covariance satisfying 1-differential privacy
private.cov <- covDP(D1, D2, 1, lb1, ub1, lb2, ub2)
cat("Privacy preserving covariance: ", private.cov,
    "\nTrue covariance: ", cov(D1, D2))



x <- rnorm(500) # Simulate dataset
hist(x, main = "Non-private histogram", ylim=c(0, 110), col="gray")
private.hist <- histogramDP(x, 1) # Satisfies (1,0)-DP
plot(private.hist, main = "Private histogram",
     ylim=c(0, 110), col="gray")



# Simulate a dataset
D <- rnorm(500)
lower.bound <- -3 # 3 standard deviations below mean
upper.bound <- 3 # 3 standard deviations above mean
quant <- 0.25
eps <- 1
# Get 25th quantile satisfying 1-differential privacy
private.quantile <- quantileDP(D, quant, eps, lower.bound, upper.bound)
cat("Privacy preserving quantile: ", private.quantile,
    "\nTrue quantile: ", quantile(D, 0.25))

# Get median requiring released value to be in dataset
private.median <- medianDP(c(1,0,3,3,2), eps, lower.bound = 0,
                           upper.bound = 4,
                           uniform.sampling = FALSE)
cat("Privacy preserving median: ", private.median,
    "\nTrue median: ", median(c(1,0,3,3,2)))




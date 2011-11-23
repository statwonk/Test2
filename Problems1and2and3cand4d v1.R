## Test 2
## Christopher Peters
## Reliability and Survival Anaylsis
## Luis A. Escobar
## Fall 2011


# Make some data for 1a)
tmp.time <- log.seq(1,20,length=100)
log.tmp.time <- log(tmp.time)
tmp.prob <- pnorm(log.tmp.time,mean=log(5),sd=1)

# Plot 1a)
probpaper(x.range=c(1,20), y.range=c(0.021,.989),  distribution="lognormal", grids = TRUE,ylab = "Probability", xlab = "Time", my.title="The title")
lines(log(tmp.time),qnorm(tmp.prob))

# Make some data for 1b)
tmp.time <- log.seq(1,20,length=100)
log.tmp.time <- log(tmp.time)
tmp.prob <- pnorm(log.tmp.time,mean=log(5),sd=2)

# Plot 1b)
probpaper(x.range=c(1,20), y.range=c(0.021,.989),  distribution="lognormal", grids = TRUE,ylab = "Probability", xlab = "Time", my.title="The title")
lines(log(tmp.time),qnorm(tmp.prob))

# Make some data for 2a)
tmp.time <- log.seq(1,20,length=100)
lon.tmp.time <- log(tmp.time)
tmp.prob <- psev(log.tmp.time, location = log(10), scale = 1)

# Plot 2a)
probpaper(x.range=c(1,20), y.range=c(0.021,.989),  distribution="weibull", grids = TRUE,ylab = "Probability", xlab = "Time", my.title="The title")
lines(log(tmp.time), qsev(tmp.prob))

# Make some data for 2b)
tmp.time <- log.seq(1,20,length=100)
lon.tmp.time <- log(tmp.time)
tmp.prob <- psev(log.tmp.time, location = log(10), scale = 2)

# Plot 2b)
probpaper(x.range=c(1,20), y.range=c(0.021,.989),  distribution="weibull", grids = TRUE,ylab = "Probability", xlab = "Time", my.title="The title")
lines(log(tmp.time), qsev(tmp.prob))

# Plot 3c)

# Create the data
problem4c <- as.data.frame(c(1, 2))
problem4c$Status <- c("Failure", 
                      "R-Censored")
names(problem4c) <- c("Time", "Status")

simple.contour(problem4c, "L")

# First weibull
tmp.time <- log.seq(1,100,length=100)
lon.tmp.time <- log(tmp.time)
tmp.prob <- psev(log.tmp.time, location = log(30), scale = 1)
probpaper(x.range=c(1,100), y.range=c(0.021,.989),  distribution="weibull", grids = TRUE,ylab = "Probability", xlab = "Time", my.title="The title")
lines(log(tmp.time), qsev(tmp.prob))

# Second weibull
tmp.time <- log.seq(1,100,length=100)
lon.tmp.time <- log(tmp.time)
tmp.prob <- psev(log.tmp.time, location = log(40), scale = 2)
probpaper(x.range=c(1,100), y.range=c(0.021,.989),  distribution="weibull", grids = TRUE,ylab = "Probability", xlab = "Time", my.title="The title")
lines(log(tmp.time), qsev(tmp.prob))


# Generating random Weibull numbers

first_weibull <- rweibull(1:100, 30, 1)
second_weibull <- rweibull(1:100, 40, 2)
first_weibull_cdf <- pweibull(first_weibull)

weibulls2 <- as.data.frame(first_weibull)
weibulls2$second_weibull <- second_weibull

write.csv(weibulls2, file = "C:\\Users\\Chris\\School\\EXST_Reliability_and_Survival_Analysis\\weibulls_prob4.csv")

hazard_num <- dweibull(first_weibull, 30, 1)*dweibull(second_weibull, 40, 2)

hazard_denom <- (-pweibull(first_weibull, 30, 1, lower = FALSE))*(-pweibull(second_weibull, 40, 2, lower = FALSE))
hazard <- hazard_num/hazard_denom

probpaper(x.range=c(1,100), y.range=c(0.021,.989),  distribution="weibull", grids = TRUE,ylab = "Probability", xlab = "Time", my.title="The title")
lines(log(tmp.time), qweibull(hazard))



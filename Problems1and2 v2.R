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
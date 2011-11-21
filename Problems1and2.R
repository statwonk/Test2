## Test 2
## Christopher Peters
## Reliability and Survival Anaylsis
## Luis A. Escobar
## Fall 2011
library(ggplot2)
library(e1071)
library(qAnalyst)

p <- c(0.01, 0.02, 0.05, 0.1, 0.2, 0.3, 0.4,
       0.5, 0.6, 0.7 0.8, 0.9, 0.95, 0.98)
# log(tp) = mu + inverse_norm(p)*sigma


## 1a.

mu <- log(5)
sigma <- 1

time <- mu + qlnorm(p, mean = mu, sd = sigma)

plot1.data.frame <- data.frame(time = time, tp = p)

probplot(plot1.data.frame$time, qdist = qlnorm, name = "warping")

probplot
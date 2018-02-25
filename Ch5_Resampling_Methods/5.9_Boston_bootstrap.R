# BOOTSTRAP WITH BOSTON HOUSING DATA SET

#------------------------------------------------------------------------------------------------------- 

# DATA AND VISUALIZATION

# call the MASS library and attach the data
library(MASS)
attach(Boston); names(Boston) # plus whatever you need

#------------------------------------------------------------------------------------------------------- 

# SAMPLE MEAN AND STANDARD ERROR OF VARIABLE MEDV

# sample mean
mu = mean(medv); mu
sigma = sd(medv)/sqrt(length(medv)); sigma

# need to run sigmahat.fn that returns the mean of a subsample determined by boot
boot(medv, sigmahat.fn, 10)

# compute the bootstrap confidence interval and compare with the usual one
mu + 2*0.4278641
mu - 2*0.4278641
t.test(Boston$medv)

#------------------------------------------------------------------------------------------------------- 

# EXAMINE FURTHER SAMPLE STATISTICS

# estimate the variance of the sample median using bootstrap
boot(medv, med.fn, 10)

# provide an estimate of the tenth percentile
mu01 = quantile(medv, probs = .1)
boot(medv, mu01.fn, 10)



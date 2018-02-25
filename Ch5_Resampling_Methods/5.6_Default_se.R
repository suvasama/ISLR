# COMPUTE THE STANDARD ERRORS USING BOOTSTRAP

#------------------------------------------------------------------------------------------------------- 

library(ISLR)
attach(Default)

# logistic regression on the entire data set
def.glm = glm(default ~ income + balance, family = binomial); 

#------------------------------------------------------------------------------------------------------- 

# BOOTSTRAPPING

# call the function boot.fn() that computes the coefficient estimates for the particular model and is located in the same direction
source("boot.fn.r")

set.seed(1)
library(boot)

# the following line may take a while for too many iteration rounds
boot(Default, boot.fn, 300)

# compare with the original standard errors
summary(def.glm)

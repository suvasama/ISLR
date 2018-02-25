# PERFORM CROSS VALIDATION ON SIMULATED DATA

#------------------------------------------------------------------------------------------------------- 

# Generate a simulated data set

set.seed(2)
x = rnorm(100)
y = x - 2*x^2 + rnorm(100)

# scatter plot of X against Y
plot(x,y)

#------------------------------------------------------------------------------------------------------- 

xy = data.frame(X = x, Y = y)

# fit the following models using least squares
y.glm = glm(Y ~ X, data = xy)
y2.glm = glm(Y ~ poly(X,2), data = xy)
y3.glm = glm(Y ~ poly(X,3), data = xy)
y4.glm = glm(Y ~ poly(X,4), data = xy)

# compute the LOOCV errors 
library(boot)
cv.err = cv.glm(xy, y.glm); print(cv.err$delta)
cv.err = cv.glm(xy, y2.glm); print(cv.err$delta)
cv.err = cv.glm(xy, y3.glm); print(cv.err$delta)
cv.err = cv.glm(xy, y4.glm); print(cv.err$delta)

# set a different seed and repeat

#------------------------------------------------------------------------------------------------------- 

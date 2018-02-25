# DEVELOP A MODEL TO PREDICT IF A CAR GETS HIGH OR LOW GAS MILEAGE

#------------------------------------------------------------------------------------------------------- 

# DATA AND VISUALIZATION

# call the ISLR library
library(ISLR)
attach(Auto)

# variable mpg as dummy variable; dataframe w/o mpg
mpg01 = as.numeric(mpg >= median(mpg))
autodata = Auto[-1]; 

# data visualization
for (x in autodata[-8]) {
	plot(mpg01,x)
}

for (x in autodata[-8]) {
	boxplot(mpg01,x)
}

#------------------------------------------------------------------------------------------------------- 

# SPLIT THE DATA INTO A TRAIN AND TEST SETS

# take 1/5 of the data to form a test set
n = floor(length(mpg01)/5); test = 1:n

testmpg = mpg01[test]; trainmpg = mpg01[-test]
testauto = autodata[test,]; trainauto = autodata[-test,]

#------------------------------------------------------------------------------------------------------- 

# LINEAR DISCRIMINANT ANALYSIS

library(MASS)
auto.lda = lda(trainmpg ~ cylinders + displacement + horsepower + weight + acceleration + year + origin, data = trainauto)
plot(auto.lda)

auto.pred = predict(auto.lda, testauto)
auto.class = auto.pred$class

table(auto.class, testmpg)
mean(auto.class == testmpg)

#------------------------------------------------------------------------------------------------------- 

# QUADRATIC DISCRIMINANT ANALYSIS

auto.qda = qda(trainmpg ~ cylinders + displacement + horsepower + weight + acceleration + year + origin, data = trainauto)

auto.pred = predict(auto.qda, testauto)
auto.class = auto.pred$class

table(auto.class, testmpg)
mean(auto.class == testmpg)

#------------------------------------------------------------------------------------------------------- 

# LOGISTIC REGRESSION

auto.glm = glm(trainmpg ~ cylinders + displacement + horsepower + weight + acceleration + year + origin, family = binomial, data = trainauto)
summary(auto.glm)

auto.probs = predict(auto.glm, testauto, type = 'response')
n = length(testmpg); auto.pred = rep(0,n)
auto.pred[auto.probs > .5] = 1

table(auto.pred, testmpg)
mean(auto.pred == testmpg)

#------------------------------------------------------------------------------------------------------- 

# K-NEAREST NEIGHBORS

# k = 1: best explanatory power
library(class)
knn.pred = knn(trainauto[-8], testauto[-8], trainmpg, k = 1)
mean(testmpg == knn.pred)

# k = 2: increasing k does not help; higher ks predict at the same rate 
knn.pred = knn(trainauto[-8], testauto[-8], trainmpg, k = 2)
mean(testmpg == knn.pred)



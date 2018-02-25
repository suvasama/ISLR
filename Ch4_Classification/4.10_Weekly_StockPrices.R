# Exercises Chapter 4: Predict if stock prices go up or down

# call the ISLR library
library(ISLR)

# data summary and visualization
names(Weekly)
summary(Weekly)
cor(Weekly[,-9])		# cannot calculate correlation for direction b/c categorial

pairs(Weekly)
attach(Weekly); plot(Volume)

# Logistic regression
weekly.glm = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family = binomial)
summary(weekly.glm)

# Predict the data and compute the confusion matrix
weekly.probs = predict(weekly.glm, type = "response")
n = length(Weekly$Direction)		# number of observation
weekly.pred = rep("Down", n)
weekly.pred[weekly.probs > .5] = "Up"

table(weekly.pred, Direction)		# confusion matrix
mean(weekly.pred == Direction)	#  fraction of correct predictions

#------------------------------------------------------------------------------------------------------- 

# SPLIT THE DATA AND USE ONLY Lag2 AS THE ONLY PREDICTOR

#  slit the data into training and testing sets
train = (Year <= 2008)
Weekly.2009 = Weekly[!train, ];	dim(Weekly.2009)
Direction.2009 = Direction[!train]

# Logistic regression
weekly.glm = glm(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)
summary(weekly.glm)

# Predict the data and compute the confusion matrix
weekly.probs = predict(weekly.glm, Weekly.2009, type = "response")
n = length(Direction.2009)		# number of observation
weekly.pred = rep("Down", n)
weekly.pred[weekly.probs > .5] = "Up"

table(weekly.pred, Direction.2009)		# confusion matrix
mean(weekly.pred == Direction.2009)	#  fraction of correct predictions

#------------------------------------------------------------------------------------------------------- 

# LINEAR DISCRIMINANT ANALYSIS

library(MASS)
weekly.lda = lda(Direction ~ Lag2, data = Weekly, subset = train); weekly.lda
plot(weekly.lda)
weekly.pred = predict(weekly.lda, Weekly.2009)
weekly.class = weekly.pred$class
table(weekly.class, Direction.2009)
mean(weekly.class == Direction.2009)

#------------------------------------------------------------------------------------------------------- 

# QUADRATIC DISCRIMINANT ANALYSIS

weekly.qda = qda(Direction ~ Lag2, data = Weekly, subset = train); weekly.qda
weekly.pred = predict(weekly.qda, Weekly.2009)
weekly.class = weekly.pred$class
table(weekly.class, Direction.2009)
mean(weekly.class == Direction.2009)

#------------------------------------------------------------------------------------------------------- 

# K-NEAREST NEIGHBORS

library(class)
train.X = cbind(Lag2[train]);		test.X = cbind(Lag2[!train])
train.Direction = Direction[train]

weekly.knn = knn(train.X, test.X, train.Direction, k = 1)
table(weekly.knn, Direction.2009)
mean(weekly.knn == Direction.2009)

weekly.knn = knn(train.X, test.X, train.Direction, k = 3)
table(weekly.knn, Direction.2009)
mean(weekly.knn == Direction.2009)

# k = 4 seems to perform best
weekly.knn = knn(train.X, test.X, train.Direction, k = 4)
table(weekly.knn, Direction.2009)
mean(weekly.knn == Direction.2009)
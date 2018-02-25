# CLASSIFICATION MODELS TO PREDICT IF CRIME RATE IS BELOW OR ABOVE MEAN

#------------------------------------------------------------------------------------------------------- 

# DATA AND VISUALIZATION

# call the MASS library and attach the data
library(MASS)
attach(Boston); names(Boston) # plus whatever you need

# crime rate below mean = 0; above mean = 1
crim01 = as.numeric(crim >= median(crim))

#------------------------------------------------------------------------------------------------------- 

# SPLIT THE DATA INTO A TRAIN AND TEST SETS

# take 1/5 of the data to form a test set
n = floor(length(crim)/5); test = 1:n


crimtrain = crim01[-test]; crimtest = crim01[test]
datatrain = Boston[-test,]; datatest = Boston[test,]

#------------------------------------------------------------------------------------------------------- 

# LOGISTIC REGRESSION

crim.glm = glm(crimtrain ~ zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat + medv, family = binomial, data = datatrain)
summary(crim.glm)

crim.probs = predict(crim.glm, datatest, type = 'response')
n = length(crimtest); crim.pred = rep(0, n)
crim.pred[crim.probs > .5] = 1

table(crim.pred, crimtest)
mean(crim.pred == crimtest)

#------------------------------------------------------------------------------------------------------- 

# LINEAR DISCRIMINANT ANALYSIS

crim.lda = lda(crimtrain ~ zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat + medv, data = datatrain)
plot(crim.lda)

crim.pred = predict(crim.lda, datatest)
crim.class = crim.pred$class

table(crim.class, crimtest)
mean(crim.class == crimtest)

#------------------------------------------------------------------------------------------------------- 

# QUADRATIC DISCRIMINANT ANALYSIS

crim.qda = qda(crimtrain ~ zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat + medv, data = datatrain)

crim.pred = predict(crim.qda, datatest)
crim.class = crim.pred$class

table(crim.class, crimtest)
mean(crim.class == crimtest)

#------------------------------------------------------------------------------------------------------- 

# K-NEAREST NEIGHBORS

# k = 1
library(class)
knn.pred = knn(datatrain[-1], datatest[-1], crimtrain, k = 1)

table(crimtest, knn.pred)
mean(crimtest == knn.pred)

# k = 15
knn.pred = knn(datatrain[-1], datatest[-1], crimtrain, k = 15)

table(crimtest, knn.pred)
mean(crimtest == knn.pred)


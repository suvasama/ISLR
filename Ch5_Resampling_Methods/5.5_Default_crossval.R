# USE CROSS VALIDATION TO PREDICT TEST ERROR FOR LOGISTIC REGRESSION

#------------------------------------------------------------------------------------------------------- 

library(ISLR)
attach(Default)

# logistic regression on the entire data set
def.glm = glm(default ~ income + balance, family = binomial); summary(def.glm)

#------------------------------------------------------------------------------------------------------- 

# ESTIMATE THE TEST ERRORS USING VALIDATION

set.seed(1)
train = sample(length(default), floor(length(default)/2))

# estimate the model using only the training set of observations
def.glm = glm(default ~ income + balance, family = binomial, subset = train); summary(def.glm)

# obtain the prediction status for each individual
def.probs = predict(def.glm, Default[-train,], type = 'response')
def.pred = rep("No", length(default[-train])); def.pred[def.probs > .5] = "Yes"

table(def.pred, default[-train])
mean(def.pred == default[-train])

# validation set error
mean(def.pred != default[-train])

# repeat two times more
set.seed(2)
train2 = sample(length(default), floor(length(default)/2))
set.seed(3)
train3 = sample(length(default), floor(length(default)/2))

def2.glm = glm(default ~ income + balance, family = binomial, subset = train2);
def3.glm = glm(default ~ income + balance, family = binomial, subset = train3);

def2.probs = predict(def2.glm, Default[-train2,], type = 'response')
def3.probs = predict(def3.glm, Default[-train3,], type = 'response')
def2.pred = rep("No", length(default[-train2])); def2.pred[def2.probs > .5] = "Yes"
def3.pred = rep("No", length(default[-train3])); def3.pred[def3.probs > .5] = "Yes"

# validation set error
mean(def2.pred != default[-train2])
mean(def3.pred != default[-train3])

#------------------------------------------------------------------------------------------------------- 

# INCLUDE DUMMY VARIABLE STUDENT

# assign value 1 if person is a student
stud01 = as.numeric(student == "Yes")
stud_train = stud01[train]; stud_train2 = stud01[train2]; stud_train3 = stud01[train3]

def.glm = glm(default ~ income + balance + stud_train, family = binomial, subset = train); 
def2.glm = glm(default ~ income + balance + stud_train2, family = binomial, subset = train2);
def3.glm = glm(default ~ income + balance + stud_train3, family = binomial, subset = train3);

# repeat the analysis
def.probs = predict(def.glm, Default[-train,], type = 'response')
def2.probs = predict(def2.glm, Default[-train2,], type = 'response')
def3.probs = predict(def3.glm, Default[-train3,], type = 'response')
def.pred = rep("No", length(default[-train])); def.pred[def.probs > .5] = "Yes"
def2.pred = rep("No", length(default[-train2])); def2.pred[def2.probs > .5] = "Yes"
def3.pred = rep("No", length(default[-train3])); def3.pred[def3.probs > .5] = "Yes"

# validation set error
mean(def.pred != default[-train])
mean(def2.pred != default[-train2])
mean(def3.pred != default[-train3])





# COMPUTE THE LOOCV TEST ERROR ESTIMATE FOR WEEKLY DATA

#------------------------------------------------------------------------------------------------------- 

# call the ISLR library
library(ISLR)

# attach data
attach(Weekly)

#------------------------------------------------------------------------------------------------------- 

# LOGISTIC REGRESSION AND PREDICTION

# fit the model using all observations and all but the first one
weekly.glm = glm(Direction ~ Lag1 + Lag2,  family = binomial)
Weekly1 = Weekly[-1]
weeklyred.glm = glm(Direction ~ Lag1 + Lag2,  family = binomial, data = Weekly1)

# predict Direction using all observations and all but one observation
weekly.probs = predict(weekly.glm, type = 'response')
weekly.probs1 = predict(weeklyred.glm, type = 'response')

# in the second case, predict the first observation in the data
weekly.probs_first = predict(weeklyred.glm, newdata = Weekly[1,], type = 'response' )

# predict up if prob > .5
weekly.pred = "Down"; weekly.pred[weekly.probs_first > .5] = "Up"
# compare with the true realization
if (Direction[1] == weekly.pred) {
	print(0)		# prediction correct, no error made
} else {
	print(1)
}

#------------------------------------------------------------------------------------------------------- 

# PREDICT EACH OBSERVATION IN THE DATA AND COMPUTE THE LOOCV RATE

errors = 0;

for (i in length(Direction)) {
	data = Weekly[-i]; 
	weeklyred.glm = glm(Direction ~ Lag1 + Lag2,  family = binomial, data = data)
	weekly.probs = predict(weeklyred.glm, newdata = Weekly[i,], type = 'response')
	weekly.pred = "Down"; weekly.pred[weekly.probs > .5] = "Up"
	if (Direction[i] == weekly.pred) {
		errors = errors + 1
	}
}

# compute the LOOCV test error rate
print(errors/length(Direction) )

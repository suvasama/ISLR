boot.fn = function(data, index) 
	return(coef(glm(default ~ income + balance, data = data, subset = index, family = binomial)))

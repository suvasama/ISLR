mu01.fn = function(data,index) {
	x = Boston$medv[index]
	return(quantile(x, probs = .1))
}
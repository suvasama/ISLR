# Ex 4.12: FUNCTIONS AND PLOTTING

x = 1:10
plot(x,Power(x,2), type = "l")

# logarithmic scales
plot(x,Power(x,2), log = "x", type = "l")
plot(x,Power(x,2), log = "y", type = "l")
plot(x,Power(x,2), log = "xy", type = "l")
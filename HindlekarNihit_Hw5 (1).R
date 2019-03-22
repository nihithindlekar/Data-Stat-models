#read the data
pov <- read.csv("countyComplete.csv")

#plotting the relation between the variables
plot(pov$poverty~pov$hs_grad, xlab='Graduates', ylab='Poverty', main = 'Poverty')

#calculating correlation
cor(pov$poverty,pov$hs_grad)

#linear model
lmod <- lm(poverty ~ hs_grad, data= pov)

#summary
summary(lmod)

#fitting the line
abline(linear_model, col='red')



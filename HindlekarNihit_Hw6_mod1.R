
#read the data
pov <- read.csv("countyComplete.csv")

#plotting the relation between the variables
pairs(poverty ~ hs_grad + age_under_18 ,data = pov, row1attop=FALSE)

#linear model
lmod <- lm(poverty ~ hs_grad + age_under_18, data= pov)

#calculating coefficients and fitted values
coef(lmod)

fitted2 <- fitted(lmod)

#computing the r-squared
cor(fitted2, pov$poverty) ^ 2

#correlation between variables and fitted values
cor(fitted2, pov$hs_grad)
cor(fitted2, pov$age_under_18)



#read the data
pov <- read.csv("countyComplete.csv")

#plotting the relation between the variables
pairs(poverty ~ hs_grad + age_under_18 + per_capita_income + age_over_65 ,data = pov, row1attop=FALSE)

#linear model
lmod2 <- lm(poverty ~ hs_grad + age_under_18 + per_capita_income + age_over_65, data= pov)

#calculating coefficients and fitted values
coef(lmod2)

fitted3 <- fitted(lmod2)

#computing the r-squared
cor(fitted3, pov$poverty) ^ 2


#correlation between variables and fitted values
cor(fitted3, pov$per_capita_income)
cor(fitted3, pov$age_over_65)
cor(fitted3, pov$hs_grad)
cor(fitted3, pov$age_under_18)



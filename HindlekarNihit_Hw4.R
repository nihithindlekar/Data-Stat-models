
cc = read.csv("countyComplete.csv")
age = cc$age_under_18
#declare the sample size
n = 100

#create a function for calculating variance
avar <- function(x){sum((x-mean(x))^2)/(length(x))}
#calculate the population variance and store it in a variable pop_var
pop_var = avar(age)


#calculate the standard deviation of the population 
Sd_pop = pop_var ^ 0.5
#calculate the standard error 
SE = Sd_pop / (n^0.5)
#creating a random sample of 100 samples from population 
x1 = sample(age, 100)
#declaring the point estimate as the sample mean
estimate = mean(x1)

Sd_pop/(300^0.5)
Sd_pop/(400^0.5)

#calculating the conf interval
CI = estimate + (1.96 * SE)
CI2 = estimate - (1.96 * SE)

##############################################################

#OIS method

##take a sample of size n=100 from population
samp = sample(cc$age_under_18, size = n, replace=TRUE)
##calculate the mean of sample
mean(samp)
#calculate the variance of samp
var_samp = avar(samp)
#Std deviation of sample
sd_samp = var_samp^0.5
#Std error for sample mean
SE_samp = sd_samp / (n^0.5)

#Conf interval
ci = mean(samp) + (1.96 * SE_samp)
ci2 = mean(samp) - (1.96 * SE_samp)

##################################################

#Kaplan method

#take a sample of size= 100 from population
resamp <- deal(cc$age_under_18, 100) #deal function acts similar to the sample() function
#resample 500 times using the do() function
trials = do(500) * mean(resample(resamp), replace=TRUE)

#calculate the confidence interval using the confint() function
confint(trials)


#variance kap
Kap_var = avar(resamp)

#sd kap
sd_Kap= Kap_var^0.5

#SE kap
SE_Kap = sd_Kap/ sqrt(100)


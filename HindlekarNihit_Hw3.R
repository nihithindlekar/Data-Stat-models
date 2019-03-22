
#Per capita income by state

#Installing the mosaic package
install.packages("mosaic")
require(mosaic)

#Reading the file
data = read.csv("countyComplete.csv")


#Grouping the mean of per capita income and state

mn = mean(data$per_capita_income ~ data$state)


mnf <- function(x){return(mn[[x]])}


mnff <- function(v){sapply(v,mnf)}



data= transform(data,fitted= mnff(state))
data= transform(data, resid=(per_capita_income - fitted))

#Calculating variance
var(data$fitted)

var(data$resid)

var(data$fitted) + var(data$resid)

var(data$per_capita_income)

(var(data$fitted)) / (var(data$per_capita_income))





#Model1


group1 = read.csv("countyComplete.csv")


min(group1$age_over_65)

max(group1$age_over_65)

group1$Over65 = cut(group1$age_over_65, breaks=c(0,10,20,30,40,50), labels=c("Senior_1","Senior_2","Senior_3","Senior_4","Senior_5"))
group1$Over65

mn_age = mean(group1$poverty ~ group1$Over65)
mn_age

mn_agef <- function(x){return(mn_age[[x]])}

mn_agef("Senior_2")

mn_ageff <- function(v){sapply(v,mn_agef)}
group1$Over65


group1= transform(group1,fitted= mn_ageff(Over65))
group1= transform(group1, resid=(poverty - fitted))

var(group1$fitted)

var(group1$resid)

var(group1$fitted) + var(group1$resid)
var(group1$poverty)

(var(group1$fitted)) / (var(group1$poverty))





#Model2

group2 = read.csv("countyComplete.csv")

group2$gradhs = cut(group2$hs_grad, breaks=c(0,25,50,75,100), labels=c("First","Second","Third","Fourth"))

mn_hg = mean(group2$poverty ~ group2$gradhs)
mn_hg
#First   Second       Third        Fourth 
#NaN    38.00000    22.77212    14.18886 

mn_hgf <- function(x){return(mn_hg[[x]])}
mn_hgf("Second")
#[1] 38

mn_hgff <- function(b){sapply(b,mn_hgf)}
mn_hgff(group2$gradhs)

group2 = transform(group2,fitted=mn_hgff(group2$gradhs))
group2 = transform(group2,resid=(group2$poverty - fitted))



var(group2$fitted)
#[1] 9.64773

var(group2$resid)
#[1] 31.10595

var(group2$fitted) + var(group2$resid)
#[1] 40.75368

var(group2$poverty)
#[1] 40.75368

(var(group2$fitted))/(var(group2$poverty))
#[1] 0.2367327
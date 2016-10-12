# Chapter 3 Lab: Linear Regression

library(MASS)
library(ISLR)

#########################################################
# Simple Linear Regression

fix(Boston)   # invokes ``edit`` on Boston and then assigns the new edited version of Boston in the workspace
names(Boston)
?Boston
attach(Boston)  # By attaching Boston variable names in the data-set can be accessed directly

lm.fit=lm(medv~lstat)     #medv = median house value in $1,000's for 506 nghbds in Boston
lm.fit                    # lstat = % of households with low socioeconomic status  
summary(lm.fit)  # provides more info that lm.fit

names(lm.fit)    # see what other info is available in lm.fit. Can extract them via lm.fit$coefficients etc but safer to use extractor functions
coef(lm.fit)
confint(lm.fit)

predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="confidence")
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="prediction")

plot(lstat,medv)  # plot the data
abline(lm.fit)   # plot the fitted regression line
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")

plot(lstat,medv,col="red") # This and next few commands just show how to plot different symbols
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
plot(1:20,1:20,pch=1:20)

# Time for some diagnostic plots
par(mfrow=c(2,2))
plot(lm.fit)
plot(predict(lm.fit), residuals(lm.fit))  # residual plot: some evidence of non-linearity
plot(predict(lm.fit), rstudent(lm.fit))   # studentized residuals 
plot(hatvalues(lm.fit))  # leverage values (from the diagonal of the hat matrix)
which.max(hatvalues(lm.fit))   # data-point with largest leverage stat

####################################################################
# Multiple Linear Regression

lm.fit=lm(medv~lstat+age,data=Boston)
summary(lm.fit)
lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)

library(car)  # Using this package as it can be used to compute variance inflation factors
vif(lm.fit)   # some signs of multi-colinearity (tax and rad)

lm.fit1=lm(medv~.-age,data=Boston)  # fitting multiple regression with all vars except age
summary(lm.fit1)
lm.fit1=update(lm.fit, ~.-age)  # alternatively the update function can be used

# Can include interaction Terms
summary(lm(medv~lstat*age,data=Boston))  # the "*" tells R to use lstat and age as well as the interaction

# Non-linear Transformations of the Predictors
lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)  #p-val for quadratic term approx 0
lm.fit=lm(medv~lstat)
anova(lm.fit,lm.fit2)   # performs a hypothesis test comparing the 2 models, one of which is nested
# anova confirms suspicion that model with quadratic term is superior 

par(mfrow=c(2,2))
plot(lm.fit2)  # plots look better as well

lm.fit5=lm(medv~poly(lstat,5))  # fitting a 5th order polynomial in lstats
summary(lm.fit5)  # suggests a 5th order polynomial improves the fit. But beware!

summary(lm(medv~log(rm),data=Boston))  # other non-linear transformations possible

########################################################################
# Qualitative Predictors

fix(Carseats)
names(Carseats)
?Carseats

lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)
attach(Carseats)
contrasts(ShelveLoc)  # ShelveLoc is a factor with 3 levels so 2 dummy variables required for this

###########################################################################
# Writing Functions

LoadLibraries=function(){
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded.")
}
LoadLibraries
LoadLibraries()
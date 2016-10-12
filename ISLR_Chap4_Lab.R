# Chapter 4 Lab: Logistic Regression, LDA, QDA, and KNN

# The Stock Market Data
library(ISLR)
names(Smarket)   # Contains % returns for S&P 500 for 1250 days from 2001 to 2005 
dim(Smarket)     # It also contains returns for 5 previous days and previous day's trading volume
summary(Smarket)
pairs(Smarket)
#cor(Smarket)  # will produce an error since Direction is a factor (with 2 levels: "Down" & "Up")
cor(Smarket[,-9])  # As expected correlations between returns are close to 0
attach(Smarket)
plot(Volume)

#############################################################
# Logistic Regression
# Logistic regression is a specific example of "Generalized Linear Regression" (GLM)
# We fit it with the glm() function which is similar to lm() except we must pass in the 
# argument family = binomial to tell it to fit a logistic regression
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef # coeffs, std. errors, z-values abd p-values
summary(glm.fit)$coef[,4]   # just the p-values

glm.probs=predict(glm.fit,type="response")  # predicted probabilities
glm.probs[1:10]
contrasts(Direction)  # 1 for up so predicted probs are for market going up

glm.pred=rep("Down",1250)  # A 1250 x 1 vector with "Down" in every position 
glm.pred[glm.probs>.5]="Up" # Changing to "Up" those entries that are predicted to be "up"
table(glm.pred,Direction)  # The confusion matrix
(507+145)/1250   # % of correct predictions
mean(glm.pred==Direction)  # again, % of correct predictions on training set

train=(Year<2005)   # Creating separate training and test sets 
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]
# Notice use of subset=train argument on next line
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005) # confusion matrix for test set
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)  # test error rate is 52% Not good at all ...

# Recall p-values are all pretty large so perhaps a good idea to throw most of them out
glm.fit=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)  # Just using lag1 and lag2
glm.probs=predict(glm.fit,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)   # results are "better": 56% of moves correctly predicted
# But it turns out market went up 56% of the time in 2005 so results not very good really
106/(106+76)  # =58.2% = % of correct predictions when prediction was "Up"

# Can get specific predicted probabilities by passing values of explanatory variables to the fitted model
predict(glm.fit,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type="response")

############################################################
# Linear Discriminant Analysis

library(MASS)
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)  # fit LDA on same data
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class,Direction.2005)  #confusion matrix
mean(lda.class==Direction.2005)
sum(lda.pred$posterior[,1]>=.5)  # posterior is an n x K=2  matrix of poserior probs (n = # of data points)
sum(lda.pred$posterior[,1]<.5)
lda.pred$posterior[1:20,1]
lda.class[1:20]
sum(lda.pred$posterior[,1]>.9) # Using .9 as our threshold to predict market will decrease


# Quadratic Discriminant Analysis
qda.fit=qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.fit
qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)
mean(qda.class==Direction.2005)  # Accurate almost 60% of the time. Not bad!

######################################################
# K-Nearest Neighbors

library(class)
train.X=cbind(Lag1,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction=Direction[train]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction,k=1)  # will train with 1st and 3rd args using k=1 NN
table(knn.pred,Direction.2005)
(83+43)/252   # Only 50% accuracy! 
knn.pred=knn(train.X,test.X,train.Direction,k=3)  # Repeat with k =3
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)   # 53.6% accuracy now

# An Application to Caravan Insurance Data: See Discussion in Section 4.6.6
# Will see there that don't alwasy care about total error rate. Also makes sense to 
# vary threshold in logistic regression below for this reason.
dim(Caravan)
attach(Caravan)
names(Caravan)
head(Caravan)
summary(Purchase)
348/5822
standardized.X=scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])
test=1:1000
train.X=standardized.X[-test,]
test.X=standardized.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=1)
mean(test.Y!=knn.pred)
mean(test.Y!="No")
table(knn.pred,test.Y)
9/(68+9)
knn.pred=knn(train.X,test.X,train.Y,k=3)
table(knn.pred,test.Y)
5/26
knn.pred=knn(train.X,test.X,train.Y,k=5)
table(knn.pred,test.Y)
4/15
glm.fit=glm(Purchase~.,data=Caravan,family=binomial,subset=-test)
glm.probs=predict(glm.fit,Caravan[test,],type="response")
glm.pred=rep("No",1000)
glm.pred[glm.probs>.5]="Yes"
table(glm.pred,test.Y)
glm.pred=rep("No",1000)
glm.pred[glm.probs>.25]="Yes"
table(glm.pred,test.Y)
11/(22+11)

require(DAAG)
require(ggplot2)
require(MASS)
library(caret)
# This code implements reduced rank LDA (Fisher Discriminant Analysis)
# It can reproduce the subplots of Figure 4.8 in HTF by specifing coordinates a,b 
# For example, a=1,b=3 reproduces the top-left sub-figure of Figure 4.8

a=9 # First Fisher coordinate to plot
b=10 # second Fisher coordinate to plot

################################################################
# First download the training data from the HTF website
url<-"http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/vowel.train"
vtrain<-read.table(url,header=TRUE,sep=',')
vtrain<-as.data.frame(vtrain)
# columns are row.names,y,x.1,x.2,x.3,x.4,x.5,x.6,x.7,x.8,x.9,x.10
# y is the c  lass, and x.1 to x.10 are predictors

# Now download the test data
url<-"http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/vowel.test"
  vtest<-read.table(url,header=TRUE,sep=',')
vtest<-as.data.frame(vtest)
#################################################################

# Now find the Fisher discriminant directions using the "lda" function

ldatrain<-lda(y~x.1+x.2+x.3+x.4+x.5+x.6+x.7+x.8+x.9+x.10, data=vtrain)

vtrain.lda.values <- data.frame(predict(ldatrain, vtrain[,3:12]))
vtrain.lda.values.means <- data.frame(predict(ldatrain, data.frame(ldatrain.means)))


vtest.lda.values <- data.frame(predict(ldatrain, vtest[,3:12]))


plot.data <- subset(vtrain.lda.values, select= c(paste('x.LD',a,sep=''), paste('x.LD',b,sep='')))
plot.data.means <- subset(vtrain.lda.values.means, select= c(paste('x.LD',a,sep=''), paste('x.LD',b,sep='')))

p <- ggplot() +
  geom_point(data = plot.data, aes(
              x = plot.data[,1],
              y = plot.data[,2],
              color=factor(vtrain.lda.values$class), shape=1)) +
  xlab(paste('Coordinate ',a)) +
  ylab(paste('Coordinate ',b)) + 
  geom_point(data = plot.data.means, aes(
    x = plot.data.means[,1],
    y = plot.data.means[,2],
    colour=factor(vtrain.lda.values.means$class), shape=1, stroke=5, size=10)) +
  scale_shape_identity() + theme(legend.position="none")
p

#################
# Part B
# Now we us LDA again to predict values in the training set and the test set
errors.train = numeric(length=10)
errors.test = numeric(length=10)
rep=10
 
for(t in 1:rep){
  
  ldatrain.predict <- data.frame(predict(ldatrain, vtrain[,3:12], dimen=t))
  
  errors.train[t] = 1- confusionMatrix(ldatrain.predict$class, vtrain$y)$overall[1]

  ldatest.predict <- data.frame(predict(ldatrain, vtest[,3:12], dimen=t))
  
  errors.test[t] = 1- confusionMatrix(ldatest.predict$class, vtest$y)$overall[1]
}

rm(errplot)
errplot <-  ggplot() +
              geom_line(data = data.frame(errors.train), aes(
              x = c(1,2,3,4,5,6,7,8,9,10), 
              y = errors.train[1:10]), colour="blue") + scale_shape_identity() +
              geom_point(data = data.frame(errors.train), aes(
              x = c(1,2,3,4,5,6,7,8,9,10), 
              y = errors.train[1:10]), shape=16) +
              xlab('Dimension') +
              ylab('Misclassification Rate') +
              geom_line(data = data.frame(errors.test), aes(
              x = c(1,2,3,4,5,6,7,8,9,10), 
              y = errors.test[1:10]), colour="orange") +
              geom_point(data = data.frame(errors.test), aes(
              x = c(1,2,3,4,5,6,7,8,9,10), 
              y = errors.test[1:10]), shape=16) +
              ggtitle('LDA and Dimension Reduction on the Vowel Data')
errplot

for(t in 2:2){
  ldatest.predict <- data.frame(predict(ldatrain, vtest[,3:12], dimen=t))
}
confusionMatrix(ldatest.predict$class, vtest$y)



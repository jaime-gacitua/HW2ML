library(e1071)
library(caret)

rep <- 1000

error = numeric(length=rep)
error.lap = numeric(length=rep)
error.misspct = numeric(length=rep)
error.misspct.lap = numeric(length=rep)

spamdata_pre <- read.csv(".\\spam.csv")
spamdata <- spamdata_pre[,3:21]
names(spamdata) #variable column headings
summary(spamdata)

# Check is missing spampct is relevant
spamdata$spampct.present <- factor(!is.na(spamdata$spampct))

# Check if it is working properly
head(spamdata)
# spampct seems to be important, because when it is not present, we have 1062 cases that email is not spam.
table(spamdata$spam, spamdata$spampct.present)


for(t in 1:rep){
  sub <- sample(nrow(spamdata), floor(nrow(spamdata) * 0.8))
  trainData <- spamdata[sub, ]
  testData <- spamdata[-sub, ]

  naive.model <- naiveBayes(spam~., data = trainData, laplace = 0)
  naive.model.lap <- naiveBayes(spam~., data = trainData, laplace = 3)
  naive.model.misspct <- naiveBayes(spam~., data = trainData, laplace = 0)
  
  # Independent variables
  x_test <- testData[,1:18]
  # Independent variables + dummy variable representing missing spampct value
  x_test.misspct <- testData[,1:18]
  x_test.misspct$spampct.present <- testData$spampct.present
  
  # y dependent variables
  y_test <- testData[,19]
  
  pred <- predict(naive.model, x_test)
  pred.lap <- predict(naive.model.lap, x_test)
  pred.misspct <- predict(naive.model.misspct, x_test.misspct)
  
  pred.output <- confusionMatrix(pred, y_test)
  pred.output.lap <- confusionMatrix(pred.lap, y_test)
  pred.output.misspct <- confusionMatrix(pred.misspct, y_test)
  
  #print(naive.model)
  #print(pred.output)
  
  error[t] <- 1-pred.output$overall[1]
  error.lap[t] <- 1-pred.output.lap$overall[1]
  error.misspct[t] <- 1-pred.output.misspct$overall[1]
  
  }

cat('Mean error base',mean(error))
cat('Mean error laplace',mean(error.lap))
cat('Mean error misspct',mean(error.misspct))

cat('Var error base',sd(error))
cat('Var error laplace',sd(error.lap))
cat('Var error misspct',sd(error.misspct))

par(mfrow = c(3,1))
hist(error, col='yellow', xlim=c(0.0,0.1))
hist(error.lap, col='red', xlim=c(0.0,0.1))
hist(error.misspct, col='green', xlim=c(0.0,0.1))




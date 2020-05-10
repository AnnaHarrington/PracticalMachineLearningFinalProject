library(caret)
library(corrplot)
library(e1071)
library(randomForest)
set.seed(1993)
trainingdatain <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"))
testingdatain  <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"))

trainingdata <- trainingdatain
testingdata <- testingdatain

trainingdata <- trainingdata[, -c(1:7)]
testingdata <- testingdata[, -c(1:7)]
naremove    <- sapply(trainingdata, function(x) mean(is.na(x))) > 0.9
trainingdata <- trainingdata[, naremove==FALSE]
testingdata  <- testingdata[, naremove==FALSE]

intrain <- createDataPartition(trainingdata$classe, p=0.7, list=FALSE)
trainingdata <- trainingdata[intrain, ]
testingdatasplit <- trainingdata[-intrain, ]

near0variance <- nearZeroVar(trainingdata)
trainingdata <- trainingdata[, -near0variance]
testingdatasplit  <- testingdatasplit[, -near0variance]

correlation <- cor(trainingdata[, -53])

corrplot(correlation, order = "FPC", method = "color", type = "lower", 
         tl.cex = 0.8 , tl.col = rgb(0, 0, 0))


randomforest <- trainControl(method="cv", number=3, verboseIter=FALSE)
randomforest1 <- train(classe ~., data=trainingdata, trControl=randomforest)
print(randomforest1$finalModel)

randomforest2 <- predict(randomforest1, newdata=testingdatasplit)
randomforest3 <- confusionMatrix(randomforest2, testingdatasplit$class)
print(randomforest3)

plot(randomforest3$table, col=randomforest3$byClass, main="Random Forest Accuracy")

randomforest4 <- predict(randomforest1, newdata=testingdata)
print(randomforest4)
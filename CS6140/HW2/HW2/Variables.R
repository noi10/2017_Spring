mydata <- as.data.frame(read.table("prostate.txt"))
trainSet <- mydata[mydata$train == "TRUE", -c(10)]
testSet <- mydata[mydata$train == "FALSE", -c(10)]
x.test <- model.matrix(lpsa ~ ., data = testSet)
y.test <- testSet$lpsa
library(e1071)

cat("-- iris --\n")

train.data = read.table("ExampleDatasets/irisTraining.txt")
test.data = read.table("ExampleDatasets/irisTesting.txt")

predictor <- naiveBayes(train.data[,1:4], factor(train.data[,5]))
prediction <- table(predict(m, test.data[,1:4]), factor(test.data[,5]))

cat("True Positives: ", prediction[2,2], "\n")
cat("False Positives: ", prediction[2,1], "\n")
cat("True Negatives: ", prediction[1,1], "\n")
cat("False Negatives: ", prediction[1,2], "\n")

cat("\n-- irisPC --\n")

train.data = read.table("ExampleDatasets/irisPCTraining.txt")
test.data = read.table("ExampleDatasets/irisPCTesting.txt")

predictor <- naiveBayes(train.data[,1:2], factor(train.data[,3]))
prediction <- table(predict(m, test.data[,1:2]), factor(test.data[,3]))

cat("True Positives: ", prediction[2,2], "\n")
cat("False Positives: ", prediction[2,1], "\n")
cat("True Negatives: ", prediction[1,1], "\n")
cat("False Negatives: ", prediction[1,2], "\n")

cat("\n-- buy --\n")

train.data = read.table("ExampleDatasets/buyTraining.txt")
test.data = read.table("ExampleDatasets/buyTesting.txt")

predictor <- naiveBayes(train.data[,1:4], factor(train.data[,5]), laplace=1)
prediction <- table(predict(m, test.data[,1:4]), factor(test.data[,5]))

cat("True Positives: ", prediction[2,2], "\n")
cat("False Positives: ", prediction[2,1], "\n")
cat("True Negatives: ", prediction[1,1], "\n")
cat("False Negatives: ", prediction[1,2], "\n")

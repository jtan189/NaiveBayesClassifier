# Implementation of the Naive Bayes Classifier in R
## Author: Josh Tan

# You can use setwd("C:/YourPATH here/") to set the working directory..

# input parameters
training.file = "AssigData2/irisPCTraining.txt"
testing.file = "AssigData2/irisPCTesting.txt"

train = as.matrix(read.table(training.file))
num.rows = dim(train)[1]
num.cols = dim(train)[2] - 1 # the last attribute is the class label, so it does not count.

# <test>
#cat("training data:\n", trainingMatrix)
#cat("rows: ", n, "\n")
#cat("cols: ", d, "\n")
# </test>

#Training... Collect mean and standard deviation for each dimension for each class..
#Also, calculate P(C+) and P(C-)

train.pos.rows = which(train[, (num.cols + 1)] == 1)
train.neg.rows = which(train[, (num.cols + 1)] == -1)

train.pos.colmeans = colMeans(train[train.pos.rows, 1:num.cols])
train.neg.colmeans = colMeans(train[train.neg.rows, 1:num.cols])

# http://nsaunders.wordpress.com/2010/08/20/a-brief-introduction-to-apply-in-r/

train.pos.sd = apply(train[train.pos.rows, 1:num.cols], 2, sd)
train.neg.sd = apply(train[train.neg.rows, 1:num.cols], 2, sd)

prob.pos = length(train.pos.rows) / num.rows
prob.neg = length(train.neg.rows) / num.rows

#Testing .....

test = as.matrix(read.table(testing.file))
test.numrows = dim(test)[1] # Number of points in the testing data.

prob.test.pos <- rep(1, test.numrows)
prob.test.neg <- rep(1, test.numrows)
test.pred = vector()

# assessment
true.pos = 0;
true.neg = 0;
false.pos = 0;
false.neg = 0;

for (i in 1:test.numrows) {

    for (j in 1:num.cols) {

        prob.test.pos[i] = prob.test.pos[i] * dnorm(test[i,j], train.pos.colmeans[j], train.pos.sd[j])
        prob.test.neg[i] = prob.test.neg[i] * dnorm(test[i,j], train.neg.colmeans[j], train.neg.sd[j])

        ## cat("prob: ", prob.test.pos[i], "\n")
        ## if (is.na(prob.test.pos[i])) {
        ##     print("wtf!!!!!!! POS \n")
        ##     cat("i: ", i, "; j: ", j, "\n")
        ## }

        ## if (is.na(prob.test.neg[i])) {
        ##     print("wtf!!!!!!! NEG \n")
        ##     cat("i: ", i, "; j: ", j, "\n")
        ## }
    }

    prob.test.pos[i] = prob.test.pos[i] * prob.pos
    prob.test.neg[i] = prob.test.neg[i] * prob.neg

    if (prob.test.pos[i] > prob.test.neg[i]) {
        test.pred[i] = 1
    } else if (prob.test.pos[i] < prob.test.neg[i]) {
        test.pred[i] = -1
    } else {
        test.pred[i] = 0 # add logic to pick one
    }

    ## cat("pred: ", test.pred[i], "\n")
    if (test.pred[i] == 1) {
        if (test[i, num.cols + 1] == 1) {
            true.pos = true.pos + 1
        } else {
            false.pos = false.pos + 1
        }
    } else {
        if (test[i, num.cols + 1] == -1) {
            true.neg = true.neg + 1
        } else {
            false.neg = false.neg + 1
        }
    }
}

accuracy = (true.pos + true.neg) / test.numrows
precision = true.pos / (true.pos + false.pos)
recall = true.pos / (true.pos + false.neg)

cat("accuracy: ", accuracy, "\n")

cat("true pos: ", true.pos, "\n")
cat("false pos: ", false.pos, "\n")
cat("true neg: ", true.neg, "\n")
cat("false neg: ", false.neg, "\n")

cat("precision: ", precision, "\n")
cat("recall: ", recall, "\n")


#for (i in 1:nn) {

  #For each point find the P(C+|Xi) and P(C-|Xi) and decide if the point belongs to C+ or C-..
  #Recall we need to calculate P(Xi|C+)*P(C+) ..
  #P(Xi|C+) = P(Xi1|C+) * P(Xi2|C+)....P(Xid|C+)....Do the same for P(Xi|C-)
  #Now that you've calculate P(Xi|C+) and P(Xi|C-), we can decide which is higher 
  #P(Xi|C-)*P(C-) or P(Xi|C-)*P(C-) ..
  #increment TP,FP,FN,TN accordingly, remember the true label for the ith point is in Xtest[i,(d+1)]

#}

# Calculate all the measures required..

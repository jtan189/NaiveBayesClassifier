# Implementation of the Naive Bayes Classifier in R (for categorical attributes)
## Josh Tan
## CSCI 479
## 11/5/13

# input parameters
train.file = "ExampleDatasets/buyTraining.txt"
test.file = "ExampleDatasets/buyTesting.txt"

# TRAINING

# read in training data
train.data = as.matrix(read.table(train.file))
train.nrows = nrow(train.data)
ncols = ncol(train.data) - 1 # last attribute is the class label

## separate data based on class
train.data.pos = which(train.data[, (ncols + 1)] == 1)
train.data.neg = which(train.data[, (ncols + 1)] == -1)

## ## for each class, calculate means for each attribute
## train.data.pos.mean = colMeans(train.data[train.data.pos, 1:ncols])
## train.data.neg.mean = colMeans(train.data[train.data.neg, 1:ncols])

## ## for each class, calculate standard deviations for each attribute
## train.data.pos.sd = apply(train.data[train.data.pos, 1:ncols], 2, sd)
## train.data.neg.sd = apply(train.data[train.data.neg, 1:ncols], 2, sd)

## for each class, calculate priors
prior.pos = length(train.data.pos) / train.nrows
prior.neg = length(train.data.neg) / train.nrows


## as.data.frame(table(train.data[,1]))


# TESTING

# read in testing data
test.data = as.matrix(read.table(test.file))
test.nrows = nrow(test.data)

# initialize posteriors and class predictions
post.pos <- rep(prior.pos, test.nrows)
post.neg <- rep(prior.neg, test.nrows)
test.pred = vector()

# initialize assessment variables
true.pos = 0;
true.neg = 0;
false.pos = 0;
false.neg = 0;

for (i in 1:test.nrows) {

    for (j in 1:ncols) {
        ## calculate numerator of posteriori (likelihood * prior)
        num.occur = length(which(train.data[,j] == test.data[i,j]))

        if (num.occur == 0) {
            print("Do something.\n")
        }

        post.pos[i] = post.pos[i] * (num.occur / train.nrows)
        post.neg[i] = post.neg[i] * (num.occur / train.nrows)
    }

    # compare posteriors to determine which is greater; corresponding class will be used
    if (post.pos[i] >= post.neg[i]) { # if equal, just choose the positive class
        test.pred[i] = 1
    } else {
        test.pred[i] = -1
    } 

    # increment appropriate assessment variable
    if (test.pred[i] == 1) {
        if (test.data[i, ncols + 1] == 1) {
            true.pos = true.pos + 1
        } else {
            false.pos = false.pos + 1
        }
    } else {
        if (test.data[i, ncols + 1] == -1) {
            true.neg = true.neg + 1
        } else {
            false.neg = false.neg + 1
        }
    }
}

# calculate performance metrics
accuracy = (true.pos + true.neg) / test.nrows
precision = true.pos / (true.pos + false.pos)
recall = true.pos / (true.pos + false.neg)

cat("True Positives: ", true.pos, "\n")
cat("False Positives: ", false.pos, "\n")
cat("True Negatives: ", true.neg, "\n")
cat("False Negatives: ", false.neg, "\n")
cat("Accuracy: ", accuracy, "\n")
cat("Precision: ", precision, "\n")
cat("Recall: ", recall, "\n")

## Implementation of the Naive Bayes Classifier in R (for categorical attributes)
## Josh Tan
## CSCI 479
## 11/5/13

## input parameters
train.file = "ExampleDatasets/buyTraining.txt"
test.file = "ExampleDatasets/buyTesting.txt"

## TRAINING

## read in training data
train.data = as.matrix(read.table(train.file))
train.nrows = nrow(train.data)
ncols = ncol(train.data) - 1 # last attribute is the class label

## separate data based on class
train.data.pos = which(train.data[, (ncols + 1)] == 1)
train.data.neg = which(train.data[, (ncols + 1)] == -1)

## for each class, calculate priors
prior.pos = length(train.data.pos) / train.nrows
prior.neg = length(train.data.neg) / train.nrows

## TESTING

## read in testing data
test.data = as.matrix(read.table(test.file))
test.nrows = nrow(test.data)

## initialize posteriors and class predictions
post.pos <- rep(prior.pos, test.nrows)
post.neg <- rep(prior.neg, test.nrows)
test.pred = vector()

## initialize assessment variables
true.pos = 0;
true.neg = 0;
false.pos = 0;
false.neg = 0;

for (i in 1:test.nrows) {

    for (j in 1:ncols) {
        ## calculate numerator of posteriori (likelihood * prior)
        num.occur = length(which(train.data[,j] == test.data[i,j])) + 1 # apply laplace correction by adding 1

        denom = train.nrows + length(unique(test.data[,j])) + length(unique(train.data[,j])) # add in number of unique categories

        post.pos[i] = post.pos[i] * (num.occur / denom)
        post.neg[i] = post.neg[i] * (num.occur / denom)

        ## <debug>
        ## print("-------\n")
        ## cat('post.pos: ', post.pos[i], "\n")
        ## cat('post.neg: ', post.neg[i], "\n")
        ## cat('prior.pos: ', prior.pos, "\n")
        ## cat('prior.neg: ', prior.neg, "\n")
        ## cat('numer: ', num.occur, "\n")
        ## cat('denom: ', denom, "\n")
        ## print("-------\n")
        ## </debug>
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

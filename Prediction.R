## Generalised column names for all n-gram models##
## where "n.gram" represents the relevant n-gram (i.e. trigram, bigram, unigram), "n_1.gram" represents the corresponding predictor
## (i.e bigram, unigram) and "unigram" the outcome (which is always a unigram regardless of the level of the language model). 
## "n.gram_count" and "n_1.gram_count" are the frequency counts of the predictor and outcome, and "prob.estimate" is the probability
## estimate (i.e. maximum liklihood estimate (MLE) or some smoothed probability estimate).



make.names(c("n.gram", "n_1.gram", "unigram", "n.gram_count", "n_1.gram_count", "prob.estimate"))

## Prediction alogrithm from n-gram model ##

library(dplyr)


## Clean up input for prediction model ##


input.clean <- function (x = character(0), m = 2) {
        require(tm)
        x <- tolower(x)
        x <- removePunctuation(x)
        x <- removeNumbers(x)
        #x <- removeWords(x, stopwords("english"))#
        x <- stripWhitespace(x)
        x <- unlist(strsplit(x, split = " "))
        n <- length(x)
        x <- paste(x[(n-(m-1)):n],  collapse = " ")
        x
}


## 3-gram model ##

predict.ngram <- function (x = character, model, n = 1) {
                         require(dplyr)
                         candidates <- filter(model, model[ , 3] == x)
                         candidates <- arrange(candidates, desc(MLE))
                         candidates <- slice(candidates, 1:n)
                         select(candidates, unigram)
}
## Prediction alogrithm from n-gram model ##

library(dplyr)


## Clean up input for prediction model ##

input.clean <- function (x) {
        require(tm)
        x <- tolower(x)
        x <- removePunctuation(x)
        x <- removeNumbers(x)
        #x <- removeWords(x, stopwords("english"))#
        x <- stripWhitespace(x)
        x <- unlist(strsplit(x, split = " "))
        n <- length(x)
        x <- paste(x[(n-1):n],  collapse = " ")
        x
}


## 3-gram model ##

predict.ngram <- function (x = character, model, n = 1) {
                         candidates <- filter(model, bigram == x)
                         candidates <- arrange(candidates, desc(MLE))
                         candidates <- slice(candidates, 1:n)
                         select(candidates, unigram)
}
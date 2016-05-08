## Prediction alogrithm from n-gram model ##

library(dplyr)

## 3-gram model ##

predict.ngram <- function (x = character, model, n = 1) {
                         candidates <- filter(model, bigram == x)
                         candidates <- arrange(candidates, desc(MLE))
                         candidates <- slice(candidates, 1:n)
                         select(candidates, unigram)
}
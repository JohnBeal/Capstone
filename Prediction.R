## Generalised column names for all n-gram models##
## where "n.gram" represents the relevant n-gram (i.e. trigram, bigram, unigram), "n_1.gram" represents the corresponding predictor
## (i.e bigram, unigram) and "unigram" the outcome (which is always a unigram regardless of the level of the language model). 
## "n.gram_count" and "n_1.gram_count" are the frequency counts of the predictor and outcome, and "prob.estimate" is the probability
## estimate (i.e. maximum liklihood estimate (MLE) or some smoothed probability estimate).


make.names(c("n.gram", "n_1.gram", "unigram", "n.gram_count", "n_1.gram_count", "prob.estimate"))

## Prediction alogrithm from n-gram model ##

library(dplyr)


## Clean up input for prediction model ##


input.clean <- function (x = character(0), n = 2, stopword = FALSE) {
        require(tm)
        
        ## Replicate text clean up process used in n-gram preparation ##
        ## change to lower CASE, remove punctuation, remove numbers and strip extra white space
        x <- tolower(x)
        x <- removePunctuation(x)
        x <- removeNumbers(x)
        
        ## Conditional stopword removal ##
        if (stopword == TRUE){
                x <- removeWords(x, stopwords("english"))}
        
        x <- stripWhitespace(x)
        
        ## Take the last m words as output ##
        x <- unlist(strsplit(x, split = " "))
        m <- length(x)
        x <- paste(x[(m-(n-1)):m],  collapse = " ")
        x
}

## n-gram predictorl ##

predict.ngram <- function (x = character, model, s = 1) {
                         require(dplyr)
                         candidates <- filter(model, model[ , "n_1.gram"] == x)
                         candidates <- arrange(candidates, desc(prob.estimate))
                         candidates <- slice(candidates, 1:s)
                         if (nrow(candidates) > 0) {
                                output <- select(candidates, unigram)
                         }
                         else output <- NULL
                         output
                                 
}

## Back-off model ##

predict.ngramBOff <- function (x = character, s = 1) {
                        output <- NULL
                        ## Apply trigram language model ##
                        if (is.null(output) == TRUE) {
                                output <- predict.ngram(input.clean(x = x, n = 2), model = uvw_trigram_pruned_MLE, s = s)
                                          
                                         }
                        ## Conditionally apply bigram language model ##
                        if (is.null(output) == TRUE)  {
                        output <- predict.ngram(input.clean(x = x, n = 1), model = uvw_bigram_pruned_MLE, s = s)
                                           }
                        ## Conditionally apply unigram language model ##
                        if (is.null(output) == TRUE) {
                                candidates <- uvw_unigram_pruned_MLE
                                candidates <- slice(candidates, 1:s)
                                output <- select(candidates, unigram)
                                                }
                        output
}
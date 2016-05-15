## Script to produce n-gram language models from training sets of HC Corpora en_US corpora (i.e. Twitter, Blogs and News) ##
## 1. Retrival of Training Sets, assembly into single corpus.
## 2. Data cleaning (lowercase, removing punctuation and numbers (possible stopword removal and stemming)
## 3. Tokenization to produce TDMs (Term Document Matrix) of n-grams and (n-1)-grams for each corpus 
##    and conversion of TDMs to cumulative Term Frequency Vectors (TFV)
## 4. Output for storage

library(RWeka)
library(tm)
require(SnowballC)
library(slam)

## 1. Retrival of Training Sets ##

## Establish connections to files containing en_US corpora training sets ##
con <- list(".\\Coursera-SwiftKey\\final\\en_US\\en_US.twitter-training.txt", 
            ".\\Coursera-SwiftKey\\final\\en_US\\en_US.blogs-training.txt", 
            ".\\Coursera-SwiftKey\\final\\en_US\\en_US.news-training.txt")

## Read in training sets ##

mydata <- lapply(con, readLines, encoding = "UTF-8")
names(mydata) <- c("Twitter", "Blogs", "News")

## Assemble training sets into Corpora using tm package ##
## Merge corpora into single corpus for further processing ##

myCorpora <- lapply(lapply(mydata, VectorSource), Corpus)
myCorpus <- c(myCorpora[["Twitter"]], myCorpora[["Blogs"]], myCorpora[["News"]])

## 2. Data Cleaning & Tokenization ##
## For the sake of efficiency, fucntions: tolower, removePunctuation & removeNumbers are mapped to the documents in the corpora
## before n-gram tokenization (with relevent controls set to FALSE) to avoid repetition. 

myCorpus <- tm_map(myCorpus, tolower)
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)
myCorpus <- tm_map(myCorpus, PlainTextDocument, language = "en")

## 3. n-gram Tokenization ##

## Tokenize n-gram TDMs for n = 3, 2, 1.##
## Use row_sums to collapse the TDM into Term Frequency Vector ##

myTDM_trigram <- TermDocumentMatrix(myCorpus, control = 
                                list(tokenize = function(x) {NGramTokenizer(x, Weka_control(min = 3, max = 3))},
                                     tolower = FALSE, removePunctuation = FALSE, removeNumers = FALSE, stopwords = FALSE,
                                     stemming = FALSE, bounds = list(global = (c(1, Inf))),  wordLengths = c(3, Inf)))

myTFV_trigram <- row_sums(myTDM_trigram)
myTFV_trigram <- sort(myTFV_trigram,decreasing = TRUE)

myTDM_bigram <- TermDocumentMatrix(myCorpus, control = 
                                             list(tokenize = function(x) {NGramTokenizer(x, Weka_control(min = 2, max = 2))},
                                                  tolower = FALSE, removePunctuation = FALSE, removeNumers = FALSE, stopwords = FALSE,
                                                  stemming = FALSE, bounds = list(global = (c(1, Inf))),  wordLengths = c(3, Inf)))

myTFV_bigram <- row_sums(myTDM_bigram)
myTFV_bigram <- sort(myTFV_bigram,decreasing = TRUE)

myTDM_unigram <- TermDocumentMatrix(myCorpus, control = 
                                            list(tokenize = function(x) {NGramTokenizer(x, Weka_control(min = 1, max = 1))},
                                                 tolower = FALSE, removePunctuation = FALSE, removeNumers = FALSE, stopwords = FALSE,
                                                 stemming = FALSE, bounds = list(global = (c(1, Inf))),  wordLengths = c(3, Inf)))

myTFV_unigram <- row_sums(myTDM_unigram)
myTFV_unigram <- sort(myTFV_unigram,decreasing = TRUE)

## 4. Output for storage ##

write.csv(myTFV_trigram, file = ".\\Coursera-SwiftKey\\final\\en_US\\TFV_trigram_sw(-)_(bound(1)).csv")
write.csv(myTFV_bigram, file = ".\\Coursera-SwiftKey\\final\\en_US\\TFV_bigram_sw(-)_(bound(1)).csv")
write.csv(myTFV_unigram, file = ".\\Coursera-SwiftKey\\final\\en_US\\TFV_unigram_sw(-)_(bound(1)).csv")


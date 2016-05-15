## Script to produce n-gram language models from training sets of HC Corpora en_US corpora (i.e. Twitter, Blogs and News) ##
## 1. Retrival of Training Sets.
## 2. Data cleaning (lowercase, removing punctuation and numbers (possible stopword removal and stemming)
## 3. Tokenization to produce TDMs (Term Document Matrix) of n-grams and (n-1)-grams for each corpus ##
## 4. Combination of all three corpora and conversion to cumulative Term Frequency Vectors (TFV) & output for storage ##
## 5. Assembly into models

library(RWeka)
library(tm)
require(SnowballC)
library(slam)

## 1. Retrival of Training Sets ##

## Establish connections to files containing en_US corpora trainint sets ##
con <- list(".\\Coursera-SwiftKey\\final\\en_US\\en_US.twitter-training.txt", 
            ".\\Coursera-SwiftKey\\final\\en_US\\en_US.blogs-training.txt", 
            ".\\Coursera-SwiftKey\\final\\en_US\\en_US.news-training.txt")

## Read in training sets ##

mydata <- lapply(con, readLines, encoding = "UTF-8")
names(mydata) <- c("Twitter", "Blogs", "News")

## Assemble training sets into Corpora using tm package ##

myCorpora <- lapply(lapply(mydata, VectorSource), Corpus)

myCorpora2 <- c(myCorpora[["Twitter"]], myCorpora[["Blogs"]], myCorpora[["News"]]) 
myCorpora2 <- tm_map(myCorpora2, tolower)
myCorpora2 <- tm_map(myCorpora2, removePunctuation)
myCorpora2 <- tm_map(myCorpora2, removeNumbers)
myCorpora2 <- tm_map(myCorpora2, PlainTextDocument, language = "en")

myTDM_trigram2 <- TermDocumentMatrix(myCorpora2, control = 
                                list(tokenize = function(x) {NGramTokenizer(x, Weka_control(min = 3, max = 3))},
                                     tolower = FALSE, removePunctuation = FALSE, removeNumers = FALSE, stopwords = FALSE,
                                     stemming = FALSE, bounds = list(global = (c(10, Inf))),  wordLengths = c(3, Inf)))

myTFV_trigram2 <- row_sums(myTDM_trigram2)
myTFV_trigram2 <- sort(myTFV_trigram2,decreasing = TRUE)

myTDM_bigram2 <- TermDocumentMatrix(myCorpora2, control = 
                                             list(tokenize = function(x) {NGramTokenizer(x, Weka_control(min = 2, max = 2))},
                                                  tolower = FALSE, removePunctuation = FALSE, removeNumers = FALSE, stopwords = FALSE,
                                                  stemming = FALSE, bounds = list(global = (c(10, Inf))),  wordLengths = c(3, Inf)))

myTFV_bigram2 <- row_sums(myTDM_bigram2)
myTFV_bigram2 <- sort(myTFV_bigram2,decreasing = TRUE)

myTDM_unigram2 <- TermDocumentMatrix(myCorpora2, control = 
                                            list(tokenize = function(x) {NGramTokenizer(x, Weka_control(min = 1, max = 1))},
                                                 tolower = FALSE, removePunctuation = FALSE, removeNumers = FALSE, stopwords = FALSE,
                                                 stemming = FALSE, bounds = list(global = (c(10, Inf))),  wordLengths = c(3, Inf)))

myTFV_unigram2 <- row_sums(myTDM_unigram2)
myTFV_unigram2 <- sort(myTFV_unigram2,decreasing = TRUE)

## 2. Data Cleaning & Tokenization ##
## For the sake of efficiency, fucntions: tolower, removePunctuation & removeNumbers are mapped to the documents in the corpora
## before n-gram tokenization (with relevent controls set to FALSE) to avoid repetition. 

## Change to lower case; remove punctuation and numbers ##

myCorpora <- lapply(myCorpora, tm_map, tolower)
myCorpora <- lapply(myCorpora, tm_map, removePunctuation)
myCorpora <- lapply(myCorpora, tm_map, removeNumbers)
myCorpora <- lapply(myCorpora, tm_map, PlainTextDocument, language = "en")

## 3. n-gram Tokenization ##

## Tokenize n-gram TDMs for n = 3, 2, 1.##


myTDM_trigram <- lapply(myCorpora, TermDocumentMatrix, control = 
                                list(tokenize = function(x) {NGramTokenizer(x, Weka_control(min = 3, max = 3))},
                                     tolower = FALSE, removePunctuation = FALSE, removeNumers = FALSE, stopwords = FALSE,
                                     stemming = FALSE, bounds = list(global = (c(10, Inf))),  wordLengths = c(3, Inf)))

myTFV_trigram <- lapply(myTDM_trigram, row_sums)
myTFV_trigram <- lapply(myTFV_trigram, sort, decreasing = TRUE)
myTFV_trigram_comb <- c(myTFV_trigram[["Twitter"]], myTFV_trigram[["Blogs"]], myTFV_trigram[["News"]])


myTDM_bigram <- lapply(myCorpora, TermDocumentMatrix, control = 
                               list(tokenize = function(x) {NGramTokenizer(x, Weka_control(min = 2, max = 2))},
                                    tolower = FALSE, removePunctuation = FALSE, removeNumers = FALSE, stopwords = FALSE,
                                    stemming = FALSE, bounds = list(global = (c(10, Inf))),  wordLengths = c(3, Inf)))

myTFV_bigram <- lapply(myTDM_bigram, row_sums)
myTFV_bigram <- lapply(myTFV_bigram, sort, decreasing = TRUE)
myTFV_bigram_comb <- c(myTFV_bigram[["Twitter"]], myTFV_bigram[["Blogs"]], myTFV_bigram[["News"]])

myTDM_unigram <- lapply(myCorpora, TermDocumentMatrix, control = 
                                list(tokenize = function(x) {NGramTokenizer(x, Weka_control(min = 1, max = 1))},
                                     tolower = FALSE, removePunctuation = FALSE, removeNumers = FALSE, stopwords = FALSE,
                                     stemming = FALSE, bounds = list(global = (c(10, Inf))),  wordLengths = c(3, Inf)))

myTFV_unigram <- lapply(myTDM_unigram, row_sums)
myTFV_unigram <- lapply(myTFV_unigram, sort, decreasing = TRUE)
myTFV_unigram_comb <- c(myTFV_unigram[["Twitter"]], myTFV_unigram[["Blogs"]], myTFV_unigram[["News"]])





## 5. Maximum Likelihood Estimate ##

### qML(wi | wi–2, wi–1)= trigram count/preceding bigram = count(wi–2, wi–1, wi)/count(wi–2, wi–1)
## Function to take first two words of trigram (wi-2, wi-1, wi) to generate corresponding bigram (wi-2, wi-1)##

n_n_1.gram <- function (x) {words <- unlist(strsplit(x, split = " "))
                            n <- length(words)
                            paste(words[1:(n-1)], collapse = " ")}
        
n_unigram <- function (x) {words <- unlist(strsplit(x, split = " "))
                           n <- length(words)
                           words[n]}

## Take combined 4-gram TDM  ##
## Assemble data frame with trigrams and trigram counts from the TDM ##
## Calculate the trigram (wi-3, wi-2, wi-1) corresponding to each 4-gram (wi-3, wi–2, wi–1, wi) ##
## Model designated uvw4 ##

uvw4 <- data.frame(quadgram = names(myTDM_4gram_comb), 
                  quadgramcount = myTDM_4gram_comb,
                  trigram = sapply(names(myTDM_4gram_comb), FUN = quad_trigram),
                  unigram = sapply(names(myTDM_4gram_comb), FUN = quad_unigram),
                  stringsAsFactors = FALSE)

## Assemble data frame with bigrams and bigram counts from the TDM ##
uv4 <- data.frame(trigram = names(myTDM_trigram_comb), trigramcount = myTDM_trigram_comb, stringsAsFactors = FALSE)

## Perform inner join on Uvw & uv data frames by the bigram variable to give single data frame with 4grams, 4ram counts, ##
## corresponding trigram (wi-3, w-2, w-1) and trigram counts ##

detach("package:dplyr", unload=TRUE)
library(plyr)

uvw4 <- join(uvw4, uv4, by = "trigram", type = "inner")

## MLE generated from count(wi-3, wi–2, wi–1, wi)/count(wi-3, wi–2, wi–1) ##

uvw4 <- mutate(uvw4, MLE = (uvw4["quadgramcount"]/uvw4["trigramcount"]))
names(uvw4[ , 6]) <- "MLE"

## Tokenize to produce N-grams ##

library(RWeka)
library(tm)
require(SnowballC)
library(slam)

## Establish connections to files containing en_US corpora trainint sets ##
con <- list(".\\Coursera-SwiftKey\\final\\en_US\\en_US.twitter-training.txt", 
            ".\\Coursera-SwiftKey\\final\\en_US\\en_US.blogs-training.txt", 
            ".\\Coursera-SwiftKey\\final\\en_US\\en_US.news-training.txt")

## Read in training sets ##
mydata <- lapply(con, readLines, encoding = "UTF-8")
names(mydata) <- c("Twitter", "Blogs", "News")

## Assemble training sets into Corpora using tm package ##

myCorpora <- lapply(lapply(mydata, VectorSource), Corpus)

## Data Cleaning ##

myCorpora <- lapply(myCorpora, tm_map, tolower)
myCorpora <- lapply(myCorpora, tm_map, removePunctuation)
myCorpora <- lapply(myCorpora, tm_map, removeNumbers)
myCorpora <- lapply(myCorpora, tm_map, PlainTextDocument, language = "en")

## myCorpora <- lapply(myCorpora, tm_map, removeWords, stopwords("english")) ##
## Stopwords not removed to see if this improves prediction performance ##

## Tokenize n-gram TDMs for n = 1, 2 ,3 ##

myTDM_unigram_nostop <- lapply(myCorpora, TermDocumentMatrix, control = 
                                list(tokenize = function(x) {NGramTokenizer(x, Weka_control(min = 1, max = 1))}))


## 3-gram Tokenize with min = 10 terms to limit matrix size ##
## Logically, if term appears as a trigram n times, its corresponding bigrams will each appear >= n times, 
## so bigram tokenization can also be restricted to min = 10 ##

myTDM_bigram_nostop <- lapply(myCorpora, TermDocumentMatrix, control = 
                               list(tokenize = function(x) {NGramTokenizer(x, Weka_control(min = 2, max = 2))},
                                    bounds = list(global = (c(10, Inf)))))


myTDM_trigram_nostop <- lapply(myCorpora, TermDocumentMatrix, control = 
                                list(tokenize = function(x) {NGramTokenizer(x, Weka_control(min = 3, max = 3))},
                                     bounds = list(global = (c(10, Inf)))))



## Combine TDMs for corpora into single TDM ##

myTDM_trigram_comb_nostop <- c(myTDM_trigram_nostop[["Twitter"]], myTDM_trigram_nostop[["Blogs"]], myTDM_trigram_nostop[["News"]])

myTDM_trigram_comb_nostop_cum <- row_sums(myTDM_trigram_comb_nostop)
myTDM_trigram_comb_nostop_cum <- sort(myTDM_trigram_comb_nostop_cum, decreasing = TRUE)

myTDM_bigram_comb_nostop <- c(myTDM_bigram_nostop[["Twitter"]], myTDM_bigram_nostop[["Blogs"]], myTDM_bigram_nostop[["News"]])

myTDM_bigram_comb_nostop_cum <- row_sums(myTDM_bigram_comb_nostop)
myTDM_bigram_comb_nostop_cum <- sort(myTDM_bigram_comb_nostop_cum, decreasing = TRUE)

## Calculate cumulative frequncies and sort ##

myTDM_unigramcum <- lapply(myTDM_unigram, row_sums)
myTDM_unigramcum <- lapply(myTDM_unigramcum, sort, decreasing = TRUE) 

myTDM_bigramcum <- lapply(myTDM_bigram, row_sums)
myTDM_bigramcum <- lapply(myTDM_bigramcum, sort, decreasing = TRUE) 

myTDM_trigramcum <- lapply(myTDM_trigram, row_sums)
myTDM_trigramcum <- lapply(myTDM_trigramcum, sort, decreasing = TRUE) 



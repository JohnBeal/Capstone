## Tokenize to produce N-grams ##

library(RWeka)
library(tm)
require(SnowballC)


## Establish connections to files containing en_US corpora trainint sets ##
con <- list(".\\Coursera-SwiftKey\\final\\en_US\\en_US.twitter-training.txt", 
            ".\\Coursera-SwiftKey\\final\\en_US\\en_US.blogs-training.txt", 
            ".\\Coursera-SwiftKey\\final\\en_US\\en_US.news-training.txt")

## Read in training sets ##
mydata <- lapply(con, readLines, encoding = "UTF-8")
names(mydata) <- c("Twitter", "Blogs", "News")

## Assemble training sets into Corpora using tm package ##

myCorpora <- lapply(lapply(mydata, VectorSource), Corpus)

myTDM_bigram <- lapply(myCorpora, TermDocumentMatrix, control = 
                               list(tokenize = function(x) {NGramTokenizer(x, Weka_control(min = 2, max = 2))}))

myTDM_bigramcum <- lapply(myTDM_bigram, row_sums)
myTDM_bigramcum <- lapply(myTDM_bigramcum, sort, decreasing = TRUE) 


## Tokenize Corpus object by mapping NgramTokenizer function from RWeka package ##
## Mapping of PlainTextDocument function ensures correct format of Corpus for subsequent tm transformations ##

myCorpora.ngram <- lapply(myCorpora, tm_map, NGramTokenizer, control = Weka_control(min = 2, max = 2))
myCorpora.ngram <- lapply(myCorpora.ngram, tm_map, PlainTextDocument, language = "en")


## Generate TDMs for N-grams ##
myTDM.Ngram <- lapply(myCorpora.ngram, TermDocumentMatrix, control = list(stopwords = TRUE, stemming = TRUE))

## Note, the problem here is that TermDocumentMatrix calls a tokenizer (defaults to words), so is retokenizing n-grams
myTDM.Ngram_cum <- lapply(myTDM.Ngram, row_sums)
myTDM.Ngram_cum <- lapply(myTDM.Ngram_cum, sort, decreasing = TRUE) 

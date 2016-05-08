### Tokenisation ###

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
close(con)

## Assemble training sets into Corpora using tm package ##

myCorpora <- lapply(lapply(mydata, VectorSource), Corpus)


## Tokenize Corpus object by mapping AlphabeticTokenizer function from RWeka package ##
## AlphabeticTokenizer removes punctuation symbols (punctuation, emoticons etc) and tokenizes with low overhead ##
## Removal of punctuation allows for more successful subsequent stopword removal and stemming ## 
## Mapping of PlainTextDocument function ensures correct format of Corpus for subsequent tm transformations ## 

myCorpora <- lapply(myCorpora, tm_map, AlphabeticTokenizer)
myCorpora <- lapply(myCorpora, tm_map, PlainTextDocument, language = "en")





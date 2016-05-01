### Tokenisation ###

library(RWeka)
library(tm)
require(SnowballC)

                                                ## Twitter ##

## Create tm package Corpus object from training set

CorpTwitter_training <- Corpus(VectorSource(training_twitter))

## Tokenize Corpus object by mapping AlphabeticTokenizer function from RWeka package ##
## AlphabeticTokenizer removes punctuation symbols (punctuation, emoticons etc) and tokenizes with low overhead ##
## Removal of punctuation allows for more successful subsequent stopword removal and stemming ## 
## Mapping of PlainTextDocument function ensures correct format of Corpus for subsequent tm transformations ## 

CorpTwitter_training <- tm_map(CorpTwitter_training, AlphabeticTokenizer)
CorpTwitter_training <- tm_map(CorpTwitter_training, PlainTextDocument, language = "en")

## tm TermDocumentMatrix function generates TermDocumentMatrix (TDM) from Corpus ##
## Removal of (English) stopwords is enacted, as well as stemming (SnowBallC package) ##

TDMTwitter_training <- TermDocumentMatrix(CorpTwitter_training, control = list(stopwords = TRUE, stemming = TRUE))


## Frequency Filtering ##
## Generate list of terms which have frequncies of at least 20 in the matrix XX

TDMTwitter_trainingHiFreq <- findFreqTerms(TDMTwitter_training, 20, Inf)

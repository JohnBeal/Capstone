### Tokenisation ###

library(RWeka)
library(tm)
require(SnowballC)


## Create Corpus from Training Set

training_twitterCorpus <- Corpus(VectorSource(training_twitter))


## Create TDM

training_twitterTDM <- TermDocumentMatrix(training_twitterCorpus, control = list(stopwords = TRUE, stemming = TRUE))
training_twitterTDMHiFreq <- findFreqTerms(training_twitterTDM, 10, Inf)


## Tokenization by AlphabeticTokenizer (RWeka) on tm Corpus

#training_tok <- AlphabeticTokenizer(training)#


training_twitter_tok <- tm_map(training_twitterCorpus, AlphabeticTokenizer)
training_twitter_tokPT <- tm_map(training_twitter_tok, PlainTextDocument, language = "en")
training_twitterTDM_tok <- TermDocumentMatrix(training_twitter_tokPT, control = list(stopwords = TRUE, stemming = TRUE))
training_twitterTDMHiFreq2 <- findFreqTerms(training_twitterTDM_tok, 20, Inf)

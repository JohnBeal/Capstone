## Script to produce 4-gram language model from training sets of HC Corpora en_US corpora (i.e. Twitter, Blogs and News) ##
## 1. Retrival of Training Sets.
## 2. Data cleaning (lowercase, removing punctuation and numbers [Note: NO stopword removal, no stemming])
## 3. Tokenization to produce TDMs (Term Document Matrix) of 4-grams and 3-grams for each corpus ##
## 4. Combination of all three corpora and conversion to cumulative TDM ##
## 5. Maximum Liklihood Estimate (MLE) 

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

## 2. Data Cleaning ##

## Change to lower case, remove punctuation and numbers ##
## Stopwords not removed, as this appears detrimental to prediction performance ##

myCorpora <- lapply(myCorpora, tm_map, tolower)
myCorpora <- lapply(myCorpora, tm_map, removePunctuation)
myCorpora <- lapply(myCorpora, tm_map, removeNumbers)
myCorpora <- lapply(myCorpora, tm_map, PlainTextDocument, language = "en")


## 3. n-gram Tokenization ##

## Tokenize n-gram TDMs for n = 3 , 4##
## 3-gram/2-gram tokenized with min = 10 boundary condition ##

myTDM_trigram <- lapply(myCorpora, TermDocumentMatrix, control = 
                               list(tokenize = function(x) {NGramTokenizer(x, Weka_control(min = 3, max = 3))},
                                    bounds = list(global = (c(10, Inf)))))


myTDM_4gram <- lapply(myCorpora, TermDocumentMatrix, control = 
                                list(tokenize = function(x) {NGramTokenizer(x, Weka_control(min = 4, max = 4))},
                                     bounds = list(global = (c(10, Inf)))))

## 4. Combination of corpora into single, cumulative TDM [3-gram & 4-gram] ##

## Combine TDMs for corpora into single TDM (one each for 4-grams and 3-grams) ##
## Take cumulative frequencies accross the corpus and order by frequency ##


myTDM_trigram_comb <- c(myTDM_trigram[["Twitter"]], myTDM_trigram[["Blogs"]], myTDM_trigram[["News"]])

myTDM_trigram_comb <- row_sums(myTDM_trigram_comb)
myTDM_trigram_comb <- sort(myTDM_trigram_comb, decreasing = TRUE)

myTDM_4gram_comb <- c(myTDM_4gram[["Twitter"]], myTDM_4gram[["Blogs"]], myTDM_4gram[["News"]])

myTDM_4gram_comb <- row_sums(myTDM_4gram_comb)
myTDM_4gram_comb <- sort(myTDM_4gram_comb, decreasing = TRUE)

## 5. Maximum Likelihood Estimate ##

### qML(wi | wi–2, wi–1)= trigram count/preceding bigram = count(wi–2, wi–1, wi)/count(wi–2, wi–1)
## Function to take first two words of trigram (wi-2, wi-1, wi) to generate corresponding bigram (wi-2, wi-1)##

quad_trigram <- function (x) {paste(unlist(strsplit(x, split = " "))[1], unlist(strsplit(x, split = " "))[2], 
                                unlist(strsplit(x, split = " "))[3],  sep =" ")}
quad_unigram <- function (x) {unlist(strsplit(x, split = " "))[4]}

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

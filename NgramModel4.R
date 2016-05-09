## Script to produce 3-gram language model from training sets of HC Corpora en_US corpora (i.e. Twitter, Blogs and News) ##
## 1. Retrival of Training Sets.
## 2. Data cleaning (lowercase, removing punctuation and numbers [Note: NO stopword removal, no stemming])
## 3. Tokenization to produce TDMs (Term Document Matrix) of 3-grams and 2-grams for each corpus ##
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

## Tokenize n-gram TDMs for n = 2 ,3 ##
## 3-gram/2-gram tokenized with no min = 1 boundary condition ##

myTDM_bigram <- lapply(myCorpora, TermDocumentMatrix, control = 
                               list(tokenize = function(x) {NGramTokenizer(x, Weka_control(min = 2, max = 2))},
                                    bounds = list(global = (c(1, Inf)))))


myTDM_trigram <- lapply(myCorpora, TermDocumentMatrix, control = 
                                list(tokenize = function(x) {NGramTokenizer(x, Weka_control(min = 3, max = 3))},
                                     bounds = list(global = (c(1, Inf)))))

## 4. Combination of corpora into single, cumulative TDM [2-gram & 3-gram] ##

## Combine TDMs for corpora into single TDM (one each for 3-grams and 2-grams) ##
## Take cumulative frequencies accross the corpus and order by frequency ##


myTDM_trigram_comb <- c(myTDM_trigram[["Twitter"]], myTDM_trigram[["Blogs"]], myTDM_trigram[["News"]])

myTDM_trigram_comb <- row_sums(myTDM_trigram_comb)
myTDM_trigram_comb <- sort(myTDM_trigram_comb, decreasing = TRUE)

myTDM_bigram_comb <- c(myTDM_bigram[["Twitter"]], myTDM_bigram[["Blogs"]], myTDM_bigram[["News"]])

myTDM_bigram_comb <- row_sums(myTDM_bigram_comb)
myTDM_bigram_comb <- sort(myTDM_bigram_comb, decreasing = TRUE)

## 5. Maximum Likelihood Estimate ##

### qML(wi | wi–2, wi–1)= trigram count/preceding bigram = count(wi–2, wi–1, wi)/count(wi–2, wi–1)
## Function to take first two words of trigram (wi-2, wi-1, wi) to generate corresponding bigram (wi-2, wi-1)##

tri_bigram <- function (x) {paste(unlist(strsplit(x, split = " "))[1], unlist(strsplit(x, split = " "))[2], sep =" ")}
tri_unigram <- function (x) {unlist(strsplit(x, split = " "))[3]}

## Take combined 3-gram TDM  ##
## Assemble data frame with trigrams and trigram counts from the TDM ##
## Calculate the bigram (wi-2, wi-1) corresponding to each trigram (wi–2, wi–1, wi) ##
## Model designated uvw3 ##

uvw3 <- data.frame(trigram = names(myTDM_trigram_comb), 
                  trigramcount = myTDM_trigram_comb,
                  bigram = sapply(names(myTDM_trigram_comb), FUN = tri_bigram),
                  unigram = sapply(names(myTDM_trigram_comb), FUN = tri_unigram),
                  stringsAsFactors = FALSE)

## Assemble data frame with bigrams and bigram counts from the TDM ##
uv3 <- data.frame(bigram = names(myTDM_bigram_comb), bigramcount = myTDM_bigram_comb, stringsAsFactors = FALSE)

## Perform inner join on Uvw & uv data frames by the bigram variable to give single data frame with trigrams, trigram counts, ##
## corresponding bigram (w-2, w-1) and bigram counts ##

uvw3 <- join(uvw3, uv3, by = "bigram", type = "inner")

## MLE generated from count(wi–2, wi–1, wi)/count(wi–2, wi–1) ##

uvw3 <- mutate(uvw3, MLE = (uvw3["trigramcount"]/uvw3["bigramcount"]))
names(uvw3[ , 6]) <- "MLE"

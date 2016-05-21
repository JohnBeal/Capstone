## 5. Model Assembly ##

## Function to take first (n-1) words of n-gram ##

n_n_1.gram <- function (x) {words <- unlist(strsplit(x, split = " "))
n <- length(words)
paste(words[1:(n-1)], collapse = " ")}

## Function to take last word of n-gram XX

n_unigram <- function (x) {words <- unlist(strsplit(x, split = " "))
n <- length(words)
words[n]}

## Read in n-gram Term Frequncy Vectors from storage files ##

TFV_trigram <- read.csv(file = ".\\Coursera-SwiftKey\\final\\en_US\\TFV_trigram_sw(-)_(bound(10)).csv", stringsAsFactors = F)
names(TFV_trigram) <- c("trigram", "frequency")

TFV_bigram <- read.csv( file = ".\\Coursera-SwiftKey\\final\\en_US\\TFV_bigram_sw(-)_(bound(10)).csv", stringsAsFactors = F)
names(TFV_bigram) <- c("bigram", "frequency")

TFV_unigram <- read.csv(file = ".\\Coursera-SwiftKey\\final\\en_US\\TFV_unigram_sw(-)_(bound(10)).csv", stringsAsFactors = F)
names(TFV_unigram) <- c("unigram", "frequency")

## Trigram  ##

uvw_trigram_pruned <- data.frame(n.gram = TFV_trigram[, "trigram"], 
                                 n.gram_count = TFV_trigram[, "frequency"],
                                 n_1.gram = sapply(TFV_trigram[, "trigram"], n_n_1.gram), 
                                 unigram = sapply(TFV_trigram[, "trigram"], n_unigram),
                                 stringsAsFactors = FALSE, row.names = NULL)


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

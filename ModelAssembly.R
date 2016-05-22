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

## Trigram - initial   ##

uvw_trigram_pruned <- data.frame(n.gram = TFV_trigram[, "trigram"], 
                                 n.gram_count = TFV_trigram[, "frequency"],
                                 n_1.gram = sapply(TFV_trigram[, "trigram"], n_n_1.gram), 
                                 ##unigram = sapply(TFV_trigram[, "trigram"], n_unigram),##
                                 stringsAsFactors = FALSE, row.names = NULL)


## Bigram - initial XX

uvw_bigram_pruned <- data.frame(n.gram = TFV_bigram[, "bigram"],
                                n.gram_count = TFV_bigram[, "frequency"],
                                n_1.gram = sapply(TFV_bigram[, "bigram"], n_n_1.gram),
                                ##unigram = sapply(TFV_bigram[, "bigram"], n_)unigram,##
                                stringsAsFactors = FALSE, row.names = NULL) 


## Unigram - initial ##

uvw_unigram_pruned <- data.frame(n.gram = TFV_unigram[, "unigram"],
                                 n.gram_count = TFV_unigram[, "frequency"],
                                 stringsAsFactors = FALSE, row.names = NULL)


## Bigram - final ##

names(TFV_unigram) <- c("n_1.gram", "n_1.gram_count")
uvw_bigram_pruned <- join(uvw_bigram_pruned, TFV_unigram, by = "n_1.gram", type = "inner")
uvw_bigram_pruned <- mutate(uvw_bigram_pruned, unigram = sapply(uvw_bigram_pruned[, "n.gram"], n_unigram))


## Trigram - final ##

names(TFV_bigram) <- c("n_1.gram", "n_1.gram_count")
uvw_trigram_pruned <- join(uvw_trigram_pruned, TFV_bigram, by = "n_1.gram", type = "inner")
uvw_trigram_pruned <- mutate(uvw_trigram_pruned, unigram = sapply(uvw_trigram_pruned[, "n.gram"], n_unigram))

## Probablity Estimate  - MLE ##

uvw_trigram_pruned_MLE <- mutate(uvw_trigram_pruned, prob.estimate = (n.gram_count/n_1.gram_count))

uvw_bigram_pruned_MLE <- mutate(uvw_bigram_pruned, prob.estimate = (n.gram_count/n_1.gram_count))

uvw_unigram_pruned_MLE <- mutate(uvw_unigram_pruned, prob.estimate = (n.gram_count/(sum(n.gram_count))))

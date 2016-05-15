## 5. Model Assembly ##

## Function to take first two words of trigram (wi-2, wi-1, wi) to generate corresponding bigram (wi-2, wi-1)##

n_n_1.gram <- function (x) {words <- unlist(strsplit(x, split = " "))
                            n <- length(words)
                            paste(words[1:(n-1)], collapse = " ")}

n_unigram <- function (x) {words <- unlist(strsplit(x, split = " "))
                           n <- length(words)
                           words[n]}

## Trigram  ##

uvw_trigram_pruned <- data.frame(n.gram = TFV_trigram_pruned["trigram"], 
                                 n.gram_count = TFV_trigram_pruned["frequency"],
                                 stringsAsFactors = FALSE)
colnames(uvw_trigram_pruned) <- c("n.gram", "n.gram_count")


uvw_trigram_pruned <- mutate(uvw_trigram_pruned, n_1.gram = n_n_1.gram(n.gram))

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

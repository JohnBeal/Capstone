## 5. Model Assembly ##

## Function to take first two words of trigram (wi-2, wi-1, wi) to generate corresponding bigram (wi-2, wi-1)##

n_n_1.gram <- function (x) {words <- unlist(strsplit(x, split = " "))
n <- length(words)
paste(words[1:(n-1)], collapse = " ")}

n_unigram <- function (x) {words <- unlist(strsplit(x, split = " "))
n <- length(words)
words[n]}

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

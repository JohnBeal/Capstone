library(plyr)
library(dplyr)

### qML(wi | wi–2, wi–1)= trigram count/preceding bigram = count(wi–2, wi–1, wi)/count(wi–2, wi–1)


## Function to take first two words of trigram (wi-2, wi-1, wi) to generate corresponding bigram (wi-2, wi-1)##

tri_bigram <- function (x) {paste(unlist(strsplit(x, split = " "))[1], unlist(strsplit(x, split = " "))[2], sep =" ")}
tri_unigram <- function (x) {unlist(strsplit(x, split = " "))[3]}


## Take combined 3-gram TDM (with lower bound = 10) ##
## Assemble data frame with trigrams and trigram counts from the TDM ##
## Calculate the bigram (wi-2, wi-1) corresponding to each trigram (wi–2, wi–1, wi) ##

uvw2 <- data.frame(trigram = names(myTDM_trigram_comb_nostop_cum), 
                  trigramcount = myTDM_trigram_comb_nostop_cum,
                  bigram = sapply(names(myTDM_trigram_comb_nostop_cum), FUN = tri_bigram),
                  unigram = sapply(names(myTDM_trigram_comb_nostop_cum), FUN = tri_unigram),
                  stringsAsFactors = FALSE)

## Take combined 2-gram TDM (with lower bound = 10) ##
## Assemble data frame with bigrams and bigram counts from the TDM ##


uv2 <- data.frame(bigram = names(myTDM_bigram_comb_nostop_cum), bigramcount = myTDM_bigram_comb_nostop_cum, stringsAsFactors = FALSE)

## Perform inner join on Uvw & uv data frames by the bigram variable to give single data frame with trigrams, trigram counts, ##
## corresponding bigram (w-2, w-1) and bigram counts ##

uvw2 <- join(uvw2, uv2, by = "bigram", type = "inner")

## MLE 3-gram model ##

uvw2 <- mutate(uvw2, MLE = (uvw2["trigramcount"]/uvw2["bigramcount"]))
names(uvw2[ , 6]) <- "MLE"

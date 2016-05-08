### qML(wi | wi–2, wi–1)= trigram count/preceding bigram = count(wi–2, wi–1, wi)/count(wi–2, wi–1)


##qML(wi|wi-1) = bigram count/unigram count = count(wi–1, wi)/count(wi–1)

myTDM_bigram[["News"]]$dimnames$Terms

##Practice on Twitter corpus##

uvw <- myTDM_trigramcum[["Twitter"]]

uv <- myTDM_bigramcum[["Twitter"]]

uvw <- data.frame(trigram = names(myTDM_trigram_comb_cum), 
                  trigramcount = myTDM_trigram_comb_cum,stringsAsFactors = FALSE)

uvw <- data.frame(trigram = names(myTDM_trigramcum[["Twitter"]]), 
                  trigramcount = myTDM_trigramcum[["Twitter"]], bigram = lapply(names(myTDM_trigramcum[["Twitter"]]), tri_bigram),
                  stringsAsFactors = FALSE)

## Function to take first two words of trigram (wi-2, wi-1, wi) to generate corresponding bigram (wi-2, wi-1)##

tri_bigram <- function (x) {paste(unlist(strsplit(x, split = " "))[1], unlist(strsplit(x, split = " "))[2], sep =" ")}


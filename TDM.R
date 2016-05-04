library(tm)
library(slam)


## tm TermDocumentMatrix function generates TermDocumentMatrix (TDM) from Corpus ##
## Removal of (English) stopwords is enacted, as well as stemming (SnowBallC package) ##

myTDM <- lapply(myCorpora, TermDocumentMatrix, control = list(stopwords = TRUE, stemming = TRUE))

## From TDMs generate matrices containing cumulative term frequencies ##

myTDM_cum <- lapply(myTDM, row_sums)

## Sort cumulative TDMs by frequency ##

myTDM_cum <- lapply(myTDM_cum, sort, decreasing = TRUE) 

## Convert cumulative TDMs to data frames for graphing ##

myTDM_cum <- lapply(myTDM_cum, data.frame)
myTDM_cum <- lapply(myTDM_cum, "colnames<-", "cum.frequency")



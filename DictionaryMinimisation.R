## Minimal Dictionary Determination ##
## Count-based evaluation of term frequencies used to find minimal term dictionaries to cover 
## large proportions of word instances in corpus ##

library(tm)

## Convert cumulative TDMs to data frames for graphing ##

myTDM_cum <- lapply(myTDM_cum, data.frame)
myTDM_cum <- lapply(myTDM_cum, "colnames<-", "cum.frequency")


## Closest: function to find index of element closest to desired value (by finding minimum difference) ##  

closest <- function (x, y) {
        which(abs(x-y)==min(abs(x-y)))
        
}

## Cumulative relative frequency ##
## Calculated by dividing cumulative summed frquency by total frequency

myTDM_relcum <- lapply(myTDM_cum, function (x) {cumsum(x)/sum(x)} )


## Find index of element which corresponds to resired cumulative relative frequency ## 

##50%##
lapply(myTDM_relcum, closest, 0.5)
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
## Calculated by dividing cumulative summed frequency by total frequency

myTDM_relcum <- lapply(myTDM_cum, function (x) {cumsum(x)/sum(x)} )


## Find index of element in cumulative relative frequncy TDM which corresponds to desired boundary condition ## 
## and extract corresponding cumulative frequency from cumulative TDM##

dictionary_50 <- mapply(FUN = findFreqTerms, myTDM, 
       lowfreq = (mapply(FUN = "[", data = myTDM_cum, sapply(myTDM_relcum, closest, 0.5), "cum.frequency")))

mapply(FUN = "[", data = myTDM_cum, sapply(myTDM_relcum, closest, 0.5), "cum.frequency")



mapply(FUN = "[", data = myTDM_cum, sapply(myTDM_relcum, closest, 0.1), "cum.frequency")




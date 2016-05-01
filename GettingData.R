## Set directory path ##
path = ".\\Coursera-SwiftKey\\final\\en_US"
set.seed(9178)


## Sample corpus files ##
## Take 10% of each corpus as sample, further split in to training and validation sets, and output in files for later use ##


                                ## Twitter ##

con <- file(description = paste(path, "en_US.twitter.txt", sep = "\\"), encoding = "UTF-8")
sample <- sample(x  = readLines(con = con), size = 0.1*length(readLines(con = con)), replace = FALSE)
close(con = con)
inTrain <- as.logical(rbinom(n = length(sample), size = 1, p = 0.8))

### Training Set ### 
training <- sample[inTrain]
con <- file(description = paste(path, "en_US.twitter-training.txt", sep = "\\"), encoding = "UTF-8")
writeLines(text = training, con = con)
close(con = con)


### Test Set ###
testing <- sample[!inTrain]
con <- file(description = paste(path, "en_US.twitter-testing.txt", sep = "\\"), encoding = "UTF-8")
writeLines(text = testing, con = con)
close(con = con)

                                ## Blogs ##

con <- file(description = paste(path, "en_US.blogs.txt", sep = "\\"), encoding = "UTF-8")
sample <- sample(x  = readLines(con = con), size = 0.1*length(readLines(con = con)), replace = FALSE)
close(con = con)
inTrain <- as.logical(rbinom(n = length(sample), size = 1, p = 0.8))

### Training Set ### 
training <- sample[inTrain]
con <- file(description = paste(path, "en_US.blogs-training.txt", sep = "\\"), encoding = "UTF-8")
writeLines(text = training, con = con)
close(con = con)


### Test Set ###
testing <- sample[!inTrain]
con <- file(description = paste(path, "en_US.blogs-testing.txt", sep = "\\"), encoding = "UTF-8")
writeLines(text = testing, con = con)
close(con = con)
 

                                ## News ##

con <- file(description = paste(path, "en_US.news.txt", sep = "\\"), encoding = "UTF-8")
sample <- sample(x  = readLines(con = con), size = 0.1*length(readLines(con = con)), replace = FALSE)
close(con = con)
inTrain <- as.logical(rbinom(n = length(sample), size = 1, p = 0.8))

### Training Set ### 
training <- sample[inTrain]
con <- file(description = paste(path, "en_US.news-training.txt", sep = "\\"), encoding = "UTF-8")
writeLines(text = training, con = con)
close(con = con)


### Test Set ###
testing <- sample[!inTrain]
con <- file(description = paste(path, "en_US.news-testing.txt", sep = "\\"), encoding = "UTF-8")
writeLines(text = testing, con = con)
close(con = con)


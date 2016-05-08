## Set directory path ##
path = ".\\Coursera-SwiftKey\\final\\en_US"
set.seed(9178)


## Sample corpus files ##
## Take 10% of each corpus as sample, further split in to training and validation sets, and output in files for later use ##


# Create list of file connections to corpora ##
con <- list(".\\Coursera-SwiftKey\\final\\en_US\\en_US.twitter.txt", 
            ".\\Coursera-SwiftKey\\final\\en_US\\en_US.blogs.txt", 
            ".\\Coursera-SwiftKey\\final\\en_US\\en_US.news.txt")

## Read data in from corpora line-by-line ##
mydata <- lapply(con, readLines, encoding = "UTF-8", skipNul = TRUE)
names(mydata) <- c("Twitter", "Blogs", "News")


## Sample 10% of each corpus without replacement ##

mydata <- mapply(FUN = sample, mydata, size = (0.1*sapply(mydata, length)), replace = FALSE)

## Split into training (80%) and validation sets (20%) ##


inTrain <- lapply(mydata, function (x) {as.logical(rbinom(n = length(x), size = 1, p = 0.8))})


## Twitter ##

writeLines(text = mydata[["Twitter"]][inTrain[["Twitter"]]], 
           con = file(description = ".\\Coursera-SwiftKey\\final\\en_US\\en_US.twitter-training.txt", encoding = "UTF-8"))

writeLines(text = mydata[["Twitter"]][!inTrain[["Twitter"]]], 
           con = file(description = ".\\Coursera-SwiftKey\\final\\en_US\\en_US.twitter-testing.txt", encoding = "UTF-8"))

## Blogs ##

writeLines(text = mydata[["Blogs"]][inTrain[["Blogs"]]], 
           con = file(description = ".\\Coursera-SwiftKey\\final\\en_US\\en_US.blogs-training.txt", encoding = "UTF-8"))

writeLines(text = mydata[["Blogs"]][!inTrain[["Blogs"]]], 
           con = file(description = ".\\Coursera-SwiftKey\\final\\en_US\\en_US.blogs-testing.txt", encoding = "UTF-8"))


## News ##

writeLines(text = mydata[["News"]][inTrain[["News"]]], 
           con = file(description = ".\\Coursera-SwiftKey\\final\\en_US\\en_US.news-training.txt", encoding = "UTF-8"))

writeLines(text = mydata[["News"]][!inTrain[["News"]]], 
           con = file(description = ".\\Coursera-SwiftKey\\final\\en_US\\en_US.news-testing.txt", encoding = "UTF-8"))


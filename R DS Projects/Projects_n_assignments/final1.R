rm(list=ls())

setwd("C:/Users/Gracy/Coursera - Data Science Specialization/Course 10 - Capstone/Week 1/Coursera-SwiftKey/final/en_US")

library(doParallel)
library(data.table)
library(text2vec)

parallelizeTask <- function(task, ...) {
        # Calculate the number of cores
        ncores <- detectCores() - 1
        # Initiate cluster
        cl <- makeCluster(ncores)
        registerDoParallel(cl)
        #print("Starting task")
        r <- task(...)
        #print("Task done")
        stopCluster(cl)
        r
}

read_file <- function(input_file_name){
        file_data <- parallelizeTask(fread,input_file_name,sep='\n',header = FALSE,data.table = FALSE, col.names = c("text") )
}

twitter_file <- read_file("en_US.twitter.txt")

#news_file <- read_file("en_US.news.txt")
#blog_file <- read_file("en_US.blogs.txt")

#twitter_file <- read_file("hello.txt")
#news_file <- read_file("hello2.txt")
#blog_file <- read_file("hello3.txt")


ncores <- detectCores() - 1
cl <- makeCluster(ncores)
registerDoParallel(cl)




## Function to substitute english shorten sentences
replaceShortEnglish <- function(t){
        t <- gsub(pattern = "can't", replacement = " cannot", t)
        t <- gsub(pattern = "'m", replacement = " am", t)
        t <- gsub(pattern = "ain't", replacement = "am not", t)
        t <- gsub(pattern = "'re", replacement = " are", t)
        t <- gsub(pattern = "'ve", replacement = " have", t)
        t <- gsub(pattern = "'d", replacement = " would", t)
        t <- gsub(pattern = "'ll", replacement = " will", t)
        t <- gsub(pattern = "n't", replacement = " not", t)
        t <- gsub(pattern = "what's", replacement = "what is", t)
        t <- gsub(pattern = "won't", replacement = "will not", t)
        return(t)
}
## Function to clean twitter like content, url, RT and hastags
untwitter <- function(t){
        ## Remove URL
        t <- gsub('http\\S+\\s*', '', t)
        ## Remove hastags
        t <- gsub('#\\S+\\s*', '', t)
        # Remove "rt"
        t <- gsub('\\brt\\b', '', t)
        # Remove "rt"
        t <- gsub('\\bu\\b', ' you ', t)
        return(t)
}
## Function to remove all characters except english alphabetic and quotes, hypens, numbers
cleanText <- function(t) {
        t <- gsub("[^a-z -\\']", "", t)
        t <- gsub("[[:digit:]]", "", t)
        return(t)
}


## Our tokenizer function
stem_tokenizer <- function(v) {
        v %>%
        untwitter %>%
        replaceShortEnglish %>%
        cleanText 
        # poerter stemmer
        #  %>% lapply(wordStem, 'en')
}


tokens_twitter <-  twitter_file %>%
        tolower %>%
        word_tokenizer %>%
        stem_tokenizer

#names(tokens) <- c("blog_file", "news_file", "twitter_file")

swC <- c(stopwords("en"), badWords) 


## Create the iterator on the tokens
it1 <- itoken(tokens_twitter, progessbar = FALSE,n_chunks = 1000)
## Generate the vocabulary using the iterator, 1-gram badwords / stopwords list.
#vocab1 <- create_vocabulary(it, ngram = c(ngram_min = 1L, ngram_max = 1L), stopwords = swC)
vocab1 <- create_vocabulary(it1, ngram = c(ngram_min = 1L, ngram_max = 1L))


## We need to reinitialise iterator : it
it1 <- itoken(tokens_twitter,n_chunks = 1000)
## Here we create dtm directly:
v_vectorizer <- vocab_vectorizer(vocab1)
#{ sink("/dev/null"); dtm1 <- create_dtm(it, v_vectorizer); sink()}
dtm1 <- create_dtm(it1, v_vectorizer)


## Save dtm1 R object
#save(dtm1, file = '../../data/Rda/dtm1.Rda')

## Function which change a named vector of tokens to a sorted dataframe
vector2TopDF <- function(v){
        v <- v[order(v, decreasing = T)][1:1000]
        v <- data.frame(features = names(v),
                        freq = v, row.names = NULL)
        v <- transform(v, features = reorder(features,freq))
        ## Order factor by freq
        v$features <-factor(v$features, levels=v[order(v$freq), "features"])
        return(v)
}

## Extract the top 1000 most frequent concepts
wordsFreq <- colSums(as.matrix(dtm1))
top1_twitter <- vector2TopDF(wordsFreq[order(wordsFreq, decreasing = T)][1:10000])


## Extract the top 1000 most frequent concepts in blogs
#blogFreq <- dtm1[rownames(dtm1) == "blog_file",]
#top1blogs <- vector2TopDF(blogFreq[order(blogFreq, decreasing = T)][1:1000])
## Extract the top 1000 most frequent concepts in news
#newsFreq <- dtm1[rownames(dtm1) == "news_file",]
#top1news <- vector2TopDF(newsFreq[order(newsFreq, decreasing = T)][1:1000])
## Extract the top 1000 most frequent concepts in blogs
#twitterFreq <- dtm1[rownames(dtm1) == "twitter_file",]
#top1twitter <- vector2TopDF(twitterFreq[order(twitterFreq, decreasing = T)][1:1000])


## clean space
rm(dtm1,vocab1)
gg <- gc(reset = TRUE)


stopCluster(cl)


#tracker <- rbind(tracker, data.frame(operation = "Create Vocabulary / DTM Unigram - text2vec", time = Sys.time(),free.ram.Mo = getRam()))

#put itoken inside parrallel fun

#https://rstudio-pubs-static.s3.amazonaws.com/188264_a2fc6f4f4f94471a9d4bd99cfde3638a.html
#https://rpubs.com/sujith/text2vec
#http://dsnotes.com/post/text2vec/
        

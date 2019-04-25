#https://rpubs.com/DMW29/194887

rm(list=ls())

setwd("C:/Users/Gracy/Coursera - Data Science Specialization/Course 10 - Capstone/Week 1/Coursera-SwiftKey/final/en_US")

library(doParallel)
library(data.table)
library(text2vec)
library(tidytext)

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


## Function which change a named vector of tokens to a sorted dataframe
vector2TopDF <- function(v){
        v <- v[order(v, decreasing = T)][1:100]
        v <- data.frame(features = names(v),
                        freq = v, row.names = NULL)
        v <- transform(v, features = reorder(features,freq))
        ## Order factor by freq
        v$features <-factor(v$features, levels=v[order(v$freq), "features"])
        return(v)
}


read_file <- function(input_file_name){
        file_data <- parallelizeTask(fread,input_file_name,sep='\n',header = FALSE,data.table = FALSE, col.names = c("text"),nrows=50000 )
}

twitter_file <- read_file("en_US.twitter.txt")

#news_file <- read_file("en_US.news.txt")
#blog_file <- read_file("en_US.blogs.txt")



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
        t <- gsub("[^a-z _ -\\']", "", t)
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

swC <- c(stopwords("en"), badWords) 




chunk <- 1000000
n <- nrow(twitter_file)
r  <- rep(1:ceiling(n/chunk),each=chunk)[1:n]
d <- split(twitter_file,r)


unigram_twitter <- data.frame(features = as.factor(character()),freq = numeric(0))
bigram_twitter <- data.frame(features = as.factor(character()),freq = numeric(0))
trigram_twitter <- data.frame(features = as.factor(character()),freq = numeric(0))

for (splits in 1:length(d)) {
        print("Processing a chunck")
        ncores <- detectCores() - 1
        cl <- makeCluster(ncores)
        registerDoParallel(cl)
        
        tokens_twitter <-  twitter_file %>%
                tolower %>%
                word_tokenizer %>%
                stem_tokenizer
        
        ## Create the iterator on the tokens
        it1 <- itoken(tokens_twitter, progessbar = FALSE,n_chunks = 100)
        ## Generate the vocabulary using the iterator, 1-gram badwords / stopwords list.
        #vocab1 <- create_vocabulary(it, ngram = c(ngram_min = 1L, ngram_max = 1L), stopwords = swC)
        vocab1 <- create_vocabulary(it1, ngram = c(ngram_min = 1L, ngram_max = 1L))
        
        ## We need to reinitialise iterator : it
        it1 <- itoken(tokens_twitter,n_chunks = 100)
        ## Here we create dtm directly:
        v_vectorizer <- vocab_vectorizer(vocab1)
        #{ sink("/dev/null"); dtm1 <- create_dtm(it, v_vectorizer); sink()}
        dtm1 <- create_dtm(it1, v_vectorizer)
        DF <- tidy(dtm1)
        
        ## Extract the top 1000 most frequent concepts
        wordsFreq <- colSums(as.matrix(dtm1))
        top1_twitter <- vector2TopDF(wordsFreq[order(wordsFreq, decreasing = T)][1:100])
        unigram_twitter <- rbind(unigram_twitter,top1_twitter)
        
        
        
        #bigram
        vocab2 <- create_vocabulary(it1, ngram = c(ngram_min = 2L, ngram_max = 2L))
        v_vectorizer <- vocab_vectorizer(vocab2)
        dtm2 <- create_dtm(it1, v_vectorizer)
        DF2 <- tidy(dtm2)
        
        wordsFreq <- colSums(as.matrix(dtm2))
        top2_twitter <- vector2TopDF(wordsFreq[order(wordsFreq, decreasing = T)][1:100])
        bigram_twitter <- rbind(bigram_twitter,top2_twitter)
        
        #trigram
        vocab3 <- create_vocabulary(it1, ngram = c(ngram_min = 3L, ngram_max = 3L))
        v_vectorizer <- vocab_vectorizer(vocab3)
        dtm3 <- create_dtm(it1, v_vectorizer)
        DF3 <- tidy(dtm3)
        
        wordsFreq <- colSums(as.matrix(dtm3))
        top3_twitter <- vector2TopDF(wordsFreq[order(wordsFreq, decreasing = T)][1:100])
        trigram_twitter <- rbind(trigram_twitter,top3_twitter)
        
        
        ## clean space
        rm(dtm1,vocab1)
        rm(dtm2,vocab2)
        rm(dtm3,vocab3)
        gg <- gc(reset = TRUE)
        
        
        
        
        
        stopCluster(cl)
        
        
}


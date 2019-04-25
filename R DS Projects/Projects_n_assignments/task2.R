#Tasks to accomplish

#Exploratory analysis - perform a thorough exploratory analysis of the data,
#understanding the distribution of words and relationship between the words in the corpora.


#Understand frequencies of words and word pairs - build figures and tables 
#to understand variation in the frequencies of words and word pairs in the data.
#Questions to consider

#Some words are more frequent than others - what are the distributions of word frequencies?
#What are the frequencies of 2-grams and 3-grams in the dataset?
#How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?
#How do you evaluate how many of the words come from foreign languages?
#Can you think of a way to increase the coverage -- 
#identifying words that may not be in the corpora or using a 
#smaller number of words in the dictionary to cover the same number of phrases?

#a summary of the en_US data-set (number of lines, number of words).

#at least one plot to illustrate the data. Usually people show the frequency count of the most frequent word 
#and most frequent bi-grams, and for some unknown reason: a word cloud.

#You don't have to build a model and you won't loose point if you don't describe a model strategy.
#But if you write down a plan some people might give you feedback and advice.

#The most important criteria for the report is to make it readable for non-data scientists.

setwd("C:/Users/Gracy/Coursera - Data Science Specialization/Course 10 - Capstone/Week 1/Coursera-SwiftKey/final/en_US")

fileInformation <- function(fileName,pattern) {
        
        
        conn <- file(fileName, "rb")
        fulltext <- readLines(conn,skipNul = TRUE)
        str(fulltext)
        
        
        print("Details: ")
        print(fileName)
        
        #Create Corpus
        vs <- VectorSource(fulltext)
        mycorpus <- Corpus(vs, readerControl=list(readPlain, language="en", load=TRUE))
        
        
        print(mycorpus)
        
        summary(mycorpus)
        
        dtm <- DocumentTermMatrix(docs)
        
        print(dtm)
        
        freq <- colSums(as.matrix(dtm))
        
        length(freq)
        
        ord <- order(freq,decreasing=TRUE)
        
        freq[head(ord)]
        
        freq[tail(ord)]  
        
        
        close(conn)
        
}

blog_info <- fileInformation("en_US.blogs.txt")
news_info <- fileInformation("en_US.news.txt")
twit_info <- fileInformation("en_US.twitter.txt")


#Try using quanteda

library(quanteda)
library(readtext)

mytwitter <- readtext("sample.txt")
mytwitterblog <- readtext("en_US.blogs.txt")
mytwitternews <- readtext("en_US.news.txt")
mytwittertext <- readtext("en_US.twitter.txt")

myCorpusTwitter <- corpus(mytwitter)


myCorpusTwitterblog <- corpus(mytwitterblog)
myCorpusTwitternews <- corpus(mytwitternews)
myCorpusTwittertext <- corpus(mytwittertext)

mainCorpus <- myCorpusTwitterblog + myCorpusTwitternews + myCorpusTwittertext

summary(myCorpusTwitter, 5)

#Try sampling now

tokenInfo <- summary(myCorpusTwitter)


tokenInfo[which.max(tokenInfo$Tokens), ] 



options(width = 200)
kwic(myCorpusTwitter, "love")

head(docvars(myCorpusTwitter))

toks <- tokens(myCorpusTwitter)

tokens_ngrams(toks, n = 1:3)


myDfm <- dfm(myCorpusTwitter)
myDfm[, 1:5]


myStemMat <- dfm(myCorpusTwitter, remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)
myStemMat[, 1:5]




topfeatures(myStemMat, 20)

set.seed(100)
textplot_wordcloud(myStemMat, min.freq = 6, random.order = FALSE,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))


loveSimi <- textstat_simil(myStemMat, c("love" , "hate"), 
                             margin = "documents", method = "cosine")


getProfanityWords <- function(corpus) {
        profanityFileName <- "profanity.txt"
        if (!file.exists(profanityFileName)) {
                profanity.url <- "https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
                download.file(profanity.url, destfile = profanityFileName, method = "curl")
        }
        
        if (sum(ls() == "profanity") < 1) {
                profanity <- read.csv(profanityFileName, header = FALSE, stringsAsFactors = FALSE)
                profanity <- profanity$V1
                profanity <- profanity[1:length(profanity)-1]
        }
        
        profanity
}


tokenize(myStemMat, remove_punct = TRUE, ngrams = 2)

library(doParallel)
library(quanteda)

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

f1 <- parallelizeTask(readtext,"sample.txt")

makeSentences <- function(input) {
        output <- tokens(input, what = "sentence", removeNumbers = TRUE,
                           removePunct = TRUE, removeSeparators = TRUE,
                           removeTwitter = TRUE, removeHyphens = TRUE)
        #output <- removeFeatures(output, getProfanityWords())
        #unlist(lapply(output, function(a) paste('#s#', toLower(a), '#e#')))
}

makeTokens <- function(input, n = 1L) {
        tokens(input, what = "word", removeNumbers = TRUE,
                 removePunct = TRUE, removeSeparators = TRUE,
                 removeTwitter = FALSE, removeHyphens = TRUE,
                 ngrams = n, simplify = TRUE)
}


mytwitter <- readtext("sample.txt")
tweets <- readLines("sample.txt")
myCorpusTwitter <- corpus(mytwitter)

subset <- parallelizeTask(sample,tweets, size=length(tweets)*.01, replace=FALSE)

sentences <- parallelizeTask(makeSentences, myCorpusTwitter)
ngram1 <- parallelizeTask(makeTokens, sentences, 1)
ngram2 <- parallelizeTask(makeTokens, sentences, 2)


dfm1 <- parallelizeTask(dfm, ngram1)
dfm2 <- parallelizeTask(dfm, ngram2)

dt1 <- data.table(ngram = topfeatures(dfm1), count = colSums(dfm1), key = "ngram")
dt2 <- data.table(ngram = topfeatures(dfm2), count = colSums(dfm2), key = "ngram")
# Store the total number of ngrams (features in quanteda terminology) for later use
nfeats <- nfeature(dfm1)

docfreq(dfm2[, 1:20])


library(text2vec)
create_vocabulary(ngram2)


DF <- as.data.frame(dfm1)
MX <- t(DF)
# split the current column names by '_'
colsSplit <- strsplit(colnames(DF),'_')
# replicate the rows of the matrix and give them the new split row names
MX <-MX[unlist(lapply(1:length(colsSplit),function(idx) rep(idx,length(colsSplit[[idx]])))),]
rownames(MX) <- unlist(colsSplit)
# aggregate the matrix rows having the same name and transpose again
MX2 <- t(do.call(rbind,by(MX,rownames(MX),colSums)))
# turn the matrix into a dfm
res.dfm <- as.dfm(MX2)



dfm_new <- parallelizeTask(dfm,myCorpusTwitter,remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)

top20 <- topfeatures(dfm_new, 20)

library(ggplot2)
library(reshape2)
dfplot <- as.data.frame(melt(top20))
dfplot$word <- dimnames(dfplot)[[1]]
dfplot$word <- factor(dfplot$word,
                      levels=dfplot$word[order(dfplot$value,
                                               decreasing=TRUE)])

fig <- ggplot(dfplot, aes(x=word, y=value)) + geom_bar(stat="identity")
fig <- fig + xlab("Word in Corpus")
fig <- fig + ylab("Count")
print(fig)



dfm_new_ngram2 <- parallelizeTask(dfm,myCorpusTwitter,remove = stopwords("english"), stem = TRUE,ngrams=2, remove_punct = TRUE)

top20ngram2 <- topfeatures(dfm_new_ngram2, 20)

dfplot <- as.data.frame(melt(top20ngram2))
dfplot$word <- dimnames(dfplot)[[1]]
dfplot$word <- factor(dfplot$word,
                      levels=dfplot$word[order(dfplot$value,
                                               decreasing=TRUE)])

fig <- ggplot(dfplot, aes(x=word, y=value)) + geom_bar(stat="identity")
fig <- fig + xlab("Word in Corpus")
fig <- fig + ylab("Count")
print(fig)
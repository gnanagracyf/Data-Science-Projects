## Check for required packages and install if need be.
#packages<-function(x){
#        x<-as.character(match.call()[[2]])
#        if (!require(x,character.only=TRUE)){
#                install.packages(pkgs=x,repos="http://cran.r-project.org")
#                require(x,character.only=TRUE)
#        }
#}

library(tm)
library(stringr)
#packages(RWeka)
library(plyr)
library(doParallel)
library(data.table)
library(text2vec)
library(tidytext)
library(tidytext)
library(dplyr)


setwd("C:/Users/Gracy/Coursera - Data Science Specialization/Course 10 - Capstone/Capstone")


#textFiles <- c("en_US.twitter.txt", "en_US.blogs.txt", "en_US.news.txt")


read_file <- function(input_file_name){
        file_data <- parallelizeTask(fread,input_file_name,sep='\n',header = FALSE,data.table = FALSE, col.names = c("text"),nrows=500 )
}

twitter_file <- read_file("en_US.twitter.txt")
blog_file <- read_file("en_US.blogs.txt")
news_file <- read_file("en_US.news.txt")



## Clean text samples after joining the three text source file samples
## The order in which the text are cleaned does matter
#badWordsList <- readLines("Bad Word List.csv", n=-1, skipNul = TRUE)

groupText <- c(twitter_file, blog_file, news_file)
rm(twitter_file, blog_file, news_file)
gc(verbose = FALSE)



groupText <- iconv(groupText, "latin1", "ASCII", sub="")
groupText <- tolower(groupText)
groupText <- removeNumbers(groupText)
groupText <- removePunctuation(groupText, preserve_intra_word_dashes = TRUE)
groupText <- gsub("http[[:alnum:]]*", "", groupText)
#groupText <- removeWords(groupText, badWordsList)
groupText <- stripWhitespace(groupText)
groupText <- str_trim(groupText, side = c("both"))
groupText <- gsub("\u0092", "'", groupText)
groupText <- gsub("\u0093|\u0094", "", groupText)
groupText <- removePunctuation(groupText, preserve_intra_word_dashes = FALSE)
## Remove back-to-back same words
groupText <- gsub("\\b(\\w+) \\1\\b", "\\1", groupText)
## Remove repeated letters when 3 or more are in a row
groupText <- gsub("(.)\\1{2,}" ,"\\1", groupText)



groupText <- gsub(pattern = "can't", replacement = " cannot", groupText)
groupText <- gsub(pattern = "'m", replacement = " am", groupText)
groupText <- gsub(pattern = "ain't", replacement = "am not", groupText)
groupText <- gsub(pattern = "'re", replacement = " are", groupText)
groupText <- gsub(pattern = "'ve", replacement = " have", groupText)
groupText <- gsub(pattern = "'d", replacement = " would", groupText)
groupText <- gsub(pattern = "'ll", replacement = " will", groupText)
groupText <- gsub(pattern = "n't", replacement = " not", groupText)
groupText <- gsub(pattern = "what's", replacement = "what is", groupText)
groupText <- gsub(pattern = "won't", replacement = "will not", groupText)


groupText <- gsub('http\\S+\\s*', '', groupText)
## Remove hastags
groupText <- gsub('#\\S+\\s*', '', groupText)
# Remove "rt"
groupText <- gsub('\\brt\\b', '', groupText)
# Remove "rt"
groupText <- gsub('\\bu\\b', ' you ', groupText)

groupText <- gsub("[^a-z _ -\\']", "", groupText)
groupText <- gsub("[[:digit:]]", "", groupText)



## Create corpus from cleaned groupText, remove large files, and perform 
## garbage collection
groupCorp <- VCorpus(VectorSource(groupText))
rm(groupText)
rm(badWordsList)
gc(verbose = FALSE)

## Unigram Function
## wordLengths = c(0,Inf) is added to capture words like i, a, is, it, am, my,
## etc. in the corpus because the default is c(4,Inf). I also added it to the 
## bi- and tri-gram tdm. I hope it doesn't mess it up.
tdm <- TermDocumentMatrix(groupCorp, control = list(wordLengths = c(0,Inf)))
tdmurs <- rowSums(as.matrix(tdm))
head (tdmurs)
tail(tdmurs)

## create data frame of unigrams
tdmu.df <- data.frame(tdmurs, stringsAsFactors = FALSE)
tdmu.df <- tdmu.df[order(tdmu.df[,"tdmurs"], decreasing = TRUE),,drop = FALSE]
colnames(tdmu.df) <- c("uniCount")
tdmu.df$uniTerm <- row.names(tdmu.df)

## Remove large files and perform garbage collection
rm(tdm)
rm(tdmurs)
gc()

## Bigram Function
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdmTok <- TermDocumentMatrix(groupCorp, 
                             control = list(tokenize = BigramTokenizer, 
                                            wordLengths = c(0,Inf)))
tdmbrs <- rowSums(as.matrix(tdmTok))
head(tdmbrs)

## Create data frame of bigrams, get rid of some bad text
tdmb.df <- data.frame(tdmbrs, stringsAsFactors = FALSE)
tdmb.df <- tdmb.df[order(tdmb.df[,"tdmbrs"], decreasing = TRUE),,drop = FALSE]
colnames(tdmb.df) <- c("biCount")
tdmb.df$bigram <- row.names(tdmb.df)
tdmb.df$firstWord <- sub(" .*$", "", tdmb.df$bigram)
tdmb.df$secWord <- sub("^.* ", "", tdmb.df$bigram)

## Remove large files and perform garbage collection
rm(tdmTok)
rm(tdmbrs)
gc()


## Tri-gram function
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tdmTok3 <- TermDocumentMatrix(groupCorp, 
                              control = list(tokenize = TrigramTokenizer, 
                                             wordLengths = c(0,Inf)))
tdmrs3 <- row_sums(tdmTok3)
head(tdmrs3)

## Create data frame of trigrams and clean up some bad text
tdm3.df <- data.frame(tdmrs3, stringsAsFactors = FALSE)
colnames(tdm3.df) <- c("triCount")
tdm3.df <- tdm3.df[order(tdm3.df[,"triCount"], decreasing = TRUE),,drop = FALSE]
tdm3.df$trigram <- row.names(tdm3.df)
tdm3.df$firstTwo <- gsub("\\s*\\w*$", "", tdm3.df$trigram)
tdm3.df$lastWord <- gsub("^.* ([[:alnum:]]+)$", "\\1", tdm3.df$trigram)

## Remove large files and perform garbage collection
rm(tdmTok3)
rm(tdmrs3)
gc()

## Remove corpus as we are done with it
rm(groupCorp)
gc()

## I'm going to save the dataframes in .csv format
write.csv(tdmu.df, file = "unigrams.csv")
write.csv(tdmb.df, file = "bigrams.csv")
write.csv(tdm3.df, file = "trigrams.csv")

## I need to add an OOV feature with counts equal to the number missing from
## the distribution of words in the corpus, or I can set the probability at 
## 0.000017 based on other research. Kneser-Ney also discounts every count by 
## 0.75 and adds some probability equal how common an ending is the word.
## Simple Good-Turing is a method to create a probability density that accounts
## for unseen ngrams by discounting the original probabilities.

## Can I actually implement Simple Good-Turing smoothing?
## How many words are seen only once?
## unigrams
##tdmu.df[tdmu.df$uniCount == 1,]
sum(tdmu.df[tdmu.df$uniCount == 1,]$uniCount)
## Can I get rid of all terms not in the English language?
## Yes I can! Sweet. It was rather fast too.
## Reference: http://stackoverflow.com/questions/5812478/how-i-can-select-rows-
## from-a-dataframe-that-do-not-match. Here is the reference to the dictionary - 
## Reference: http://www-01.sil.org/linguistics/wordlists/english/, to which I
## added contractions - 
## Reference: http://www.enchantedlearning.com/grammar/contractions/list.shtml
## and i and removed the apostrophe from words like i'm, state's, country's, 
## etc.. This was all done using JMP.
wordsEn <- read.csv("wordsEn.txt", header = TRUE, stringsAsFactors = FALSE)
uniset <- subset(tdmu.df, (uniTerm %in% wordsEn$wordsEn))
## Let's look at our new, clean data set. Nice
##uniset[uniset$uniCount == 1,]
sum(uniset[uniset$uniCount == 1,]$uniCount)
## I have term frequencies, now I need frequencies of frequencies. 
## Can I do this?
## Reference: http://stackoverflow.com/questions/25293045/count-number-of-rows-
## in-a-data-frame-in-r-based-on-group
unifreq <- aggregate(uniCount ~ as.character(uniCount), data = uniset, FUN = function(x){NROW(x)})
colnames(unifreq) <- c("freq", "freqoffreq")
unifreq$freq <- as.numeric(unifreq$freq)
## Now I have a dataset that matches the SGT example in the paper. Reference:
## http://www.grsampson.net/AGtf1.html. Now I calculate Z_r = Z_n_r/(r``-r`).
## Moving to Excel to use the worksheet using paper data to perform SGT
## smoothing. Name of file = "SGT Data Set.xlsx" and performs SGT for unigrams, 
## bigrams and trigrams. JMP is used to get Zipf's model for adjusting counts. 
## It is assumed the vocabulary consists of the number of observations in uniset.
write.csv(unifreq, file = "unifreq.csv")
## Read in the smoothed counts and probabilities
uniSGT <- read.csv("uniSGT.csv", header = TRUE, stringsAsFactors = FALSE)
## Using join as said to be faster than merge to associate probabilites by count
## to uniset. Reference: 
## http://stackoverflow.com/questions/1299871/how-to-join-merge-data-frames-
## inner-outer-left-right
uniset$id <- as.numeric(uniset$uniCount)
uniSGT$id <- as.numeric(uniSGT$r)
unigramSGT <- join(uniSGT, uniset, type = "left")
## Clean up the extra redundant columns
unigramSGT <- subset(unigramSGT, select = -c(id))

## Save file to disc.
write.csv(unigramSGT, file = "unigramSGT.csv")


## Now do the same thing for the bigram data set
## How many bigrams are seen only once?
tdmb.df[tdmb.df$biCount == 1,]
sum(tdmb.df[tdmb.df$biCount == 1,]$biCount)
## Can I get rid of all terms not in the English language?
## Yes I can! Sweet. It was rather fast too.
## Reference: http://stackoverflow.com/questions/5812478/how-i-can-select-rows-
## from-a-dataframe-that-do-not-match. I'll do this for firstWord first and then
## second word second. The remaining should be a clean data set of english
## words from the english alphabet.
wordsEn <- read.csv("wordsEn.txt", header = TRUE, stringsAsFactors = FALSE)
biset <- subset(tdmb.df, (firstWord %in% wordsEn$wordsEn))
biset <- subset(biset, (secWord %in% wordsEn$wordsEn))
## Let's look at our new, clean data set. Nice
biset[biset$biCount == 1,]
sum(biset[biset$biCount == 1,]$biCount)
## I have term frequencies, now I need frequencies of frequencies. 
## Can I do this?
## Reference: http://stackoverflow.com/questions/25293045/count-number-of-rows-
## in-a-data-frame-in-r-based-on-group
bifreq <- aggregate(biCount ~ as.character(biCount), data = biset, FUN = function(x){NROW(x)})
colnames(bifreq) <- c("freq", "freqoffreq")
bifreq$freq <- as.numeric(bifreq$freq)
## Now I have a dataset that matches the SGT example in the paper. Reference:
## http://www.grsampson.net/AGtf1.html. Now I calculate Z_r = Z_n_r/(r``-r`).
## Moving to Excel to use the worksheet using paper prosody data to smooth and then 
## reading the results back in
write.csv(bifreq, file = "bifreq.csv")
## Read in the smoothed counts and probabilities
biSGT <- read.csv("biSGT.csv", header = TRUE, stringsAsFactors = FALSE) 
## Using join as said to be faster than merge to associate probabilites by count
## to uniset. Reference: 
## http://stackoverflow.com/questions/1299871/how-to-join-merge-data-frames-
## inner-outer-left-right
biset$id <- as.numeric(biset$biCount)
biSGT$id <- as.numeric(biSGT$r)
bigramSGT <- join(biSGT, biset, type = "left")
## Clean up the extra redundant columns
bigramSGT <- subset(bigramSGT, select = -c(id))

## Save file to disc. 
write.csv(bigramSGT, file = "bigramSGT.csv")

## Trigrams are next and last
## How many trigrams are seen only once?
tdm3.df[tdm3.df$triCount == 1,]
sum(tdm3.df[tdm3.df$triCount == 1,]$triCount)
## Can I get rid of all terms not in the English language?
## Yes I can! Sweet. It was rather fast too.
## Reference: http://stackoverflow.com/questions/5812478/how-i-can-select-rows-
## from-a-dataframe-that-do-not-match. I'll do this for firstWord first and then
## second word second and the third word after adding a column holding the first
## and second words only. The remaining should be a clean data set of english
## words from the english alphabet.
wordsEn <- read.csv("wordsEn.txt", header = TRUE, stringsAsFactors = FALSE)
tdm3.df$firstWord <- sub(" .*$", "", tdm3.df$firstTwo)
tdm3.df$secWord <- gsub("^.* ([[:alnum:]]+)$", "\\1", tdm3.df$firstTwo)
triset <- subset(tdm3.df, (firstWord %in% wordsEn$wordsEn))
triset <- subset(triset, (secWord %in% wordsEn$wordsEn))
triset <- subset(triset, (lastWord %in% wordsEn$wordsEn))
## Let's look at our new, clean data set. Nice
triset[triset$triCount == 1,]
sum(triset[triset$triCount == 1,]$triCount)
## I have term frequencies, now I need frequencies of frequencies. 
## Can I do this? Absolutely!
## Reference: http://stackoverflow.com/questions/25293045/count-number-of-rows-
## in-a-data-frame-in-r-based-on-group
trifreq <- aggregate(triCount ~ as.character(triCount), data = triset, FUN = function(x){NROW(x)})
colnames(trifreq) <- c("freq", "freqoffreq")
trifreq$freq <- as.numeric(trifreq$freq)
## Now I have a dataset that matches the SGT example in the paper. Reference:
## http://www.grsampson.net/AGtf1.html. Now I calculate Z_r = Z_n_r/(r``-r`).
## Moving to Excel to use the worksheet I developed from the paper prosody data to 
## implement Simple Good-Turing smoothing and then reading the results back in
write.csv(trifreq, file = "trifreq.csv")
## Read in the smoothed counts and probabilities
triSGT <- read.csv("triSGT.csv", header = TRUE, stringsAsFactors = FALSE) 
## Using join as said to be faster than merge to associate probabilites by count
## to uniset. Reference: 
## http://stackoverflow.com/questions/1299871/how-to-join-merge-data-frames-
## inner-outer-left-right
triset$id <- as.numeric(triset$triCount)
triSGT$id <- as.numeric(triSGT$r)
trigramSGT <- join(triSGT, triset, type = "left")
## Clean up the extra redundant columns and save data to disc
trigramSGT <- subset(trigramSGT, select = -c(id))

## Save file to disc.
write.csv(trigramSGT, file = "trigramSGT.csv")

## unigramSGT, bigramSGT and trigramSGT will be joined in JMP to create the final
## model used to precict the next word given a sentence. The model uses the 
## Simple Good-Turing smoothed counts and probabilities. The data are joined 
## beginning with the trigramsGT data. The trigramSGT data are joined to the 
## bigramSGT data by the last two words of the trigramSGT data. So, if the 
## trigram = "play it again", the trigramSGT and bigramSGT data are joined on
## "it again". Then the resulting data set are joined to the unigramSGT data 
## on "again". They are joined using a full outer join keeping non-matches in 
## each table to get all the bigrams that match the last two words of the 
## trigram, but also all of the trigrams that do not have a bigram match. These
## are considered unseen trigrams and are weighted as the zero count SGT 
## probability. The same is used for the unseen bigrams.




#################HELPER FUNCTIONS

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
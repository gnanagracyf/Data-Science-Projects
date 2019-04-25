#read files into data frame
# group by the words and sum freq
# calc prob for those ngram
rm(list=ls())

library(dplyr)
library(stringr)
library(doParallel)


setwd("C:/Users/Gracy/Coursera - Data Science Specialization/Course 10 - Capstone/Week 1/Coursera-SwiftKey/final/en_US/data")

ncores <- detectCores() - 1
# Initiate cluster
cl <- makeCluster(ncores)
registerDoParallel(cl)


uni_file <- read.csv("unigrams.csv",header = F)
bi_file <- read.csv("bigrams.csv",header = F)
tri_file <- read.csv("trigrams.csv",header = F)
four_file <- read.csv("fourgrams.csv", header = F)
five_file <- read.csv("fivegrams.csv",header = F)
six_file <- read.csv("sixgrams.csv",header = F)


colnames(uni_file) <- c("unigram_word","uni_freq")
colnames(bi_file) <- c("bigram_word1","bigram_word2","bi_freq")
colnames(tri_file) <- c("trigram_word1","trigram_word2","trigram_word3","tri_freq")
colnames(four_file) <- c("fourgram_word1","fourgram_word2","fourgram_word3","fourgram_word4","four_freq")
colnames(five_file) <- c("fivegram_word1","fivegram_word2","fivegram_word3","fivegram_word4","fivegram_word5","five_freq")
colnames(six_file) <- c("sixgram_word1","sixgram_word2","sixgram_word3","sixgram_word4","sixgram_word5","sixgram_word6","six_freq")


#Remove rows with NA values ( complete cases)

uni_file <- na.omit(uni_file)
bi_file <- na.omit(bi_file)
tri_file <- na.omit(tri_file)
four_file <- na.omit(four_file)
five_file <- na.omit(five_file)
six_file <- na.omit(six_file)

#Remove rows if it has single characters 

uni_file <- uni_file[!(str_length(uni_file$unigram_word)) == 1, ]
bi_file <- bi_file[!((str_length(bi_file$bigram_word1) == 1) | (str_length(bi_file$bigram_word2) == 1)), ]
tri_file <- tri_file[!((str_length(tri_file$trigram_word1) == 1) | (str_length(tri_file$trigram_word2) == 1) | (str_length(tri_file$trigram_word3) == 1)), ]
four_file <- four_file[!((str_length(four_file$fourgram_word1) == 1) | (str_length(four_file$fourgram_word2) == 1) | (str_length(four_file$fourgram_word3) == 1) | (str_length(four_file$fourgram_word4) == 1)), ]
five_file <- five_file[!((str_length(five_file$fivegram_word1) == 1) |  (str_length(five_file$fivegram_word2) == 1) | (str_length(five_file$fivegram_word3) == 1) | (str_length(five_file$fivegram_word4) == 1) | (str_length(five_file$fivegram_word5) == 1)), ]
six_file <- six_file[!((str_length(six_file$sixgram_word1) == 1) | (str_length(six_file$sixgram_word2) == 1) | (str_length(six_file$sixgram_word3) == 1) | (str_length(six_file$sixgram_word4) == 1) | (str_length(six_file$sixgram_word5) == 1) | (str_length(six_file$sixgram_word6) == 1)), ]



#Group same words as there are from different chunkcs
uni_file <- uni_file %>% group_by(unigram_word) %>% summarise(sum(uni_freq))
bi_file <- bi_file %>% group_by(bigram_word1,bigram_word2) %>% summarise(sum(bi_freq))
tri_file <- tri_file %>% group_by(trigram_word1,trigram_word2,trigram_word3) %>% summarise(sum(tri_freq))
four_file <- four_file %>% group_by(fourgram_word1,fourgram_word2,fourgram_word3,fourgram_word4) %>% summarise(sum(four_freq))
five_file <- five_file %>% group_by(fivegram_word1,fivegram_word2,fivegram_word3,fivegram_word4,fivegram_word5) %>% summarise(sum(five_freq))
six_file <- six_file %>% group_by(sixgram_word1,sixgram_word2,sixgram_word3,sixgram_word4,sixgram_word5,sixgram_word6) %>% summarise(sum(six_freq))

#Reset col names again
colnames(uni_file) <- c("unigram_word","uni_freq")
colnames(bi_file) <- c("bigram_word1","bigram_word2","bi_freq")
colnames(tri_file) <- c("trigram_word1","trigram_word2","trigram_word3","tri_freq")
colnames(four_file) <- c("fourgram_word1","fourgram_word2","fourgram_word3","fourgram_word4","four_freq")
colnames(five_file) <- c("fivegram_word1","fivegram_word2","fivegram_word3","fivegram_word4","fivegram_word5","five_freq")
colnames(six_file) <- c("sixgram_word1","sixgram_word2","sixgram_word3","sixgram_word4","sixgram_word5","sixgram_word6","six_freq")



#SMOOTHING

#SMOOTHING
#What to do if no 4-gram begins with the input trigram.
#In this case one approach is to trim the input down to 2 words by deleting the first word. 
#Then we can check if any 3-gram in the corpus starts with the 2 words. The last word in the 3-gram 
#is a candidate for a guess. However, we can score it so we can compare it to other candidates. 
#This is process is called the back-off. It ends with the worse scenario - the input has never been seen in the corpus. 
#In this case you can return the most common 1-gram.


total_unigram <- 102059900 #Single words In the whole corpus

uni_score <- uni_file %>%  rowwise %>% mutate( score = uni_freq/total_unigram ) %>% arrange(desc(score)) %>% select(unigram_word,uni_freq,score)

merge_file1 <- merge(uni_file,bi_file,by.x="unigram_word",by.y="bigram_word1") # If needed we can keep it or remove asap


unigrams_data <- merge(uni_file,uni_score,all=TRUE)
unigrams_data <- unigrams_data %>% mutate(score = replace(score,is.na(score),0)) %>% as.data.frame()
saveRDS(unigrams_data, file = "uni_score.rds")
rm(unigrams_data)
rm(uni_file)
rm(uni_score)
gc(verbose = FALSE)



bi_score <- merge_file1 %>% rowwise %>% mutate( score = 0.4 * ( bi_freq/uni_freq)) %>% select(unigram_word,bigram_word2,bi_freq,score)
colnames(bi_score) <- c("bigram_word1","bigram_word2","bi_freq","score")

merge_file2 <- merge(bi_file,tri_file,by.x=c("bigram_word1","bigram_word2"), by.y = c("trigram_word1","trigram_word2"))

bigrams_data <- merge(bi_file,bi_score,all=TRUE)
bigrams_data <- bigrams_data %>% mutate(score = replace(score,is.na(score),0)) %>% as.data.frame()

saveRDS(bigrams_data, file = "bi_score.rds")
rm(bigrams_data)
rm(merge_file1)
rm (bi_file)
rm(bi_score)
gc(verbose = FALSE)

tri_score <- merge_file2 %>% rowwise %>% mutate( score = 0.16 * (tri_freq/bi_freq)) %>% select(bigram_word1,bigram_word2,trigram_word3,tri_freq,score)
colnames(tri_score) <- c("trigram_word1","trigram_word2","trigram_word3","tri_freq","score")
merge_file3 <- merge(tri_file,four_file,by.x=c("trigram_word1","trigram_word2","trigram_word3"), by.y = c("fourgram_word1","fourgram_word2","fourgram_word3"))

#tri_file <- data.frame(tri_file)
#tri_score <- data.frame(tri_score)
trigrams_data <- merge(tri_file,tri_score,all=TRUE)
trigrams_data<- trigrams_data %>% mutate(score = replace(score,is.na(score),0)) %>% as.data.frame()

saveRDS(trigrams_data, file = "tri_score.rds")
rm(trigrams_data)
rm(merge_file2)
rm (tri_file)
rm(tri_score)
gc(verbose = FALSE)


four_score <- merge_file3 %>% rowwise %>% mutate( score = 0.064 * (four_freq/tri_freq)) %>% select(trigram_word1,trigram_word2,trigram_word3,fourgram_word4,four_freq,score)
colnames(four_score) <- c("fourgram_word1","fourgram_word2","fourgram_word3","fourgram_word4","four_freq","score")

merge_file4 <- merge(four_file,five_file,by.x=c("fourgram_word1","fourgram_word2","fourgram_word3","fourgram_word4"), by.y = c("fivegram_word1","fivegram_word2","fivegram_word3","fivegram_word4"))

fourgrams_data <- merge(four_file,four_score,all=TRUE)
fourgrams_data<- fourgrams_data %>% mutate(score = replace(score,is.na(score),0)) %>% as.data.frame()

saveRDS(fourgrams_data, file = "four_score.rds")

rm(fourgrams_data)
rm(merge_file3)
rm(four_score)
rm (four_file)
gc(verbose = FALSE)

five_score <- merge_file4 %>% rowwise %>% mutate( score = 0.256 * (five_freq/four_freq)) %>% select(fourgram_word1,fourgram_word2,fourgram_word3,fourgram_word4,fivegram_word5,five_freq,score)
colnames(five_score) <- c("fivegram_word1","fivegram_word2","fivegram_word3","fivegram_word4","fivegram_word5" ,"five_freq","score")

merge_file5 <- merge(five_file,six_file,by.x=c("fivegram_word1","fivegram_word2","fivegram_word3","fivegram_word4","fivegram_word5"), by.y = c("sixgram_word1","sixgram_word2","sixgram_word3","sixgram_word4","sixgram_word5"))

fivegrams_data <- merge(five_file,five_score,all=TRUE)
fivegrams_data<- fivegrams_data %>% mutate(score = replace(score,is.na(score),0)) %>% as.data.frame()

saveRDS(fivegrams_data, file = "five_score.rds")
rm(fivegrams_data)
rm(merge_file4)
rm (five_file)
rm(five_score)
gc(verbose = FALSE)


six_score <- merge_file5 %>% rowwise %>% mutate( score = 0.1024 * (six_freq/five_freq)) %>% select(fivegram_word1,fivegram_word2,fivegram_word3,fivegram_word4,fivegram_word5,sixgram_word6,six_freq,score)
colnames(six_score) <- c("sixgram_word1","sixgram_word2","sixgram_word3","sixgram_word4","sixgram_word5","sixgram_word6" ,"six_freq","score")

sixgrams_data <- merge(six_file,six_score,all=TRUE)
sixgrams_data<- sixgrams_data %>% mutate(score = replace(score,is.na(score),0)) %>% as.data.frame()

saveRDS(sixgrams_data, file = "six_score.rds")

rm(sixgrams_data)
rm(merge_file5)
rm (six_file)
rm(six_score)
gc(verbose = FALSE)


stopCluster(cl)

#In our example, "this is a car", the base of the 4-gram_X is the 3-gram "this is a".
#So your score would be the frequency of "this is a car" divided by the frequencyi of "this is a".

#w1 < "hi how are you"
#s1 <- 10
#w2 <- "hi how are"
#s2 <- 15+
#w3 <- "hi how"
#w4 <- "hi"

#score(w1) = s1/s2






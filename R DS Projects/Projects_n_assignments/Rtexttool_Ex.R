library(tibble)
library(tidyr)
pos_tweets =  rbind(
        c('I love this car', 'positive'),
        c('This view is amazing', 'positive'),
        c('I feel great this morning', 'positive'),
        c('I am so excited about the concert', 'positive'),
        c('He is my best friend', 'positive')
)

neg_tweets = rbind(
        c('I do not like this car', 'negative'),
        c('This view is horrible', 'negative'),
        c('I feel tired this morning', 'negative'),
        c('I am not looking forward to the concert', 'negative'),
        c('He is my enemy', 'negative')
)

test_tweets = rbind(
        c('feel happy this morning', 'positive'),
        c('larry friend', 'positive'),
        c('not like that man', 'negative'),
        c('house not great', 'negative'),
        c('your song annoying', 'negative')
)

tweets = rbind(pos_tweets, neg_tweets, test_tweets)

library(RTextTools)
tweets_matrix = create_matrix(tweets[,1], language = 'English', 
                              removeStopwords = FALSE, 
                              removeNumbers = TRUE, 
                              stemWords = FALSE)


#Here we also specify the training set and the testing set
container = create_container(tweets_matrix, as.numeric(as.factor(tweets[,2])), 
                             trainSize = 1:10, testSize = 11:15, 
                             virgin = FALSE)

tweet_model <- train_model(container, algorithm = "SVM")

tweet_model_result <- classify_model(container, tweet_model)
table(as.numeric(as.factor(tweets[11:15, 2])), tweet_model_result[,"SVM_LABEL"])
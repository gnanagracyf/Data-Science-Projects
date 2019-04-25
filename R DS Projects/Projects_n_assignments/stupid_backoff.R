#stupid backoff algo

word <- c("Hi","hello","how","are","you")
freq <- c(10,20,3,60,1)

#compute score of a given word and decide which one to consider 

#Algorithm Explained
#In the Stupid Backoff model, the backoff factor Alpha is heuristically set to a fixed value (0.4) 
#instead of being computed to reduce complexity.

#In our implementation, the N-gram (Unigram, bigram and trigram) data was generated normally. 
#The backoff factor Alpha was applied at runtime in server.R script in the Shiny app.

#A brief description of the algorithm is: 
#When tring to fin the probability of word appearing in a sentence it will first look for context
#for the word at the n-gram level and if there is no n-gram of that size it will recurse to the (n-1)-
#gram and multiply its score with 0.4. The recursion stops at unigrams.

#So if I want to find the probability of "day" in the context of "a sunny day" 
#it would first look to see if the tri-gram "a sunny day" exists in the corpus, 
#if not it would try the same with the bigram "sunny day" and finally it would just get the
#frequency for "day" divided by the corpus size (total number of words in the training data).


#Count n-grams offline

#Compute pseudo-probabilities at runtime

# Use trigram if you have good evidence, otherwise bigram otherwise unigram

# if user entered "hi hello how are you". We need to predict next word. So search for this 5 gram word in dict ...if match is there
# and more than one match then compute score...which one is high use it

# if word not found..go to 4 gram and search for "hello how are you"...repeat the score calculation * 0.4 * score( hello how are you)
# if not ..go to tri gram search for "how are you"
# if not go to bi gram "are you"
# else take score of unigram "you"

sample score table 

hi hello how are you  5
hello how are you 4
how are you 3
are you 2
you 1



query <- "hi hello how are you"
#calculate score in five gram
   s(dear/ hello how are you) = count (hello how are you dear ) / count(how are you dear)  if count(hello how are you dear) >0 
   else
           s( dear/how are you) = count(how are you dear) / count ( are you dear)  if count(how are you dear) > 0
           s ( dear / are you ) =  count ( are you dear ) / count ( you dear) if count ( are you dear ) >0 
           s ( dear / you)  = count (you dear) / count ( dear) if count(you dear ) > 0
           s (dear) = count( dear) / N (the number of observed unigrams)
           
           
           
words <- c("hi hello how are you","hello how are you","how are you","are you","you")
counts <- c(10,2,1,4,6)

df <- data.frame(words,counts) 

input_word <- "how are you"

library(stringr)
library(qdap)



stupid_backoff <- function(input_word){
        result_score <- 0
        total_unigram <- 10
        if (word_count(input_word) == 1 ){
                result_score <- df$counts[df$words == input_word]/total_unigram
                return(result_score)
        }
        else if ( input_word %in% df$words ){
                result_score <- df$counts[df$words == input_word] / df$counts[df$words == word(input_word,2,word_count(input_word))]
                return(result_score)
        }
        else{
                input_word <- word(input_word,2,word_count(input_word))
                return(0.4 * stupid_backoff(input_word))
                
        }
        result_score
}
      
   


## Ensure the user input includes valid words and if not provide information on how to appropriately use the application
## Read in the dictionary
## Reference: http://www-01.sil.org/linguistics/wordlists/english/ from http://www.sil.org/ downloaded 6/21/2016
wordsEn <- read.csv(file = "wordsEn.txt", header = FALSE, sep = ",", stringsAsFactors = FALSE)

input1 <- tokenize("The quick brown fox")
input2 <- tokenize("Th5 7uIc# *ro@n 9(x")

output1 <- selectFeatures(input1, wordsEn, selection = "keep", valuetype = "fixed", padding = FALSE, case_insensitive = TRUE)
output2 <- selectFeatures(input2, wordsEn, selection = "keep", valuetype = "fixed", padding = TRUE, case_insensitive = TRUE)

output1

output2
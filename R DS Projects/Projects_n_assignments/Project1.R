##https://rpubs.com/DMW29/194887

## The order in which the text are cleaned does matter
badWordsList <- readLines("Bad Word List.csv", n=-1, skipNul = TRUE)

input1 <- "The quick brown fox"
input2 <- "Th5 7uIc# *ro@n 9(x"

input1 <- iconv(input1, "latin1", "ASCII", sub="")
input1 <- tolower(input1)
input1 <- removeNumbers(input1)
input1 <- removePunctuation(input1, preserve_intra_word_dashes = TRUE)
input1 <- gsub("http[[:alnum:]]*", "", input1)
input1 <- removeWords(input1, badWordsList)
input1 <- stripWhitespace(input1)
input1 <- str_trim(input1, side = c("both"))
input1 <- gsub("\u0092", "'", input1)
input1 <- gsub("\u0093|\u0094", "", input1)
input1 <- removePunctuation(input1, preserve_intra_word_dashes = FALSE)
## Remove back-to-back same words
input1 <- gsub("\\b(\\w+) \\1\\b", "\\1", input1)
## Remove repeated letters when 3 or more are in a row
input1 <- gsub("(.)\\1{1,}" ,"\\1", input1)

input1
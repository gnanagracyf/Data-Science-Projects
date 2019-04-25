input2 <- iconv(input2, "latin1", "ASCII", sub="")
input2 <- tolower(input2)
input2 <- removeNumbers(input2)
input2 <- removePunctuation(input2, preserve_intra_word_dashes = TRUE)
input2 <- gsub("http[[:alnum:]]*", "", input2)
input2 <- removeWords(input2, badWordsList)
input2 <- stripWhitespace(input2)
input2 <- str_trim(input2, side = c("both"))
input2 <- gsub("\u0092", "'", input2)
input2 <- gsub("\u0093|\u0094", "", input2)
input2 <- removePunctuation(input2, preserve_intra_word_dashes = FALSE)
## Remove back-to-back same words
input2 <- gsub("\\b(\\w+) \\1\\b", "\\1", input2)
## Remove repeated letters when 3 or more are in a row
input2 <- gsub("(.)\\1{1,}" ,"\\1", input2)

input2
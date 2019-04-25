library(quanteda)

eg.txt <- c('increase in_the great plenary', 
            'great plenary emission_reduction', 
            'increase in_the emission_reduction emission_increase')
eg.corp <- corpus(eg.txt)
eg.dfm <- dfm(eg.corp)

#head(eg.dfm)

#head(dfm, n = 3, nfeature = 4)

en.analyze.dfm.unigrams <- dfm(eg.corp, ignoredFeatures = stopwords("english"), stem = TRUE, ngrams = 1, verbose = FALSE)
en.analyze.dfm.uni.freq <- colSums(en.analyze.dfm.unigrams)
uni.freq <- sort(en.analyze.dfm.uni.freq, decreasing=TRUE) 
uni.freq.prune <- as.numeric()
for (i in 1:length(uni.freq)) { 
        if (uni.freq[i] > 10) {
                uni.freq.prune <- c(uni.freq.prune, uni.freq[i]) }
}
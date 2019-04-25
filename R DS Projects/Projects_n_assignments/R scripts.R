add2num <- function( a, b) {
        a+b
}

above10 <- function(vecs) {
        use_Vec <- vecs > 10
        vecs[use_vec]
}

abouve10_2 <- function( x , n){
        use_vec <- x > n
        x[use_vec]
}

col_mean <- function(x , removeNA = TRUE){
        n_cols <- ncol(x)
        means <- numeric(n_cols)
        for(i in 1:n_cols){
                means[i] = mean(x[i], na.rm = removeNA)
        }
        means
}
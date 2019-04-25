## This R function demonstrates that is able to cache potentially time-consuming computations. 
## For example, taking the mean of a numeric vector is typically a fast operation. However, 
## for a very long vector, it may take too long to compute the mean, especially if it has to be 
## computed repeatedly (e.g. in a loop). If the contents of a vector are not changing, it may make 
## sense to cache the value of the mean so that when we need it again, it can be looked up in the cache 
## rather than recomputed. In this Programming Assignment you will take advantage of the scoping rules of
## the R language and how they can be manipulated to preserve state inside of an R object.
## we introduce the <<- operator which can be used to assign a value to an object in an environment 
## that is different from the current environment. 

## Below are two functions that are used to create a special object that stores a mtrix and caches its inverse using solve function.

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

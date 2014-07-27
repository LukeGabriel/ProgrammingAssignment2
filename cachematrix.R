## Put comments here that give an overall description of what your
## functions do
## These are two functions by which you can first tag 
## matrices with sub-functions, followed by an inversion.
## During inversion, the second function can assess if this
## particular inversion has been completed, and will return
## the previously cached inversion as the value for the 
## new inversion

## Write a short comment describing this function
## This function sets sub-function tags on a matrix and 
## allows for future calls on this re-formatted matrix
## to identify if it is the same as other matrices

makeCacheMatrix <- function(x = matrix()) {

          m <- NULL
          set <- function(y) {
                  x <<- y
                  m <<- NULL
          }
          get <-function() x
          setinv <- function(solve) m <<- solve
          getinv <- function() m
          list(set = set, get = get,
               getinv = getinv,
               setinv = setinv)
}


## Write a short comment describing this function
## This function will create a new matrix which is the
## inverse of the inputted matrix, while also checking
## if this inversion has been done on other matrices with
## identical contents

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)            
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        ## Return a matrix that is the inverse of 'x'
        m
        
}

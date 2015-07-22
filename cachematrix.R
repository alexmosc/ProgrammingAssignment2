## Write the following functions:
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
#Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the iversed matrix
#get the value of the iversed matrix

makeCacheMatrix <- function(x = matrix()) {
       
       m <- NULL
       set <- function(y) {
              x <<- y
              m <<- NULL
       }
       get <- function() x
       setinv <- function(solve) m <<- solve
       getinv <- function() m
       list(set = set, get = get,
            setinv = setinv,
            getinv = getinv)

}


## The following function calculates the inverse of the special "matrix" created with the above function. 
#However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the 'setinv' function.

cacheSolve <- function(x=matrix(), ...) {
       
       m <- x$getinv()
       if(!is.null(m)) {
              message("getting cached data")
              return(m)
       }
       matrix <- x$get()
       m <- solve(matrix , ...)
       x$setinv(m)
       m
       
}


## this is a working example; RUN the functions above before proceeding
x <- matrix(sample(1:10, 81, replace = T), 9, 9)
a <- makeCacheMatrix(x)
cacheSolve(a)


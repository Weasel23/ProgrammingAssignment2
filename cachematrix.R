## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function will create a special 'matrix' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL ##Cached inverse, started with NULL
     set <- function(y, ...) {
          x <<- matrix(y, ...) ##Allows matrix to be set with passed arg
          inv <<- NULL ##if matrix reset, then cached inverse set to NULL 
     }
     get <- function() x ## returns matrix
     setinverse <- function(solve) inv <<- solve  ##function that will cache inverse
     getinverse <- function() inv ##function that will return cached inverse (or null)
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: This function will compute the inverse of a matrix, using the makeCacheMatrix function.
##   If inverse of matrix has already been computed it will retrieve the inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inverse <- x$getinverse()
     if(!is.null(inverse)) { ##Checks to see if inverse has been cached
          message("getting cached data")
          return(inverse) ##Returns inverse and exits from function
     } 
     ## Inverse not cached, need to generate it
     data <- x$get() 
     inverse <- solve(data, ...) ##Calculates the inverse
     x$setinverse(inverse) ## Caches the inverse
     inverse ## Returns the inverse
}

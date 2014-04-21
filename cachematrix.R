## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function returns a list of functions and a holder for
## a cached item
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  # set stores the matrix, and sets inverted cache value to null
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # get original data
  get <- function() x
  
  # set the cached inverse value
  setinverse <- function(inverse) i <<- inverse
  
  # return the inverse value
  getinverse <- function() i
  
  # return array of functions for the cache element
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve examines the incoming value for an already
## stored inverse, and returns that if found. Otherwise
## it computes the matrix inverse, sets it in the cache
## and return the inverse
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    # cached matrix found, so return it
    message("getting cached data")
    return(i)
  }
  # no cached data
  data <- x$get() # get original value
  i <- solve(data, ...) # compute inverse
  x$setinverse(i) # store in cache
  i # return the cached value
}

# this can be tested with the following
# m <- matrix(c(1, -1/4, -1/4, 1), nrow=2, ncol=2, byrow=TRUE)
# cm <- makeCacheMatrix(m)
# i <- cacheSolve(cm)  # should be silent
# i <- cacheSolve(cm)  # prints message of getting from cache
# a %*% i   # this should return an identity matrix


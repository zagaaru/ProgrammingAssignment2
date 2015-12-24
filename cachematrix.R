## Put comments here that give an overall description of what your
## functions do

####makeCacheMatrix Function
## Create a cacheMatrix object for an invertale matrix.
##makeCacheMatrix creates a special matrix" 
##object that can cache the input matrix and its inverse

##cacheSolve FUNCTION
##calls functions stored in the special "matrix" returned by makeCacheMatrix (above). 
##If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve retrieves the inverse from the cache. 
##If the input is new, it calculates the inverse of the data and sets the inverse in the cache 
##via the setReversedOrder function.

##TESTING
## Test your output
##mat <- matrix(data = c(4,2,7,6), nrow = 2, ncol = 2)
##mat2 <- makeCacheMatrix(mat)
##cacheSolve(mat2)

makeCacheMatrix <- function(x = matrix()) {

    cachedReversedOrder <- NULL
    set <- function(y) {
      x <<- y
      cachedReversedOrder <<- NULL
    }
    get <- function() x
    setReversedOrder <- function(ReversedOrder) cachedReversedOrder <<- ReversedOrder
    getReversedOrder <- function() cachedReversedOrder
    list(set = set, get = get,
         setReversedOrder = setReversedOrder,
         getReversedOrder = getReversedOrder)
  }
  


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the ReversedOrder of 'x'
  
    invFunc <- x$getReversedOrder()
    if(!is.null(invFunc)) {
      message("getting cached data")
      return(invFunc)
    }
    data <- x$get()
    invFunc <- solve(data, ...)
    x$setReversedOrder(invFunc)
    invFunc
  }
  



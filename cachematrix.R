## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setCache <- function(inverse) inv <<- inverse
  getCache <- function() inv
  list(set = set, get = get, setCache = setCache, getCache = getCache)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv <- x$getCache()  
  
if (!is.null(inv)){
  message("From the cache...")
  return(as.matrix(inv))
}
else
{
  inv <- solve(x$get())
  x$setCache(inv)
  }

inv
}

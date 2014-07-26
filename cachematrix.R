## ======================================================================
## R Programming Assignment 2
## Lexical Scoping
## ======================================================================
## Tom Woods - 7/2014
## ======================================================================
## Overview:
## The following two functions, makeCacheMatrix and cacheSolve, follow
## the structure given for vectors in our class example, with minor tweaks
## as noted for matrices.  The functions create a cache of the inverse of
## a matrix with the variable holding the cache stored in a different
## environemnt from the one in which the solver is invoked.
## =======================================================================
## Usage:
## Assign a reversible matrix to a variable.  Two examples of reversible 
## matrices are matrix( c(1,0,0,1),2,2 ) and matrix(1:4,2,2 ).  Make 
## a "Special" matrix using makeCacheMatrix(), then use cacheSolve() to 
## retrieve the inverse of the matrix.  Running cacheSolve once computes
## and caches the matrix, running it subsequent times retrieves the cached
## inverse.  An example follows:
##
##    command                             result
##
##    m <- matrix( c(1,0,0,1),2,2 ) 
##    mm <- makeCacheMatrix(m)
##    cacheSolve(mm)
##                                      [,1] [,2]
##                                [1,]    1    0
##                                [2,]    0    1
##
##    cacheSolve(mm)
##                                From the cache...
##                                      [,1] [,2]
##                                [1,]    1    0
##                                [2,]    0    1
## =======================================================================




## makeCacheMatrix
##------------------
## Start with an explicit coercion to a matrix and initialize the value that
## will hold the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
## run set t0 transfer the passed-in matrix to the global environment, and
## initialize an inverse variable there as well.
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
## A function to retrieve the original matrix
  get <- function() x

## Gettor and Settor functions for putting the calculated inverse into and
## retrieving it from the cache.  Note that the cache is set in the global
## environment.
  setCache <- function(inverse) inv <<- inverse
  getCache <- function() inv

## as the last operation, return a list that has each function (this seems 
## a lot like function pointers in C++)
  list(set = set, get = get, setCache = setCache, getCache = getCache)
}



## cacheSolve
##------------------
## Take a "special" matrix constructed with makeCacheMatrix and return 
## the inverse.  If the cache is empty, calculate the inverse, cache it, 
## and return it.  If the cache has data, just return that instead of 
## calculating it again.
cacheSolve <- function(x, ...) {

## Get whatever is in the cache already
inv <- x$getCache()  

## If the cache is null, do the math and set the cache with the result.
## (in the else clause.)  If it has data, return the data with a message
## specifying it is from the cache.
if (!is.null(inv))  {
    message("From the cache...")
    return(as.matrix(inv))
}
else  {
    inv <- solve(x$get())
    x$setCache(inv)
    inv
}

}

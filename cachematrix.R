## This file contains two functions. "makeCacheMatrix" caches a
## matrix and its inverse. By this function, the user may retrieve
## the matrix and its inverse from the cache or can reassign them. 
## "cacheSolve" takes a matrix and caches it and its inverse if the
## matrix has not been updated.

## This function creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL        ##initialize the variable which update the
                       ##cached inverse
    set <- function (y){ #the sub-function which update the matrix in cache
    x <<- y           ##update the matrix in cache
    inv <<- NULL      ##when the matrix is updated the inverse should
                      ##be updated (later)
  }
  get <- function() x ##this sub-function will return the matrix
  setinv <- function (inverse) inv <<- inverse ##this sub-function
                                               ##updates the cached
                                               ##inverse
  getinv <- function() inv ##this sub-function will return the inverse
  list (set = set , get = get , ##four sub-function will be returned
        setinv = setinv , getinv=getinv)
}


## This function computes the inverse of the special
## matrix returned by "makeCacheMatrix" above

cacheSolve <- function(x, ...) {
  inv <- x$getinv()      ##retrive the cached inverse (updated or not)
  if (!is.null(inv)){    ##check if the cached has been already calculated
    message("getting cached data")
    return(inv)          ##returns the inverse and end the function
  }
  newmatrix <- x$get()   ##needs to calculate the inverse therefore
                         ##it needs to know the new matrix!
  inv <- solve(newmatrix,...) #calculate the inverse of the new matrix
  x$setinv(inv)          ##update the cached inverse
  inv
        ## Return a matrix that is the inverse of 'x'
}

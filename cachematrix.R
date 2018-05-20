A## This function creates a special matrix object that can cache its 
## inverse cacheSolve. This function computes for the inverse of the 
## special matrix as it was given by makeCacheMatrix. The inverse is 
## calculated from the cacheSolve function and get the inverse from 
## the cache. The computed inverse is a squared matrix using the solve 
## function of R. Refer to the functions provided below that solved 
## inverse of a matrix


makeCacheMatrix <- function(x = matrix()) {
	  i	  <- NULL
  set		<- function(y) {
			      x <<- y
			      i <<- NULL
}
get <- function() x
setinverse <- function(inverse)	i <<- inverse
getinverse <- function() i
list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if (!is.null(i)) {
			message("getting the cached data")
			return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}


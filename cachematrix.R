## These functions solve the inverse of matricies and cache the result
## If function is called multiple times for the same matrix it retrives
## the cached result rather than computing it again.

## This function makes a list of functions that are used by the
## cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
  	i <- NULL
  	set <- function(y) {
    	x <<- y
    	i <<- NULL
  	}
  	get <- function() x
  	setinverse <- function(inverse) i <<- inverse
  	getinverse <- function() i
  	list( set = set, get = get, 
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function uses the funcitons from the makeCasheMatrix function
## to solve and cache the inverse of a matrix.

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
  	if(!is.null(i)) {
    	message("getting cached data")
    	return(i)
  	}
  	data <- x$get()
  	i <- solve(data, ...)
  	x$setinverse(i)
  	i
}

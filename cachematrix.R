# makeCacheMatrix below is a function that accepts a matrix and returns 
# a matrix list that consists of  functions such as set, get inverse
# This function cache values for inverse of the matrix.

makeCacheMatrix <- function(x=matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}

	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

#This function cacheSolve tries to retrieve inverse of a matrix if
# it has been already calculated and returns it if found, otherwise
# calculates the inverse of the matrix x via the solve(data,...) function,
#which sets and prints the inv of the matrix.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse() 
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}


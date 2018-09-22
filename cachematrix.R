## Assignment: Caching the Inverse of a Matrix
## assignment is to write a pair of functions that cache the inverse of a matrix that 
## demonstrates the concept of caching to aid performance and avoid repeated calculations to save compute time.
## assumption: supplied matrix is always invertible


## Function to create and return a matrix object and cache its inverse

makeCacheMatrix <- function(x = matrix()) {

	invmat <- NULL # set the initial value
	setMatrix <-function (y){
		mat <<- y #assign y to matrix
		invmat <<- NULL #set inverse to NULL
	}
	getMatrix <-function() x
	setInverse <- function(inverse) invmat <<- inverse
	getInverse <- function () invmat
	list(setMatrix = setMatrix, getMatrix=getMatrix, setInverse=setInverse, getInverse=getInverse)
}


## function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix`. If the inverse has
##  already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invmat <- x$getInverse()
	if (!is.null(invmat)){
		message("fetching cached inverse of the given matrix...")
		return(invmat)
	}
	
	data <- x$getMatrix()
	i <- solve(data, ...)
	x$setInverse(i)
	i
}


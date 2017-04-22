##
## cachematrix.R
## 
## Programming Assignment 2 (week 3) of data science course on coursera.org
## 
## Task: Create a cached inverse matrix object
##
## Date: 22/04/2017 (dd/mm/yyyy)
## Usage:
## M <- matrix(rnorm(9), nrow=3, ncol=3)
## to create a random matrix
##
## cacheMatrix <- makeCacheMatrix(M)
## first run to create the cached object "no cached object has been foud"
## cacheSolve(cacheMatrix)
## second run shows that cached object has been found and solve() function is not run
## 
## get/set commands:
## cacheMatrix$set(M) # Specify that matrix M should be cached
## cacheMatrix$get() # Return original matrix M
## cacheMatrix$getInverse() # Return inverse matrix
 

## function to cache matrix
# Input: Matrix
makeCacheMatrix <- function(x = matrix()) {
# initialize variable
cachedInverseMatrix <- NULL
set <- function(ma) {
		x <<- ma
		cachedInverseMatrix <<- NULL
}
get <- function() {
	x
}
# get / set
setInverse <- function(inverse) cachedInverseMatrix <<- inverse
getInverse <- function() cachedInverseMatrix

list(set = set, 
	get = get,
	setInverse = setInverse,
	getInverse = getInverse)
}


## Return the inverse of an cacheMatrix object


cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
inv <- x$getInverse()
## if matrix has already been cached, return this
if(!is.null(inv)) {
	message("getting cached data")
	return(inv)
}
else {
## else compute it
	message("no cached object found")
	data <- x$get() 
	inv <- solve(data, ...)
	x$setInverse(inv)
	inv
}
}

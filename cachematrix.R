## The function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- Null
	}
	get <- function()x
	setInverse <- function(inverse) inv <<- inverse
	list(set = set,
		get = get,
		setInverse = setInverse,
		getInverse = get Inverse)
}

## This function computes the inverse of the special matrix of makeCacheMatrix. If the inverse has been calculated, then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix is the inverse of 'x'
	inv <- x$getInverse()
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
}
mat <- x$get()
inv <- solve(mat, ...)
x$setInverse(inv)
inv
}
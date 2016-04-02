## Caching the inverse of a Matrix. Matrix inversion is usually a ##costly computation and there may be some benefits to caching the ##inverse of a matrix rather than compute it repeatedly.

## This function creates a special "matrix" object that can cache ##its inverse

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		set <- function(y) {
			x <<- y
			inv <<- NULL
		}
		get <- function() x
		setinverse <- function(inv) inv <<- inverse
		getinverse <- function() inv
		list(set = set, get = get, 
		setinverse = setinverse,
		getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" ##created by makeCachematrix above. If the inverse has already ##been calculated (and the matrix has not changed), then it should ##retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(m)){
			message("getting cached data")
			return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv 
        
}

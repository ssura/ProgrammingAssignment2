## This set of R functions performs inverse operation on matrix in an efficient manner.
## Since inverse operation is expensive, especially when the matrix is large, it is
## better to cache the results for subsequent usage of inverse operation on the same 
## matrix

## This function defines a list of action that facilitate caching of a matrix and it's inverse
## It has set, get, setsolve and getsolve actions to perform storing and retrival of 
## original matrix as well as the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	
	inverse_matrix <- NULL
	set <- function(y) {
		x <<- y
		inverse_matrix <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) inverse_matrix <<- solve
	getsolve <- function() inverse_matrix

	list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## This function is a wrapper for the solve function of R.  When this function is called
## to get the inverse of a matrix, it checks if the inverse matrix already exists in the 
## cache.  If the inverse exists in cache, then it gets returned.  Otherwise, it performs
## the inverse operation (for the first time) and stores it in cache for subsequent retrieval

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_matrix <- x$getsolve()
        if(!is.null(inverse_matrix)) {
                message("getting cached data")
                return(inverse_matrix)
        }
        data <- x$get()
        inverse_matrix <- solve(data)
        x$setsolve(inverse_matrix)
        inverse_matrix
}

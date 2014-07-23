## These functions are utilities that compute and cache the inverse of a given matrix.  
## This improves performance by not having to calculate the inverse of the matrix unless it changes.

## This function creates a special "matrix" object that can cache its inverse.  The matrix can be updated
## via the 'set' method and retrieved via 'get'.  Additionally, the inverse of the matrix can bet updated 
## via 'setInverse' and retrieved via 'getInverse'.
makeCacheMatrix <- function(x = matrix()) {
		matrixInverse <- NULL
		
		set <- function(y) {
				x <<- y
				matrixInverse <<- NULL
		}
		
		get <- function() x
		
		setInverse <- function(inverse) matrixInverse <<- inverse
		
		getInverse <- function() matrixInverse
		
		list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
		## Return a matrix that is the inverse of 'x'
		
		matrixInverse <- x$getInverse()
		## Check if it's null.  If so, it must be calculated and cached.
		if (!is.null(matrixInverse)) {
				message("getting cached data")
				return(matrixInverse)
		}
		
		matrixData <- x$get()
		matrixInverse <- solve(matrixData)
		x$setInverse(matrixInverse)
		matrixInverse
}

## Tests for the above functions.  Creates a 2000x2000 matrix and uses this as the base matrix for 
## 'makeCacheMatrix'.  The inverse of the matrix is calculated twice and the time taken is measured to 
## ensure that the second time is significantly faster than the first (should be near zero).
testCache <- function() {
		## Generate a random matrix and calculate the inverse
		testMatrix <- matrix(rnorm(4000000), 2000, 2000)
		testCacheMatrix <- makeCacheMatrix(testMatrix)
		nonCachedTimeTaken <- testGenerateInverse(testCacheMatrix)
		message("Uncached inverse took ", nonCachedTimeTaken, " seconds")
		
		## Now try it again and print the time, verifying it is less
		cachedTimeTaken <- testGenerateInverse(testCacheMatrix)
		message("Cached inverse took ", cachedTimeTaken, " seconds")
}

## Utility test function to compute the time taken to get the inverse of the given matrix
testGenerateInverse <- function(testMatrix) {
		startTime <- unclass(Sys.time())
		cacheSolve(testMatrix)
		endTime <- unclass(Sys.time())
		timeTaken <- (endTime - startTime)
		return(timeTaken)
}
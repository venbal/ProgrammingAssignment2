## Given a square and non-singular matrix, these functions create 
## inverse and stores that in cache for lookup.
## For this assignment, as mentioned in the question, assume that 
## the matrix supplied is always square and invertible.

## makeCacheMatrix holds matrix and associated inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(inverseM) inverseMatrix <<- inverseM
    getInverse <- function() inverseMatrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## checks for the existance of inverse in 'makeCacheMatrix', if not creates one and 
## stores it in the cache, otherwise, returns the cached value.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getInverse()
    if(!is.null(inverseMatrix)) {
        message("getting cached inverse matrix")
        return(inverseMatrix)
    }
    ##not in cache, create a new one
    data <- x$get()
    ##solve(..) will create an inverse matrix (error can be ignored as we are dealing
    ##only with the valid square and non-singular matrix)
    inverseMatrix <- solve(data)
    ##store the inverseMatrix
    x$setInverse(inverseMatrix)
    inverseMatrix
}

## Sample output => for this matrix,
## m <- matrix(c(34, 43, 55, 4, 33, 12, 9, 87, 56), nrow = 3, ncol = 3)
## mat <- makeCacheMatrix(m)
##
## the first time output is..
## > cacheSolve(mat)
##              [,1]         [,2]         [,3]
## [1,]  0.03196438 -0.004611776  0.002027591
## [2,]  0.09450165  0.056017175 -0.102214448
## [3,] -0.05164394 -0.007474258  0.037768855
##
## the second time output is.. (from the cache)
## > cacheSolve(mat)
## getting cached inverse matrix
##              [,1]         [,2]         [,3]
## [1,]  0.03196438 -0.004611776  0.002027591
## [2,]  0.09450165  0.056017175 -0.102214448
## [3,] -0.05164394 -0.007474258  0.037768855
##
## Multiplying both the original matrix and this inverse should yield
## identity matrix and that is what coming as a result ->
##              [,1]          [,2]         [,3]
## [1,] 1.000000e+00 -2.255141e-17 1.387779e-16
## [2,] 2.498002e-16  1.000000e+00 1.942890e-16
## [3,] 2.220446e-16 -4.163336e-17 1.000000e+00
##
## This has 1's in main diagonal, and infinitesimally small
## numbers close to 0 in other places, making that as an identity matrix.
##
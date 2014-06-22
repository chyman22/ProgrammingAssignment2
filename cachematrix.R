## 'makeCacheMatrix' makes a special "matrix" that can cache it inverse by:
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse matrix
## 4.  get the value of the inverse matrix

makeCacheMatrix <- function(testmatrix = matrix()) {
        m <- NULL
        set <- function(y) {
                testmatrix <<- y
                m <<- NULL
        }
        get <- function() testmatrix
        setinverse <- function(inverse_matrix) {
                m <<- inverse_matrix
        }
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## 'cacheSolve' computes the inverse of the special
##"matrix" returned by `makeCacheMatrix` above. If the inverse has
##already been calculated (and the matrix has not changed), then
##`cacheSolve` will retrieve the inverse from the cache.

cacheSolve <- function(testmatrix, ...) {
        m <- testmatrix$getinverse()
        if(!is.null(m)) {
                message("Getting cached Matrix")
                return(m)
        }
        input <- testmatrix$get()
        m <- solve(input, ...)
        testmatrix$setinverse(m)
        m
}

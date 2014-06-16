## There are two functions in this file. First function is
## is makeCacheMatrix() which creates a special matrix  
## object to cache inverse of an square invertible matrix.
## The second function is cacheSolve() that provide 
## inverse of a given matrix.

## makeCacheMatrix() function creates a special matrix 
## object and returns a list containing four functions-
## to set the matrix object, to retrieve matrix object,
## to set inverse of matrix object and to get inverse 
## of matrix object

makeCacheMatrix <- function(x = matrix()) {
inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve() function returns inverse of a square 
## invertible matrix. If inverse of a matrix is calculated
## earlier, this function retrieves its value from cache.
## Otherwise calculates the inverse afresh using solve()
## function.

cacheSolve <- function(x, ...) {
         inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached matrix inverse")
                return(inverse)
        }
        dataMatrix <- x$get()
        inverse <- solve(dataMatrix)
        x$setinverse(inverse)
        inverse
}

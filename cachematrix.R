## Put comments here that give an overall description of what your
## functions do


## The functions below save computation time by Caching the inverse of a matrix.
## If the contents of the matrix are not changing, it makes sense to cache the 
## inverse matrix so that when we need it again we can look it up in the cache 
## rather than recomputing it.
## Computing the inverse of a square matrix can be done with the solve function
## in R. For example, if X is a square invertible matrix, then solve(X) returns 
## its inverse.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It is a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = numeric()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The input of the function below is the output of the function makeCacheMatrix.
## cacheSolve computes the inverse of the matrix. If the inverse has already 
## been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value
## of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
## Matrix inversion is usually a costly computation.
## The two functions below removes repeatitive calculation of a inverse of a matrix by cashing it.
## The short discription of the functions is given below:

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## The functions start here:

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the given matrix
## 4. get the value of the inverse of the given matrix

## Any Matrix(whose inverse is possible) is passed as argument to makeCacheMatrix. The output of this function ( i.e. the special matrix) is then passed to the function cacheSolve to get its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inverseOfMatrix <- NULL
        set <- function(newMatrix) {
                x <<- newMatrix
                inverseOfMatrix <<- NULL
        }
        get <- function() x
        setInverse <- function(calculatedInverse) inverseOfMatrix <<- calculatedInverse
        getInverse <- function() inverseOfMatrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The function cacheSolve calculates the inverse of the special "matrix" created with the above function.
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setInverse function.
cacheSolve <- function(x, ...) {
        
        inverseOfMatrix <- x$getInverse()
        if(!is.null(inverseOfMatrix)) {
                message("Getting cached data")
                return(inverseOfMatrix)
        }
        matrx <- x$get()
        inverseOfMatrix <- solve(matrx)
        x$setInverse(inverseOfMatrix)
        inverseOfMatrix
        ## Return a matrix that is the inverse of 'x'
}
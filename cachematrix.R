## The two functions below together compute inverse of a matrix
## Since computing inverse of a matrix can be computationally intensive, it is 
## only performed when the elements of a matrix change, otherwise, the result 
## is returned directly from the cache 

## This function creates a special "matrix" object that can cache its inverse
## The object really is a list containing a function of getters and setters of
## inverse of a matrix and the matrix itself
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    ## return list of getters and setters
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by the
## makeCacheMatrix function above 
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve retrieves the inverse from the cache
cacheSolve <- function(x, ...) {
    ## get inverse of matrix, if it exists, return it 
    ## if not, compute it, save it for future calls, and return it
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## get the matrix
    data <- x$get()
    ## compute inverse of matrix and save it
    ## 'solve' function computes inverse of the matrix
    m <- solve(data, ...)
    x$setinv(m) 
    ## return inverse of the matrix
    m
}

##These functions work together. The makeCaheMatrix function creates the objects
#and functions to set and get the input matrix and the output inverse matrix

#The cacheSolve function calculates the inverse matrix and stores it in 
#makeCaheMatrix to be called when required, skipping the need to repeat the 
#calculation (if the result has already been computed)

##note: The memory consumed by makeCaheMatrix is prevented from being released
#after the function ends by creating an object that will access to 
#makeCacheMatrix environments


##1) It creates a list containing the 4 functions to:
#       set- set the matrix
#       get- get the matrix
#       setinv- set the inverse
#       getinv- get the inverse

makeCacheMatrix <- function(x = matrix()) {
        M <- NULL
        set <- function(y) {
                x <<- y
                M <<- NULL
        }
        get <- function() x
        setinv <- function(inv) M <<- inv
        getinv <- function() M
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


##2) It completes the above function: checks if the inverse matrix has been 
#computed. If so, gets the inverse from the cache and skips the computation
#Otherwise, it computes the inverse of matrix and sets it in the cache via the 
#setinv function

cacheSolve <- function(x, ...) {
        M <- x$getinv()
        if(!is.null(M)) {
                message("getting cached data")
                return(M)
        }
        data <- x$get()
        M <- solve(data, ...)
        x$setinv(M)
        M                       ## Return a matrix that is the inverse of 'x'
}

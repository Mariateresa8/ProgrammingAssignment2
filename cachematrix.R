## The two functions allow to calculate the inverse of a matrix and cache it.

## The function makeCacheMatrix creates special 'matrix' object, that is to say a list containing a function to:
##  set the matrix
##  get the matrix
##  set the inverse of the matrix
##  get the inverse of the matrix


makeCacheMatrix <- function(xx = matrix()) {
        inv <- NULL
        set <- function(yy) {
                xx <<- yy
                inv <<- NULL
        }
        get <- function() xx
        setsolve <- function(solve) inv <<- solve
        getsolve <- function() inv
        list(set = set, get = get,
             setsolve = setsolve ,
             getsolve = getsolve )
}


## The function cacheSolve return the inverse of the matrix returned by the function makeCacheMatrix.
## If the inverse of the matrix has already been calculated, the function returns the inverse from the cache 
## and print the message 'getting cached data'.
##Otherwise, the function calculates the inverse of the matrix and sets the value of the inverse in the cache.

cacheSolve <- function(xx, ...) {
        ## Return a matrix that is the inverse of 'xx'
        inv <- xx$getsolve()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- xx$get()
        inv <- solve(data, ...)
        xx$setsolve(inv)
        inv
}


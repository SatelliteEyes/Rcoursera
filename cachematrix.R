## makeCacheMatrix creates a list of functions: get, which returns the original
## matrix, setinv, which takes input from cacheSolve (or directly, but dont do
## that), and getinv, which returns the inverse of the original matrix
## 
## cacheSolve takes a matrix, checks the getinv function to see if there is a 
## cached inverse. If so, it returns the cached value. If not, it calculates the
## inverse with the solve function and stores the result using the setinv
## function

## makeCacheMatrix stores the inverse of a given matrix and severl functions
## that set and return the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        
}


## cacheSolve checks to see if the inverse is stored and returns the stored 
##value. If there is no stored value, it calculates the inverse and stores it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

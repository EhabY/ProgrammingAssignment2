## These two functions are used to calculate the inverse of a matrix
## then storing (caching) the inverse for later (much) quicker access
## since calculating the inverse is resource expensive

## this function returns a list with functions that allow the user
## to get/set the matrix or its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(m) {
        x <<- m
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(xInv) inv <<- xInv
    getInv <- function() inv
    
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function checks if the matrix inverse is already cached
## if it is then it returns it without recalculation, otherwise
## it recalculated the inverse and stores (caches) it 

cacheSolve <- function(x, ...) {
    cachedVal <- x$getInv()
    if(is.null(cachedVal)) { # value not calculated yet
        cachedVal <- solve(x$get(), ...);
        x$setInv(cachedVal)
    } else {
        message("getting cached data")
    }
    
    cachedVal
}

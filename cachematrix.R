## Inverting a matrix is a costly operation. These two functions
## make this process faster. In this way an inverted matrix,
## once calculated, is saved outside of the function environment,
## so that if it is needed again, R doesn't have to calculate it again.
## It uses the matrix already stored instead.

## This function need an invertible matrix as an argument.
## It creates a new list of 4 functions, a special object
## used by the next function cacheSolve. Each function can be called with $.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {  #set a new matrix as argument and reset the cache
        x <<- y
        inv <<- NULL
    }
    get <- function() x   #give back the matrix
    setinverse <- function(inverse) inv <<- inverse   #save inverse in cache
    getinverse <- function() inv    #give back the inverse matrix
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function uses the object created by the other as an argument.
## It checks the cache to see if there is a saved inverse, otherwise 
## it calculates it. '...' are arguments for solve().

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {   #check cache for inverse, if TRUE return it
        message("retrieving cached data")
        return(inv)
    }
    data <- x$get()   
    inv <- solve(data, ...)
    x$setinverse(inv)   #save new inverse in cache
    inv
}

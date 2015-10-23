# Routines for manipulating a cacheMatrix object which allows one to cache its
# inverse without having to recalculate it every time, thus saving computation time.

# makeCacheMatrix defines the matrix object and access methods for
# manupulating this object: 'getters' and 'setters'. It also defines
# this object's additional property - Inverse and 'getters' and 'setters'
# for it.

makeCacheMatrix <- function(x = matrix()) {
    Inv <- NULL
    set <- function(y) {
        x <<- y
        Inv <<- NULL
    }
    get <- function() 
        x
    setInv <- function(inverse) 
        Inv <<- inverse
    getInv <- function() 
        Inv
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


# cacheSolve manipulates the cacheMatrix object and returns a cached inverse if
# the property is set, otherwise calculates the inverse and sets the Inv property
# of the cacheMatrix, thus caching it.

cacheSolve <- function(x, ...) {
    Inv <- x$getInv()
    if(!is.null(Inv)) {
        message("getting cached inverse")
        return(Inv)
    }
    data <- x$get()
    Inv <- solve(data, ...)
    x$setInv(Inv)
    Inv
}

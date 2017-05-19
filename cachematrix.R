## Put comments here that give an overall description of what your
## functions do

## Creates a Cached matrix and it's inverse
## set the value of the matrix; set(y)
## get the value of the matrix; get()
## set the value of the matrix inverse; set_inv(inverse)
## get the value of the matrix inverse; get_inv()

makeCacheMatrix <- function(x = matrix()) {
    m <- x
    i <- NULL
    get <- function() m
    set <- function(y) {
        m <<- y
        i <<- NULL
    }
    get_inv <- function() i
    set_inv <- function(inverse) i <<- inverse
    list(set = set, get = get, get_inv = get_inv, set_inv = set_inv)
}


## returns the inverse of a given CacheMatrix x
## retrieves cached inverse if one exists
## if cached inverse does not, calculates the inverse of x and caches result

cacheSolve <- function(x, ...) {
    i <- x$get_inv()
    if(!is.null(i)){
        message("getting cached matrix inverse")
        return(i)
    }
    m <- x$get()
    i <- solve(m)
    x$set_inv(i)
    i
}

## The following two functions are used to cache the inverse of a matrix
# makeCacheMatrix accomplishes the following:
# 1. sets the value of the matrix
# 2. gets the value of the matrix
# 3. sets the value of the inverse of the matrix
# 4. gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(value) {
        x <<- value
    }
    get <- function() {x}
    
    cacheInverse <- function(solve) {cache <<- solve}
    getInverse <- function() { cache }
    list(set=set, get=get, cacheInverse=cacheInverse, getInverse=getInverse)

}


# cacheSolve returns the inverse of the matrix. First, it checks to see if
# the inverse has already been computed. If yes, then skip to getting the
# result. If not, it calculates the inverse and sets the value in cache. 

cacheSolve <- function(x, ...) {
    m <-- x$get()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    inverse <- solve(data)
    x$cacheInverse(inverse)
    inverse
    
}

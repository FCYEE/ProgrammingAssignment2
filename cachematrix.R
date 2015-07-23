## functions that cache the inverse of a matrix

## function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <-NULL
        set<- function(y){
                x <<- y
                i <<- NULL
        }
        get<- function() x
        setinv<- function(inv) i <<- inv
        getinv<- function() i
        list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## function computes the inverse of the special "matrix" in makeCacheMatrix
## if it has already been computed, it will return the the computed output

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		        getmat<- x$getinv()
        if (!is.null(getmat)){
                message("Retrieve inverse from cache")
                return(getmat)
        }
        i <- solve(x$get())
        x$setinv(i)
        i
}

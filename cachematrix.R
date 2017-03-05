## These functions allow the caching of the inverse 
## of a matrix so it doesn't have to be re-computed 
## each time it's needed. 

## This function takes a square matrix as its argument 
## and outputs a list of four functions called `set`, 
## `get`, `setinverse` and `getinverse`. 
##
## `set` assigns y to x and NULL to m in the environment
## within which it has been called. `get` outputs the 
## same x. `setinverse` assigns inverse to m and 
##`getinverse` returns m.

makeCacheMatrix <- function(x = matrix()) {
          m <- NULL
          set <- function(y){
              x <<- y
              m <<- NULL
          }
          get <- function() x
          setinverse <- function(inverse) m <<- inverse
          getinverse <- function() m
          list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve takes as its argument a list of four
## functions `set`, `get`, `setinverse`, `getinverse`
## which are the outputs of `makeCacheMatrix`.
##
## cacheSolve outputs the inverse of the square matrix
## which was the argument to `makeCacheMatrix`. 
## If cacheSolve has been called already with the 
## same argument then it outputs the cached solution
## instead of computing it again; it also returns a 
## message "getting cached data".

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

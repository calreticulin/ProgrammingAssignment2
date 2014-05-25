## makeCacheMatrix: create a matrix that cache and invalidate cached inverse Matrix
## cacheSolve: return cached solution whenever possible

# examples
# > m <- matrix( rnorm(1000000,1,1),nrow=1000,ncol=1000)
# > cm <- makeCacheMatrix(m)
# > system.time(cacheSolve(cm))
# user  system elapsed 
# 0.832   0.004   0.836 
# > system.time(cacheSolve(cm))
# getting cached matrix
# user  system elapsed 
# 0.001   0.000   0.000 
  

## wrapper of a matrix object, keep inv as cached variable 
## when the underlying matrix changes, cache is invalidated. 
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(invM) inv <<- invM
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## In cacheSolve, it first check if the inverse if cached and return the cache
## or it compute the inverse and set cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cinv <- x$getinverse()
  if(!is.null(cinv)) {
    message("getting cached matrix")
    return(cinv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv  
}

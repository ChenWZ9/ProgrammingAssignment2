## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## And save results already been calculated to reduce the time calculate!

## This function creates a special "matrix" object that can cache its inverse.
## And the function have four args 
##"set" :set a matrix data
##"get" :get the data
##"setinverse":set the calculated inverse
##"getinverse":get answer
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...){
  m <- x$getinverse()
  if(!is.null(m)) {
    
    message("getting cached data")
    return(m)
  }
  A <- x$get()
  b<-c(rep(1,nrow(A)))
  x$setinverse(solve(A))
  solve(A)
  ## Return a matrix that is the inverse of 'x'
}

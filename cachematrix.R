
##  This part of assignment #2.  I'm swimming in the deep end but willing to 
 ##  give it a shot.  Be kind.  


      ## This function creates special "matrix"
      ## object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {     ## Define function name
  m <- NULL                               
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,         ## returns list of functions
       setinverse = setinverse,
       getinverse = getinverse)
  
}



## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix.

## If the inverse is already calculated (and matrix has
## not changed), then the Cachesolve should retrieve 
## the inverse from the cache


cacheSolve <- function(x, ...) {      
  m <- x$getinverse()
  if(!is.null(m)) {                ## check to see if cached
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)    ## use solve function to take inverse
  x$setinverse(m)
  m               ## Return a matrix that is the inverse of 'x'
}

## Together, the two functions take matrix inverses, but before they do, they check to see 
## if the inverse has already been taken. This is beneficial since matrix inversion is 
## computationally costly. 
# Just like in the vector example, the makeCacheMatrix function outputs a list of 4 functions. 

makeCacheMatrix  <- function(x = matrix()) {
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

# this second function checks to see if the matrix inverse has already been calculated. If it hasnt, it caclulates the inverse. 
cacheSolve <- function(x, ...) {
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

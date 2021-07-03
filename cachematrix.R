### Matrix inversion is usually a costly computation and there may be some 
#benefit to caching the inverse of a matrix rather than compute it repeatedly###


### makeCacheMatrix creates a special "vector", which is really a list 
#containing a function to set the value of the matrix;get the value of 
#the matrix; set the value of the inverse; get the value of the inverse###

makeCacheMatrix <- function(x = matrix()) {
  #set the value of the matrix
  m <- NULL
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  #get the value of the matrix
  get <- function() x
  #set the value of the inverse
  setinverse <- function(inverse) m <<- inverse
  #get the value of the inverse
  getinverse <- function() m
  #Combine all above to a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


### The following function calculates the inverse of the special "vector" created 
#with the above function. However, it first checks to see if the inverse has 
#already been calculated. If so, it gets the inverse from the cache and skips the 
#computation. Otherwise, it calculates the inverse of the data and sets the 
#value of the inverse in the cache via the setinverse function.###

cacheSolve <- function(x, ...) {
  #Check if the inverse has already been calculated
  m <- x$getinverse()
  if(!is.null(m)) 
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  #Compute the inverse of the matrix
  m <- solve(data, ...)
  x$setinverse(m)
  m
  ## Return a matrix that is the inverse of 'x'
}

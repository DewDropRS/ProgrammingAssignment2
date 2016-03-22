## makeCacheMatrix contains functions that 
## sets the matrix
## gets the matrix
## retrieves the inverse matrix via a get function, 
## calculates the inverse via a set function
## it will also list these functions to the console

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
     x <<- y*
     m <<- NULL
  }
  get <- function() x
  setinvm <- function(solve) m <<- solve
  getinvm <- function() m
  list(set = set, get = get, setinvm = setinvm, getinvm = getinvm)
  
}


## This function takes a matrix argument and caches the inverse matrix if it doesn't already exist
## 1. It will first check if the inverse is already cached in the global environment
## If it finds the cached inverse matrix it will bypass calculation with the return(m)
## 2. If the cached inverse matrix cannot be found it will create the inverse and return it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## 1
    m <- x$getinvm()
    if(!is.null(m)){
      message("getting cached inverse matrix")
      return(m)
    }
    ## 2
    data <- x$get()
    m <- solve(data)
    x$setinvm(m)
    m
}

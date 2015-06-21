## @x: a square invertible matrix
## return:set the matrix and get the matrix
## return: set the inverse and get the inverse


makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}


cacheSolve <- function(x, ...) {
  ## @x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  inv = x$getinv()
  #get it from the cache and skips the computation. 
  
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  # sets the value of the inverse in the cache via the setinv function.
  
  x$setinv(inv)
  
  inv
  
}

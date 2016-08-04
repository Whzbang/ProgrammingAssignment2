## The first function caches a matrix so it can be retrieved later.

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL 
  }
  get = function() x 
  setinv = function(inverse) inv <<- inverse
  getinv = function() inv 
  list(set=set, get=get, setinv=setinv, getinv=getinv) 
}



## cachSolve returns the inverse of a matrix but also checks 
## if the inverse had already been calculated. If true then 
## it just retrieves the already calculated value. 

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  if (!is.null(inv)) {
    message("getting cache data")
    return(inv)
  }
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  x$setinv(inv)
  return(inv)
}

x = rbind(c(1, -2), c(-2, 1))
z = makeCacheMatrix(x)
z$get()

cacheSolve(z)

#here we just retrieve the cached matrix 
cacheSolve(z)

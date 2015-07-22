
## The first function makeCacheMatrix is used to create an instance in memory that can store a matrix and cache
## the matrix inverse calculated by the second function cacheSolve.
## In case the cache is NULL i.e. a new matrix is set in the memory instance created by makeMatrix then
## the cacheSolve function calculates the inverse of the matrix and is stored in the memory.
## The matrix can be passed/set while invoking makeCacheMatrix or through the set(matrix) function within makeCacheMatrix. 
## Every invocation of cacheSolve checks whether the matrix is the same (invocation of SET function sets
## the cache to NULL)


## This function creates an instance which accepts a matrix and stores its calcuated inverse in the allocated
## memory area

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y= matrix()) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv <<- inv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function checks whether the cache is empty (either the first invocation or the matrix has changed).
## If cache is available it returns the value of the inverse from the cache, else it calculates the inverse
## stores it in the cache and then returns the inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  else {  
    mat_a <- x$get()
    inv <- solve(mat_a, ...)
    x$setinv(inv)
    return(inv)
  }
}

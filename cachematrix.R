#This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  mtx <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    mtx <<- NULL
  }
  getinv <- function() mtx
  setinv <- function(inverse) {
    mtx <<- inverse
}
  return(list(
    set = set, 
    get = get,
    setinverse = setinv,
    getinverse = getinv
    ))
}

#This function computes the inverse of the special “matrix” returned by makeCacheMatrix created above. 

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- solve(x$get())
  x$setinverse(data)
data
}

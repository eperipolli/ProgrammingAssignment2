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

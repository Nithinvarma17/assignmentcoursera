makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <- NULL
  }
  get <- function() x 
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}

## Returns cached inverse if already calculated - if not, calculates and sets inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}
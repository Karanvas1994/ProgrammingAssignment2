## makeCacheMatrix function makes a list containing 4 functions( namely set,get,setinverse,getinverse)
## cacheSolve firstly,checks for a solution ( inverse of matrix x) . If there is no solution , then it calculates the same.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## this function returns the inverse of the matrix x
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data,diag(nrow(data), ...))
  x$setinverse(inverse)
  return(inverse)
        ## Return a matrix that is the inverse of 'x'
}

## This two functions will cache an inverse matrix of a given matrix
## assume that the given matrix is always invertible.

## The first function will return a list which contains four functions,
## The list can return a matrix or its inverse matrix, and can set each of the two

makeCacheMatrix <- function(x = matrix()) {
  set <- function(y){
    x <<- y
    invmatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invmatrix <<- inverse
  getinverse <- function() invmatrix
  list(set = set,get = get,setinverse = setinverse,getinverse = getinverse)
}


## The second function will check if the inverse matrix of the given matrix is included in the list
## if not, it will solve the inverse matrix and store it into the list

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setmean(inverse)
  inverse
}

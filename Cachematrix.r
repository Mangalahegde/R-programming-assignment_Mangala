## makeCacheMatrix creates a special object, a list. It contains a function to:
## 1. Set the value of matrix. 2. Get the value of matrix. 
## 3.Set the value of inverse matrix. 4.Get the value of inverse matrix.


## Pass matrix into makeCacheMatrix to create a special object

makeCacheMatrix <- function(x = matrix()) {

  inversed <- NULL
  set <- function(y) {
    x <<- y
    inversed <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inversed <<- inverse
  getinverse <- function() inversed
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Pass the special object to CacheSolve in order to get it's inverse form. 
## If it was called before and special object is the same, it'll get the cached data. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inversed <- x$getinverse()
  if(!is.null(inversed)) {
    message("getting cached data.")
    return(inversed)
  }
  data <- x$get()
  inversed <- solve(data)
  x$setinverse(inversed)
  inversed
}

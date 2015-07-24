## These 2 functions together compute the inverse of a special "matrix" object
## and cache it to avoid the resource waste in repeated computation  
## of previously known results.

## This function creates a special "matrix" containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## set the value of the matrix
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  ## set the value of the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse 
  ## get the value of the inverse of the matrix
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix() above.
## If the inverse has already been calculated (and the matrix has not changed)
## then this function retireves the inverse from the cache.

cacheSolve <- function(x, ...) 
{
        
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  ## First run for this matrix x
  data <- x$get()
  ## calculate inverse of matrix x
  inv <- solve(data)
  ## cache result - the inverse of the matrix x
  x$setinverse(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
}


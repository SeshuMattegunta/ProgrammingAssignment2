## The below two functions are used to compute the inverse a square matrix using the solve function
## and retrieve the cached results rather than computing again and again.
##  

## makeCacheMatrix function creates a list of functions to -
## set the value of matrix
## get the value of matrix
## set the inverse of matrix
## get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The following function checks the cache to verify if the inverse of the matrix exists.
## If yes, then it retrieves results from cache.
## If not, it computes the inverse using the Solve function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
  
}

## Sample Run:
##>x <- matrix(1:4,2,2)
##> x
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
##> m = makeCacheMatrix(x)
##> m$get()
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> cacheSolve(m)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(m)
##getting cached data
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> 

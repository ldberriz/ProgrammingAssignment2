## There are 2 functions: `makeCacheMatrix` creates a matrix function that can
## re use its inverse by caching it.  'cacheSolve` computes the inverse of the special
## "matrix" returned by `makeCacheMatrix`

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) { # sets x equal to an empty matrix
  I <- NULL                                 # Set the inverse I equal to NULL
  set <- function(y){  # Internal fx to makeCacheMatrix
    x <<- y     # set function assigns the argument to x.  USes top x outside set()
    I <<- NULL  # re-set Matrix Inverse to NULL.  Points to the M object outside this fx
  } ## so far noting has happened other than creating objects that live at the top of the heap
  get <- function() x     # get function returns the matrix
  setInverse <- function(solve) I <<- solve
  # setInverse overrides the previous value of M and assigns the argument to 
  # M Inverse (which is the inverse of matrix x)
  getInverse <- function() I
  # getInverse returns the Inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  # creates a list of the functions
}


## `This function computes the inverse of the matrix returned by 
## `makeCacheMatrix` above. If the inverse has already been calculated 
## and the matrix has not changed, it returns inverse from the cache.

cacheSolve <- function(x, ...) {
  I<- x$getInverse()  # Retrives the most recent value for the inverse
  if(!is.null(I)){     # # If the value of Inverse exists (!is.null) gei it from the cache
    message("getting cached data")
    return(I)        
  }
  # Otherwise retrive matrix x and calculate the inverse with the R solve() function
  message("newly calculating data")
  data <- x$get()
  I <- solve(data, ...)
  x$setInverse(I)
  # Sets Inverse to the newly calculated value   
  I #Returns the new Inverse value
}

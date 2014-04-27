## The first part is supposed to give the inverted matrix. With <<- I assign a value to the object and 
## this value can be retrieved in the second part of the function outside of it's environment.
## 

## Create an empty matrix where to store inverted matrix. as long as there is no inverted matrix this function 
## will always give back a null value for m.

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvert <- function(solve) m <<- solve
  getinvert <- function() m
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}}


## with this function I can go back and look if the matrix was inverted before, and if it's the case retreive
## the value instead of calculating again the inversion. With the is.null argument I check if the m has no value
## with the ! i invert the argument so that I retrieve the matrix only if it's not 0. If there is a value the 
## message "getting cached data" will be printed. If there is no inverted matrix the x$get argument retrieves 
## the matrix to invert and now it is inverted with the solve function and stored back in the cache with the x$setsolve(m)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinvert()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvert(m)
  m
}

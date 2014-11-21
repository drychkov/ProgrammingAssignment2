## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL   # set initial value of inverse matrix to NULL
    set <- function(y) {  # if we use the makeCacheMatrix for the second time
       x <<- y            # for another matrix, we set inverse to NULL again
       inv <<- NULL
     }
    get <- function() x # returns original matrix
    setinv <- function(solve) inv <<- solve # this method make an inverse matrix
    getinv <- function() inv # returns the inverse matrix
    list(set = set, get = get, # list of methods
         setinv = setinv,
         getinv = getinv)
  
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv() # use the method to get the inverse matrix and store it in 'inv'
  if(!is.null(inv)) { # check if an inverse matrix already computed and cached
    message("getting cached data") # message that it is cached already
    return(inv) # return the inverse matrix and exit
  }
  data <- x$get() # if 'inv' is NULL we get this code, and store the matrix into 'data'
  inv <- solve(data, ...) # get the inverse matrix
  x$setinv(inv) # store the inverse in 'x'
  inv # return the inverse matrix
  
}

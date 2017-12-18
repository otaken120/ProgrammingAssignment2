##Name:Ken Deng
##Date:12/16/2017
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  set_inverse <- function(inv_input) inv <<- inv_input
  
  get_inverse <- function() inv
  
  list(set =set, get =get, 
       set_inverse=set_inverse,
       get_inverse = get_inverse)
}


## Write a short comment describing this function

## If we have already calculated the inverse,
##then it should retrieve the inverse in the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inverse()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  
  inv <- solve(data,...)
  x$set_inverse(inv)
  inv
}

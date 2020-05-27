##  Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly.
##  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##  cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the contents of a matrix are not changing, it may make sense to cache the value of the inverse so that when we need it again, 
##  it can be looked up in the cache rather than recomputed.If the inverse has already been calculated (and the matrix has not changed), then cacheSolve  funtion retrieve the inverse from the cache.


##  x represents a square invertible matrix.
##  The `<<-` operator which can be used to assign a value to an object in an environment that is different from the current environment.
##  function solve() is used to compute the inverse of a square matrix in R. 
##  Below are two functions that are used to create a special object that stores a matrix and caches its inverse.

##  makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##  set the value of the matrix
##  get the value of the matrix
##  set the value of the inverse
##  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() inv
  list(set=set, get=get,
       setinv=setinv, getinv=getinv)
}


## cachesolve function calculates the mean of the special "matrix" created with the above function.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  ## check to see if the inverse has already been calculated. If so, get the inverse from the cache and skip the computation. 
  if (!is.null(inv)){
    
    message("getting cached data")
    return(inv)
  }
  
  ## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the `setinv` function.
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  ## Return a matrix that is the inverse of 'x'
  return(inv)
}


## The following function testInverseMatrix is used to show the difference in time for computing matrix inverse


testInverseMatrix <- function(datamat){
  temp.mat <- makeCacheMatrix(datamat)
  start.time <- Sys.time()
  
  ## First time the cacheSolve function is executed inverse of the matrix is computed
  
  cacheSolve(temp.mat)
  end.time <- Sys.time()
  lapse.time <- end.time - start.time
  print(paste("Start Time",start.time),quote=FALSE)
  print(paste("End Time",end.time),quote=FALSE)
  print(paste("Time Taken",lapse.time," seconds"),quote=FALSE)
  
  start.time <- Sys.time()
  
  ## Second time the cacheSolve function is executed inverse of the matrix is retrived from cache
  cacheSolve(temp.mat)
  end.time <- Sys.time()
  lapse.time <- end.time - start.time
  print(paste("Start Time",start.time),quote=FALSE)
  print(paste("End Time",end.time),quote=FALSE)
  print(paste("Time Taken",lapse.time," seconds"),quote=FALSE)
  
}


## Testing
testInverseMatrix( matrix(rnorm(9000000), nrow=3000, ncol=3000))

# Solution
# [1] Start Time 2020-05-27 19:16:03
# [1] End Time 2020-05-27 19:16:28
# [1] Time Taken 24.9203300476074  seconds
# getting cached data
# [1] Start Time 2020-05-27 19:16:28
# [1] End Time 2020-05-27 19:16:28
# [1] Time Taken 0.0100038051605225  seconds



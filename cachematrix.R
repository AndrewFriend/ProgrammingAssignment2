## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix creates a special "vector", which is really a list 
# containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function
# Calculates the inverse of the special "matrix" created with the above function. 
# However, it first checks to see if the mean has already been calculated. If so, 
# it gets the mean from the cache and skips the computation. Otherwise, it calculates 
# the mean of the data and sets the value of the mean in the cache via the setmean function.
cacheSolve <- function(x, ...) {
 
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

# test example
mycodetest<-function(){
  mytemp=c(7,2,1,0,3,-1,-3,4,-2)
  dim(mytemp)<-c(3,3)
  mytest<-makeCacheMatrix(mytemp)
  myres<-cacheSolve (mytest)
  mychk<-c(-2,8,-5,3,-11,7,9,-34,21)
  dim(mychk)<-c(3,3)
}

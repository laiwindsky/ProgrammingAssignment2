## use the  pair of functions to cache the inverse of a matrix.
##base on the example code,make some change

## "makeCacheMatrix" function provides the parameter of the "cacheSolve" function
##and creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y ## y is missing value ,so the value of x is invariant
    m <<- NULL
  }
  get <- function() x
  setmean <- function(solve) m <<- solve
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## cacheSolve computes the inverse of the special
##"matrix" returned by `makeCacheMatrix` above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get() ## use the function of makeCacheMatrix
  m <- solve(data, ...)
  x$setmean(m)
  m
}
## example of case
a=matrix(1:4,ncol=2,nrow=2)
x<- makeCacheMatrix(a)

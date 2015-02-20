## Put comments here that give an overall description of what your
## functions do
# 1.set the value of the matrix
# 2.get the value of the matrix
# 3.set the inverse matrix of the matrix
# 4.get the inverse matrix of the matrix

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## The following function calculates the inverse matrix  of the special 
## "vector" created with the above function. However, it first checks if the
## calculation is saved in cache from before.

cacheSolve <- function(x) {
  
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setmatrix(m)
  m
}

## Return a matrix that is the inverse of 'x'. Make sure to use nested fuctions.

#Example:
# x
#[,1]  [,2]
#[1,]  1.00 -0.25
#[2,] -0.25  1.00
#> solve(x)
#[,1]      [,2]
#[1,] 1.0666667 0.2666667
#[2,] 0.2666667 1.0666667
#> cacheSolve(makeCacheMatrix(x))
#[,1]      [,2]
#[1,] 1.0666667 0.2666667
#[2,] 0.2666667 1.0666667

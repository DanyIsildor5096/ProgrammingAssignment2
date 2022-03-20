## The next two functions create a special matrix object and calculate or cache the inverse
## of the matrix


## First of all, the makeCacheMaTrix function perform the following tasks:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function (x	= matrix()) {
  i <-  NULL
  set <- function(y){
    x <<- y 
    i <<- NULL
  }
  get <- function()x
  setinversa <- function(inversa) i <<- inversa
  getinversa <- function()i
  list (set = set, get=get, setinversa=setinversa, getinversa=getinversa)
  
}

## In the second place, the next function perform the following tasks:
## 1. Checks if the inverse of the matrix has already been calculated. If so, it gets the inverse
## from the cache and skips computation.
## 2. If the inverse of the matrix has not been calculated then calculates the inverse of the
## matrix created with the first function.

cacheSolve <- function(x, ...) {
  i <- x$getinversa()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinversa(i)
  i
}

## Finally, I make some tests in order to check if the functions work correctly
## So, first I create the following 2x2 matrix
## x <- matrix(c(7, 5, -2, 9), 2,2)
## > x
##    [,1] [,2]
##[1,]   7   -2
##[2,]   5    9
## Then, I calculate the inverse of the matrix
## s <- makeCacheMatrix (x)
## cacheSolve(s)
##             [,1]       [,2]
##  [1,]  0.12328767 0.02739726
##  [2,] -0.06849315 0.09589041
## finally, I can cache the inverse of the matrix wherever I need it.
## cacheSolve(s)
## getting cached data
##         [,1]       [,2]
##[1,]  0.12328767 0.02739726
##[2,] -0.06849315 0.09589041

## Put comments here that give an overall description of what your
## functions do
## Purpose: Matrix inversion is a costly computation that should be
## avoided if the properties of a matrix remain unchanged. This pair
## of functions will compute the inverse of a matrix, create an 
## object to store the calculated inverse, and determine if the 
## inverse must be calculated or retrieved from the cache. 

## makeCacheMatrix will perform the following actions:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve performs one of the following actions:
## 1. computes the inverse of the matrix returned from 
## makeCacheMatrix
## 2. if the inverse has been calculated and the matrix has not 
## changed, then cachesolve should retrieve the inverse from 
## the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("Retrieving cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse (m) 
  m
}
## Sample Run
## x = rbind (c(-2,6),c(6,-2))
## z = makeCacheMatrix(x)
## z$get()
##        [,1] [,2]
## [1,]   -2    6
## [2,]    6   -2

## cacheSolve(z)
## Run 1 No cache data 
##       [,1]   [,2]
##  [1,] 0.0625 0.1875
##  [2,] 0.1875 0.0625
## Run 2 Same matrix
## cacheSolve(z)
## Retrieving cached data
##       [,1]   [,2]
##  [1,] 0.0625 0.1875
##  [2,] 0.1875 0.0625
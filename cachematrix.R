## platform       x86_64-apple-darwin10.8.0   
## os             darwin10.8.0, Apple MacOS X 10.9.4
## version.string R version 3.1.1 (2014-07-10)
## comments as per the style guide of R from google: https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml

## The R script contains two functions. The functions are used to cache the inverse of a
## matrix and then use the inverse in the cache in case the existing matrix has not changed


## ASSUMPTION: kindly note that for the purpose of this project we are assuming that the matrix
##             supplied as argument to the function is always invertible and therefore there are 
##             no validation checks done in order to verify the same

makeCacheMatrix <- function(x = matrix()) {
  # This function creates a special "matrix" object that can cache its inverse
  #
  # Args:
  #   x: a matrix, which is inversible.
  #
  # Returns:
  #   A list of functions which :
  #   1. set        : sets the value of the matrix in cache to NULL
  #   2. get        : gets the value of the matrix from cache
  #   3. setinverse : creates a cache which contains the inverse of the matrix
  #   4. getorigm   : store the matrix in cache for which the inverse is getting calculated
  #   5. getinverse : gets the value of the inverse of the matrix from the cache
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(origm, solve) { 
      # store the matrix for which the inverse is getting calculated
      # storing this matrix will help us know whether the inverse is
      # getting calculated for the matrix x or any other matrix
      origm <<- origm
      m <<- solve
    }
    getinverse <- function() m
    getorigm   <- function() origm
    list(set = set, get = get,
         setinverse = setinverse,
         getorigm   = getorigm,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache 

cacheSolve <- function(x, ...) {
  # This function computes the inverse of the matrix returned by makeCacheMatrix above
  #
  # Args:
  #   x: a list that stores the list of functions returned by the makeCacheMatrix function above
  #
  # Returns:
  # Inverse of a matrix

  m <- x$getinverse()
  # checking whether the inverse of the matrix exists or not 
  # and whether the matrix whose inverse was calculated has changed or not
  if(!is.null(m) && identical(x$get(), x$getorigm())) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(data, m)
  m
}


## code for testing out the above functions
## remove all objects 
rm(list = ls())
## now compile both the above functions
## create an 3 X 3 matrix
set.seed(3)
mat1 <- matrix(rnorm(9),nrow = 3, ncol = 3)
## create an object for 
x <- makeCacheMatrix(mat1)
## now find out the inverse of this matrix
## the below output should not give the line "getting cached data"
cacheSolve(x)
## now executing the above function once again should give us the line
## "getting cached data"
cacheSolve(x)
## now in order to test whether changing the matrix will cause any issues
## or not, therefore whether we are testing for matrix equality let us set 
## another matrix
set.seed(4)
mat1 <- matrix(rnorm(9),nrow = 3, ncol = 3)
x$set(mat1)
## now on executing the below code we should not get the line 
## "getting cached data" since the inverse of the matrix will be set
## once again as the matrix has changed
cacheSolve(x)
## now executing the above function once again should give us the line
## "getting cached data"
cacheSolve(x)

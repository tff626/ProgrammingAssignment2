## Caching the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.
## Below are two functions.

## makeCacheMatrix is used to create a special object that stores a matrix 
## and cache's its inverse.

## The cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
      #The set function sets the value of the matrix
      set <- function(k) {
        x <<- k
        m <<- NULL
      }
    #The get function gets the value of the vector
    get <- function() x
    #Solve the inverse matrix of x
    setinverse <- function(solve) m <<- solve
    #Get the list of matrix in a list
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
  }


cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
  }

## Test this function.
## Step 1. define a matrix
mm=matrix(c(1,2,3,1), nrow=2,ncol=2)
## Step 2. define a specific matrix x that contains the output from
x <- makeCacheMatrix(mm)
## Step 3. For debug reason, check whether the determinant det(mm) is zero or not
det(mm)
## This should generate the first-time output
cacheSolve(x)
## This should return the generated value in the previous command
cacheSolve(x)
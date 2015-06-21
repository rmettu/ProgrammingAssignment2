## 1. The aim of the project is to cache time consuming computations in a Matrix Inverse calculation.
## 2. makeCacheMatrix and cacheSolve functions are used to solve the inverse of the matrix and cache it for furture use.
## 3. I used free floating variable to cache matrix value and "<<-" is used to assign the value to an object in an environment that is different from the current one. 


## Function 1: makeCacheMatrix
## ____________________________

## This function is used to create a list containing a function to:
## set the value of the Matix 
## get the value of the Matrix
## set the value of the Inverse of the Matrix
## get the value of the Inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
          x <<- y
          m <<- NULL
    }
    get <- function() x
    setinversematrix <- function(solve) m <<- solve
    getinversematrix <- function() m
    list(set = set, get = get,
         setinversematrix = setinversematrix,
         getinversematrix = getinversematrix)
  
}


## Function 2: cacheSolve
## _______________________

## The function first checks if the Inverse of the matix is already calculated and cached. 
## If it finds the inverse of the matirx in cache, it gets the Inverse Matrix from the cache and skips the computation step. 
## Else, it computes the Inverse using the predefined R function "solve" and sets the value in the cache.


cacheSolve <- function(x, ...) {
    m <- x$getinversematrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinversematrix(m)
    m
    
}


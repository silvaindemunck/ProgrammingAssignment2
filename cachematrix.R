## Put comments here that give an overall description of what your
## functions do

## Coursera course: 
## Data Science - R Programming
## Week 3 assignment
## User: silvaindemunck
## File: cachematrix.R

## Write a short comment describing this function

## makeCacheMatrix creates a matrix object that can cache its inverse
## <<- operator used to assign a value to an object in environment different
## from current

makeCacheMatrix <- function(x = matrix()) { ## Default mode of matrix
        invMatrix <- NULL                   ## Initialize inv as NULL
        setMatrix <- function(y) {          ## Set value of matrix
                x <<- y
                invMatrix <<- NULL
        }
        getMatrix <- function() x                             ## Get the value of the Matrix
        setInverse <- function(inverse) invMatrix <<- inverse ## Set the value of the invertible matrix
        getInverse <- function() invMatrix                    ## Get the value of the invertible matrix
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse, getInverse = getInverse) ## Refer to the functions with the $ operator
}



## Write a short comment describing this function
## This function computes the inverse of the matrix returned by makeCacheMatrix above
## If the inverse has already been calculated and the matrix has not changed,
## cacheSolve will retrieve the inverse from the cache.
## If inverse matrix from makeCacheMatrix((matrix) has a value in it, 
## it returns the message "getting cached data"

cacheSolve <- function(x, ...) {                            ## Return a matrix that is the inverse of 'x'
        invMatrix <- x$getInverse()                         
        if(!is.null(invMatrix)) {                           ## If inverse matrix is not NULL
                message("getting cached data")              ## Type message: "getting cached data"
                return(invMatrix)                           ## Return the invertible matrix
        }
        MatrixData <- x$getMatrix()                         ## Get the original Matrix Data
        invMatrix <- solve(MatrixData, ...)                 ## Use solve to inverse the matrix
        x$setInverse(invMatrix)                             ## Set the invertible matrix
        invMatrix                                           ## Return the invertible matrix
}

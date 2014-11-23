## R Programming Assignment 2
## dws

## Description:
## cacheSolve() and makeCacheMatrix() work together to compute inverse of square
## matrices, but if inverse of the same matrix is needed again, it is recovered from
## cache rather than repeating inverse calculations.
## test_cacheInverse verifies cacheSolve() and makeCacheMatrix()  .  .  .  .  .  .  .  .
## 
## WARNING: matrix x is assumed to be an invertible square matrix 
## Use only invertible square matrices.

options(digits = 4, width = 80)
setwd("~/Data_Science_Toolbox/datasciencecoursera/ProgrammingAssignment2")
## makeCacheMatrix() creates a special "matrix" object that can cache its inverse
#
makeMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}
#
## cacheSolve() computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated and if the matrix 
## has not changed, then  cacheSolve  will retrieve the inverse from cache.
#
cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

#
## test_inverse() function will verify solve(), makeCacheMatrix(), and cacheSolve() 
#
test_cacheInverse <- function(x = matrix()) {
    print(round(x,4))
    #
    message('Solve for inverse matrix using solve')
    z <- solve(x)
    print(round(z,4))
    #
    message('Invert matrix twice using cacheSolve(makeCacheMatrix(x))')
    x1 <- makeMatrix(x)
    y <- cacheSolve(x1)    
    y <- cacheSolve(x1)
    print(round(y,4))
    #
    message('Verify results are equal')
    print(z == y)
    #
    message('Verify y is inverse')
    print(round(y%*%x,4))
    round(x%*%z,4)    
}
#
## Run test cases
#
# test_cacheInverse(as.matrix(read.csv("sqmatrix.csv", header = FALSE)))
# test_cacheInverse(as.matrix(read.csv("square24.csv", header = FALSE)))
#
test_cacheInverse(matrix(c(3,2,1,6,-1,3,7,4,3),3,3))
test_cacheInverse(matrix(c(1, 2, 8, 9, 2, 5, 3, 0, 4, 4, 2, 7, 5, -2, 1, 6), 4, 4))
test_cacheInverse(matrix(c(20, 2, 3, -3, 2, 10, 2, -2, 3, 2, 40, 9, -3, -2, 9, 33), 4, 4))

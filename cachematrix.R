## Put comments here that give an overall description of what your
## functions do

## The overall purpose of the two functions is to invert a matrix and to store the 
## result in a cache for faster retrieval if the same matrix needs to be inverted again.
## The matrix to be inverted must first be stored in the makeCacheMatrix function, which
## also initializes the variable m. The cacheSolve function checks if the matrix has 
## already been inverted. If yes, it retreives the cached value. If not, then it 
## calculates the inverted matrix using the solve function. The result is stored in m and
## presented to the user at the end.

## Write a short comment describing this function
## makeCacheMatrix creates a list with 4 functions. The 4 functions do this:
## 1. set matrix
## 2. get matrix
## 3. set inverted matrix
## 4. get inverted matrix
## The variable m is used as the carrying variable through the entire process and is used
## by both makeCacheMatrix and cacheSolve. When makeCacheMatrix is first called m is set 
## to NULL. This will make cacheSolve calculate the inverted matrix after which m is 
## equal to the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL                   ## m initialized to NULL
    
    set <- function(y) {        ## the set function is not actually used in this example.
        x <<- y
        m <<- NULL
    } 
    
    get <- function() {         ## stores the matrix x
        x
    }
    
    setsolve <- function(solve) {    ## takes the inverted matrix from cacheSolve
        m <<- solve                  ## and stores it in m 
    }
    
    getsolve <- function() {         ## used to retrieve m. M holds the cached inverted
        m                            ## matrix.
    }
    
    ## the list with the 4 functions
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve) 
}


## Write a short comment describing this function

## cacheSolve takes the output from makeCacheMatrix as argument.
## It retrieves m via getsolve and checks whether m is NULL (first run) or 
## whether m has a value (not NULL). If m has a value then a message is printed followed
## by a print of m. 
## If m is NULL then the local variable 'data' is set to get (which is the matrix, we want
## to invert) and m is set to the inverted matrix vith the solve function.
## The function setsolve is called with m as an argument, which sets m to the inverted matrix.
## When cacheSolve is called again, m is still in the cache holding the inverted matrix.
## The function x$getsolve() returns the value m, which is the result.
## If the user wants to invert a new matrix this must happen via makeCacheMatrix, which
## will reset m to NULL and then the process starts over.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()         ## retrieves m (either NULL or the inverted matrix)

    if(is.null(m)) {          ## The value in the cache is NULL
        data <- x$get()
        m <- solve(data, ...) ##inverts the matrix with solve
        x$setsolve(m)         ## store the inverted matrix in m  
    }  
    else{
        message("getting cached data")
    }

    m
}
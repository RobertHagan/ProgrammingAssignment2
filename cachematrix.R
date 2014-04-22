## Two functions to eliminate the need to repeately re-compute the inverse of a matrix if it has not changed
## The two functions work together to check if the inverse of a matrix has been computed and if 
## it has already been computed, retrieve it from the parent workspace, otherwise compute the inverse
## and put it in the parent workspace for continuing use.

## Parameters should be a square matrix first and the second shuld be a vector giving the right hand
## sie of the linear system.. If b is missing, it is assumed to be an identity vector or matrix 
## so the inverse of x is returned.  To ensure the inverse of the matrix X is returned, no other parameters
## should be included.

## This function creates a cache-able matrix and includes subfunctions to save it in the workspace
## makeCacheMatrix and cascheSolve are to be used together as a pair of functions
## to save time needed to re-compute the inverse of the matrix 


makeCacheMatrix <- function(x = matrix()) {
m <- NULL  #set up a null matrix
    set <- function(y) {    
        x <- y
        m <- NULL  # set up an empty matrix to work with 
       }
    get <- function() x
    setinverse <- function(inverse)  m <<- inverse  #put the inverse in the parent workspace
    getinverse <- function() m
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)  #parameters for saving and getting back the inverse
}


## R script for function to check if inverse is already computed
## if already computed, retrieve it.  If first iteration, compute inverse with solve and return it 
## to be saved in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached inverted matrix if it exist")
      return(m)  #return the previously inverted matrix 
    }
    data <- x$get()
    m <- solve(data, ...)  # invert the matrix and pass it to m
    x$setinverse(m)
    m
}

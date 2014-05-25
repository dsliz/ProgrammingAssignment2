## The following two functions are used to create a special "matrix" object, that stores a matrix and caches its inverse.

## The first function creates a list of functions which can
## set the value of a matrix, get the value of the matrix, set the inverse of the matrix, and
## get the inverse of the matrix.

# A copy of the original matrix will be stored in k, for use later to check if the matrix has changed.
k <<- NULL

makeCacheMatrix <- function(x = matrix(runif(25,1,200),nrow=5,ncol=5)) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    k <<- x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" object created with the above function. 
## However, it first checks to see if the inverse has already been calculated, or if the matrix itself has changed. 
## If either of those conditions are true, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the inverse in the cache via the setinverse function. 

cacheSolve <- function(z, ...) {
        ## Return a matrix that is the inverse of 'z'
    a <- z$get()
    i <- z$getinverse()
    if(!is.null(i) && (k == a)) {
      message("getting cached data")
      return(i)
    }
    data <- z$get()
    i <- solve(data, ...)
    z$setinverse(i)
    i  
  
}

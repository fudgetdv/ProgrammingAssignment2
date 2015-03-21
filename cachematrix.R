## Put comments here that give an overall description of what your
## functions do

## Purpose:  To store a matix and its inverse.
## Usage:    Functions will be run in its child environment.  
##           - To add a function and access its objects from its child environment use <<- operator.
## Output:   A list of accessible functions by the child

makeCacheMatrix <- function(mtx = matrix()) {
  iMtx <- NULL
  set <- function(y){
    mtx <<- y #This function is actually being run in child environment
    iMtx <<- NULL #if matrix is already inverted we need to clear it.
  }
  get <- function() mtx #This is run in child environment.  Since does not exist it looks in parent environment.
  setsolve <- function(invert) iMtx <<- invert
  getsolve <- function() iMtx
  list (set=set, get=get, getsolve=getsolve, setsolve=setsolve)
  
}


## Write a short comment describing this function
## Purpose: Provide the inverse of a matrix.
## Usage:   Retrieves the supplied matrix inverse from parent.  
##          If the inverse does not exist in parent then calculate and store in parent.
## Output:  The inverse of the matrix in parent environment.


cacheSolve <- function(x, ...) {#passing the parent environment to this environment (child)
        ## Return a matrix that is the inverse of 'x'
  iMtx <- x$getsolve() #gets the inverted matrix from parent environment
  if(!is.null(iMtx)){ #checks if the matrix has been inverted in the parent environment
    message("getting cached data")
    return(iMtx) #returns the inverted matrix from this environment
  }
  
  data <- x$get() #gets the matrix from parent environment
  iMtx <- solve(data,...) #stores the inverted matrix in this environment
  x$setsolve(iMtx) #stores the inverterted matrix in parent environment
  iMtx #returns the inverted matrix from this environment
}

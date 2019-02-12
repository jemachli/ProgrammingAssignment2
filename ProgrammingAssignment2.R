## The following function creates a special "matrix" object that can cache its inverse.
## Creation of the funcation follows the below steps:
 
> makeCacheMatrix<-function(x=matrix()){ inv<-NULL set<- function(y) {x<<- y inv<<- NULL } get<- function() x setinverse <- function(inverse) inv <<-inverse getinverse <- function() inv list(set= set, get=get, setinverse=setinverse, getinverse=getinverse)}
Error: unexpected symbol in "makeCacheMatrix<-function(x=matrix()){ inv<-NULL set"
> makeCacheMatrix <- function(x = matrix()) {              ## define the argument with default mode of "matrix"
  +     inv <- NULL                                        ## initialize inv as NULL; will hold value of matrix inverse 
  +     set <- function(y) {                               ## define the set function to assign new 
    +         x <<- y                                      ## value of matrix in parent environment
    +         inv <<- NULL                                 ## if there is a new matrix, reset inv to NULL
    +     }
  +     get <- function() x                                ## define the get fucntion - returns value of the matrix argument
  +     
    +     setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
    +     getinverse <- function() inv                     ## gets the value of inv where called
    +     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
                                                          ## you need this in order to refer 
    +                                                     ## to the functions with the $ operator
      + }
                                        
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has alreacalculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## The function assumes that the matrix supplied is always invertible.
                                        
> cacheSolve <- function(x, ...) {
    +     inv <- x$getinverse()
    +     if(!is.null(inv)) {
      +         message("getting cached data")
      +         return(inv)
      +     }
    +     data <- x$get()
    +     inv <- solve(data, ...)
    +     x$setinverse(inv)
    +     inv
    + }

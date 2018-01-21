## Your assignment is to write a pair of functions 
## that cache the inverse of a matrix.

## Create a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ##Let's be sure we have the correct argument
  
  if(!is.matrix(x)){
    stop("I need a Matrix")
  }
  
  inv<-NULL
  
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  
  ## Defines the getter for the martix 'x'
  
  get<-function() x
  
  ## Defines the setter for the Inverse
  
  setInverse<- function (solve) inv<<-solve
  
  ##Defines the getter for the Inverse
  
  getInverse<- function () inv
  
  ## Make a list with the data
  
  list(
    set=set,
    get=get,
    setInverse=setInverse,
    getInverse=getInverse)

}

## Computes the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv<-x$getInverse()
  
  ## Let's check to see wether the result is null
  
  if(!is.null(inv)) {
    message ("getting cached inverse matrix")
    return(inv)
  }
  
  data<-x$get()
  inv<-solve(data)
  x$setInverse(inv)
  inv
}


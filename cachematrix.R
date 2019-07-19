## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse
MakeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() 
  {
    x
  }
  
  setInverse <- function(matrix) 
  { 
    inverse <<- solve(x) 
  }
  
  getInverse <- function() 
  { 
    inverse
  }
  
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
  #The makeCacheMatrix function will return a list of set,get,setinverse and getinverse. 
  #It stores a vector and that vector's inverse
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x,...) 
{
  inverse <-x$getInverse()
  
  if(!is.null(inverse)) 
  {
    message("getting the cached data ..")
    return(inverse)
  }
  data <- x$get()
  
  m<- solve(data, ...)
  x$setInverse(m)
  m
}

#Testing the code
#aMatrix <- MakeCacheMatrix(x = matrix(c(1,1,2,1), 2,2))
#aMatrix$getInverse()
#cacheSolve(aMatrix)
#cacheSolve(aMatrix)

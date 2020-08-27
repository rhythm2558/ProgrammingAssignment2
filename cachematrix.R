## The functions here will cache the inverse of an invertible matrix. This is done 
## to do fast computations.

## The makeCacheMatrix function is a setter and getter of the matrix and it's inverse.

makeCacheMatrix <- function(x = matrix()){
  invrs <- NULL
  set <- function(y){
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  
  setInvrs <- function(inverse) invrs <<- inverse
  
  getInvrs <- function() invrs
  
  list(set=set,get=get,setInvrs=setInvrs,getInvrs=getInvrs)
}


## cacheSolve function will check first whether the inverse is already computed or not.
## If not computed, it will compute the inverse and set it to the cache so that it can be 
## retrieved again for the same matrix.

cacheSolve <- function(x,...)
{
  invrs <- x$getInvrs()
  if (!is.null(invrs)){
    message("Getting cached data")
    return(invrs)
  }
  data <- x$get()
  invrs <- solve(data,...)
  x$setInvrs(invrs)
  invrs
}

##  This function is very similar to example of assignment. 
##The inv variable store the inverse of matrix. The set function
##allows to modify the matrix, the getinverse and set inverse 
##get the inverse matrix and modify the inverse matrix respectively

## This function creates the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
    set<- function(x){
      x<<- y
      inv <- NULL
  }
    get <-function() x
    setinverse <-function(inverse) inv <<-inverse
    getinverse <-function()inv
    list(set = set, get = get,
         setinverse=setinverse,
         getinverse=getinverse
         )
  
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

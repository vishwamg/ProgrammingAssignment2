## This function is used for storing a matrix object and its inverse 
makeCacheMatrix <- function(x = matrix()) {
     i<-NULL
     set<-function() {
          x<-NULL
          i<-NULL
     }
     
     get<-function()x
     
     setinverse<-function(y)i<<-y
     getinverse<-function()i
     #create and return the list with all function pointers 
     list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function computes the inverse of the matrix passed as an 
#argumnet to this function, if inverse has already been calculated
#then it returns cached inverse

cacheSolve <- function(x, ...) {
     inv<-x$getinverse()
     
     if(!is.null(inv)) {
          print("found inverse matrix in cache, returning")
          inv
     }
      
     matrix<-x$get()
     ## calculate the inverse of the matrix 'x'
     inv<-solve(matrix)
     x$setinverse(inv)
     inv
     
}

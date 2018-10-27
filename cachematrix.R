## The following functions are used to create an object that stores a
##matrix and then caches the inverse. The first function,
##makeCacheMatrix creates a special "matrix", which is actually just
#a list containing a function to : 
#(1)set the value of the matrix; 
#(2)Get the value of the matrix; 
#(3) Set the value of the inverse;
#(4) Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<- function(y){
    x<<-y
    i<<- NULL
  }
  get<- function() x
  setinverse <- function(inverse) i<<- inverse
  getinverse <- function () i
  list(set= set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)

       

}


## This function computes the inverse of the special 
##matrix returned by the makeCacheMatrix above

cacheSolve <- function(x, ...) {
        i<- x$getinverse()
        if(!is.null(i)){
          message("getting cached data")
          return(i)
        }
    data<- x$get()
    i<- solve(data, ...)
    x$setinverse(i)
    i
}

## Hello, the functions below will create and cache the inverse of a matrix 
#and then either retreive the cached inverse or calculate it
#(if it is not available).


##This first function creates an object that will 
#hold the matrix inverese.

makeCacheMatrix <- function(x = matrix()) {
    inversem<-NULL
    set<-function(y){
      x<<-y
      inversem<-NULL
    }
    get<-function() x
    setinverse<-function(inverse) inversem<<-inverse
    getinverse<-function() inversem
    list<-list(set=set,get=get,
               setinverse=setinverse,
               getinverse=getinverse)
  }



## This function will retreive it if it was already calculated, 
#otherwise it will calculate it


cacheSolve <- function(x, ...) {
    inversem<-x$getinverse()
    if(!is.null(inversem)){
      message("getting cached inverse")
      return(inversem)
    }
    data<-x$get()
    inversem<-solve(x)%*%x
    x$setinverse(inversem)
## Return a matrix that is the inverse of 'x'
    inversem
  }
  

## These functions allow user to conduct the proper caching of an inverse matrix. 
## To do so, two functions was generated.
## The first function's goal is to cache a matrix after getting its inverse form.
## The second function's objective is to calculate the cache of a matrix provided by the first function

## The makeCacheMatrix function works as a means to create a cache of the inverse form of a matrix.
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function(){x}
  setinverse<-function(inverse){inv<<-inverse}
  getinverse<-function(){inv}
  list(set=set,get=get,setinverse=setinverse, getinverse=getinverse)
}


## The cacheSolve function solves or calculates the cache of an inverse matrix returned by the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat,...)
  x$setinverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}

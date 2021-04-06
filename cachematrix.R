## These functions allow user to conduct the proper caching of an inverse matrix. 
## To do so, two functions was generated.
## The first function's goal is to cache a matrix after getting its inverse form.
## The second function's objective is to calculate the cache of a matrix provided by the first function

## The makeCacheMatrix function works as a means to create a cache of the inverse form of a matrix.
makeCacheMatrix <- function(c = matrix()) {
  inv<-NULL
  set<-function(d){
    c<<-d
    inv<<-NULL
  }
  get<-function(){c}
  gettheinverseof<-function(inverse){inv<<-inverse}
  gettheinverseof<-function(){inv}
  list(set=set,get=get,gettheinverseof=gettheinverseof, gettheinverseof=gettheinverseof)
}


## The cacheSolve function solves or calculates the cache of an inverse matrix returned by the makeCacheMatrix function.

cacheSolve <- function(c, ...) {
  inv<-c$gettheinverseof()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat,...)
  a$setinverse(inv)
  inv
}

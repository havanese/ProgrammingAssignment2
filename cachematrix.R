## Put comments here that give an overall description of what your
## functions do


##this function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
m<-NULL
  set <-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setMat<-function(solve) m<<-solve
  getMat<-function() m
  list(set=set,get=get,setMat=setMat,getMat=getMat)

}


## this function computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated(and the mean has
## not changed), then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
m<-x$getMat()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <-x$get()
  m<-solve(matrix,...)
  x$setMat(m)
  m
}

## This function will calculate the inverse of a matrix

## This function will create a list of 4 vectors
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<-solve
  getmatrix<-function() m
  result<-list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
  return(result)
}
## This function will check if the result is within the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data, ...)
  x$setmatrix(m)
  m
}

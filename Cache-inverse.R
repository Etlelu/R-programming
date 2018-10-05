############### First function "makeCacheMatrix" ############
#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x=matrix()){
  xinv<- NULL
  set <- function(y) { 
    x <<- y  ##Value of Matrix
    inv <<- NULL ##If new reset to NULL
  }
  get <- function()x
  setinvx<-function(solve) xinv <<- solve
  getinvx<-function() xinv
  list(set = set,get = get,
       setinv = setinvx,
       getinv = getinvx)
}
  ############### Second function "caacheSolve" ############
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x){
  xinv <- x$getinv()
  if(!is.null(xinv)) {
    message("getting cached inverse")
    return(xinv)
  }
  xinvaux <- x$get()
  xinv <- solve(xinvaux)
  x$setinv(xinv)
  xinv
}
##### Example;
x<-2*diag(2)
makeCacheMatrix(x)
cacheSolve(makeCacheMatrix(x))

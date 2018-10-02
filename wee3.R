############### First function "makeCacheMatrix" ############
makeCacheMatrix <- function(x=matrix()){
  xinv<- NULL
  get <- function()x
  setinvx<-function(solve) xinv <<- solve
  getinvx<-function() xinv
  list(get = get,setinv = setinvx,
       getinv = getinvx)
}
############### Second function "caacheSolve" ############
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

## The following functions wrap the solve(a,b,...) function for inverting a square matrix with a Cache
## the scope of the Cache is within a session
## usage: >res <- makeCacheMatrix ( some square matrix )
##        >local_var <- cacheSolve(res)
##        >other_var <- cacheSolve(res)
##the second invocation of cacheSolve(res) will print a message to stderr denotation the usage of cached data


## function makeCacheMatrix input: a matrix, output a list of functions that wrap the matrix, this is a helper function in the context of cacheSolve
## usage:   >res <- makeCacheMatrix( some matrix )       
##          >get() //return the original matrix (if set)
##          >set(some matrix) //will put the matrix object
##          >getinverse() //return the inverse matrix (if set)
##          >setinverse(inverse of some matrix) //will put the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(m_inverse) m <<- m_inverse
  getinverse <- function() m
  return (list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse))

}


## function cacheSolve will wrap the solve(a,b,...) function for inverting a square matrix with a Cache
## it uses the results of makeCacheMatrix in order to get Cache results 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  return(m)
}

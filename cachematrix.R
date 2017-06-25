## makeCacheMatrix is a function that stores a recently computed inverse of a
## matrix in a cache memory. it returns it whenever the same matrix is sent again
## If a new matrix is sent in this function, then it clears the cache memory to
## to store the new inverse matrix.

## This function uses the feature of lexical scoping in R to make changes to 
## to m and x. It does not calculate in inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) m <<- inv
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve uses Solve() funtion in R to calculate the inverse of a matrix if
## the matrix is not alreasy stored in cache. it has access to all functions
## in cacheMakeMatrix because of the list that gets passed on to it. The list 
##contains pointers to the parent environment through the functions.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    print("getting cached data")
    return(m)
  }
  data <- x$get()
  if(nrow(data)==ncol(data))
  {
    m<-solve(data)
  }
  else
  {
    l<-svd(data)
    m<-l$v%*%diag(1/l$d)%*%t(l$u)
  }
  x$setInverse(m)
  m
  
}

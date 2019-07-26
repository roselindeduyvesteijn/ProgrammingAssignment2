#make it possbile to store the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inv <<- solve (x)
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
#calculate the inverse of a particulary matrix
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

#test
mat<-matrix(c(1,2,5,1,4,1,2,5,7),3,3)

mat1<-makeCacheMatrix(mat)
cacheSolve(mat1)
cacheSolve(makeCacheMatrix(mat))

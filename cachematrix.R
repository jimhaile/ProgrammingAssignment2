## Calculates the inverse of a square matrix
## create a square matrix and pass it to makeCacheMatrix
## c <- matrix(1:4,2,2)
## a <- makeCacheMatrix(c)
##
## now send 'a' to cacheSolve
##  b <- cacheSolve(a)
##  b now is a 2x2 inverse of c
# to prove it...
## c %*% b 
## returns to identity matrix

## This function takes a square matrix and stores it internally

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)

}


## This function uses solve to find the inverse of the 
## internally stored matrix in makeCacheMatrix
## and calculates the inverse by using solve()
## if the inverse is not cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}


## makeCacheMatrix
## Function which makes a 'special' vector,
## which actually is a list of functions 
## to set and get a matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve
## looks up if the inverse of a matrix is already available in cache.
## If not, it calculates, stores and returns the inverse.
## If yes, it looks up and returns the inverse
cacheSolve <- function(x) {
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data)
      x$setinverse(i)
      i
}

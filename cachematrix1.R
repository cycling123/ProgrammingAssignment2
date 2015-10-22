
makeMatric <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULLinverse
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function

cacheInverse <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ##A = matrix(c(2, 4, 3, 1),nrow=2,ncol=2)
    
    m <- x$getinverse()
      if(!is.null(m)) {
          message("getting cached data")
          return(m)
      }
    data <- x$get()
    ##m <- inverse(data, ...)
    ##m<-solve(A)
     m<-solve(data)
    x$setinverse(m)
    m
    
    
}

## Functions that work together to return the inverse of a given matrix
## as efficiently as possible.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      ##initialize inverse
      inv <- NULL
      
      ##set environment variable
      env <- environment()
      
      ##print environment info for debug/learning
      ##print(environment())
      ##print(parent.env(env))
      
      ##resets the matrix if passed as argument and resets inv
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      ##retrieves the matrix
      get <- function() x
      
      ##sets the inverse
      setinverse <- function(inverse) inv <<- inverse
      
      ##retrieves inverse
      getinverse <- function() inv
      
      ##retrieves environment
      getenv <- function() environment()
      
      ##returns special "matrix"
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse,
           getenv = getenv)
      
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x) {
      
      ##retrieve the inverse from the special "matrix"

      inv <- x$getinverse()

      ##if the inverse is already set, return the cached data and notify user
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
            
      }
      
      else {
            
            ##load the matrix
            data<-x$get()

            ##solve() for the inverse, set in cache and return
            inv<-solve(data)

            ##set the inverse in cache
            x$setinverse(inv)

            #return the inverse
            inv
      }
}

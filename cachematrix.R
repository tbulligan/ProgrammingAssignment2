## Calculate the inverse of a matrix and cache it for speedier retrieval

## Example usage:
## x <- matrix(rnorm(25), 5)    // Create matrix 'x'
## cm <- makeCacheMatrix(x)     // Initialize command set
## cm$get()                     // Return matrix
## cacheSolve(cm)               // Return inverse
## cacheSolve(cm)               // Call again to return cached inverse

## makeCacheMatrix - Return a list of functions to:
## 1. Set a matrix
## 2. Get a matrix
## 3. Set the inverse
## 4. Get the inverse

makeCacheMatrix <- function(x = matrix()) {

  ## Initialize cache for inverse
  inv <- NULL
  
  ## Set matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Get matrix
  get <- function() x
  
  ## Set inverse
  setinv <- function(inverse) inv <<- inverse 
  
  ## Get inverse
  getinv <- function() inv
  
  ## Return list of functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve - Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  
  ## Check cache
  inv <- x$getinv()
  
  ## Return cached inverse value if present
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Else compute inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  ## Cache inverse
  x$setinv(inv)
  
  ## Return inverse
  inv
}

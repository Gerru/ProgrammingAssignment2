## The following 2 Functions creates a matrix and checks to see if the 
## inverse has been cached or not before it returns or calculates the inverse

## makeCacheMatrix creates a matrix and have functions  
## to set and get the inverse of the matrix
## The "inner" functions are added to a list
## to make them accessable through "variable$get()"
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL   
  set <- function(y) {  ## creates function so set (new) values of the matrix
    x <<- y
    s <<- NULL ## When new values are set to the matrix, s is set to NULL
               ## because the inverse needs to be calculated again
  }
  get <- function() x   ## ## creates function so get the values of the matrix
  setinverse <- function(inverse) {
    s <<- inverse ## sets the values sent from cacheSolve or the inverse can be set manually
  }
  getinverse <- function() s ## returns the values of s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {     
  s <- x$getsolve()
  if(!is.null(s)) {  ## Checks to see if s (Inverse of the Matrix) has already been set
    message("getting cached data")
    return(s)  ## Return a matrix that is the inverse of 'x'
  }
  data <- x$get()  ## Links the Matrix to Data
  s <- solve(data, ...) ## Inverts the Matrix
  x$setinverse(s)       ## sets the inverted matrix to s
  s
}


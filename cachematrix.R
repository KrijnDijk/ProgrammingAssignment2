## The makeCacheMatrix function creates a matrix that can cache its inverse
## The argument passed to it is a matrix x
## It first initializes the inv object to NULL
## Then the getters and setters are defined
## The set function takes y as an argument and sets x in the parent environment to that value
## It also resets the object inv, to clear the cache if new input is given
## The get function takes x from its parent environment (makeCacheMatrix)
## The setInverse function sets the object inv (in the parent environment, makeCacheMatrix) to the calculated inverse matrix
## The getInverse returns the object inv from the parent environment
## The final output of this function is a list that contains the defined functions, making them accessible to the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL 
  set <- function(y) {
    x <<- y  
    inv <<- NULL 
  }
  get <- function() x 
  setInverse <- function(inverse) inv <<- inverse 
  getInverse <- function() inv 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
} 



## The cacheSolve function computes the inverse of the matrix created by makeCacheMatrix.
## If it has already been computed and is stored in its cache, it does not compute it again but instead retrieves it from its cache.
## It first retrieves the inversed matrix (using getInverse) from the object (x) that was passed as its argument
## If the inverse is already computed, it retrieves it from its cache
## If the inverse matrix has not yet been computed, it gets the value of the object (x) (the 'input matrix')
## It then inverts this matrix and stores it in the variable inv and returns it.

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
  ## This returns a matrix that is the inverse of 'x'
}

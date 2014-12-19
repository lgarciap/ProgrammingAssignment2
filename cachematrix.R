## makeMatrix makes a matrix object that can be 
## accessed by sets and gets methods
## cacheSolve calculates the inverted matrix the 
## first time the function is called and returns the 
## value stored in the matrix object the next time that
## cacheSolve is called


## Creates a matrix object

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() {x}     #Getting the original matrix
  setinverted <- function(solve){ i <<- solve} #Setting the inverted matrix
  getinverted <- function() {i} #getting the inverted matrix
  list(set = set, get = get,
       setinverted = setinverted,
       getinverted = getinverted)
}


## Calculates the inverted matrix of the object created with makeCacheMatrix function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverted() 
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get() #get original matrix from makeMatrix
  i <- solve(data, ...) #Inverting the matrix
  x$setinverted(i) #setting the inverted matrix in the object
  i #returning inverted matrix
}

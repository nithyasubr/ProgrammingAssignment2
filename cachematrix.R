## Creating the matrix using function makecachematrix()

makeCacheMatrix <- function(x = matrix()) {
  
  ## m will contain the final result, setting it to null to start off
  m <- NULL
  
  ## creating a setter function that can be used to set the object to the matrix data
  set <- function(y) {
    x <<- y   ##Scoping the passed variable to local
    m <<- NULL
  }
  
  ## creating a getter function that will get the contents of the matrix, given object x
  ## This will just display the contents of x
  get <- function() x
  
  ## calling the solve function to compute the inverse and store it in the variable m
  ## setmatrix is the function that calls solve and stores the result in "m"
  
  setmatrix <- function(solve) m <<- solve
  
  ## calling a function to get the solution - inverse of a matrix in this case
  ## output is just the value of the result m
  
  getmatrix <- function() m
  
  ## output of this function will be the list of function objects created
  ## each of this function can be called with the invoked object
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## The function that inputs a matrix and gives out the inverse
## Fetches the cached inverse if that is available already

cacheSolve <- function(x, ...) {
  
  ## check the current cache to see if the inverse already exists
  m <- x$getmatrix()
  
  ## if above returns a non-null, output the cached data
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## if no cached inverse, get the matrix and compute the inverse
  matrix <- x$get()
  m <- solve(matrix, ...)
  
  ## set the cache with the result
  x$setmatrix(m)
  
  ##output result
  m
}

## Output

##mat <- makeCacheMatrix()
##> mat$set(matrix(1:4,2,2))
##> cacheSolveInverse(mat)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolveInverse(mat)
##getting cached data
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> 
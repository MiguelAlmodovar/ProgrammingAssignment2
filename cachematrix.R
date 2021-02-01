## Put c omments here that give an overall description of what your
## functions do

## The makeCacheMatrix function simply creates an "object" but it's a list, containing the getters and setters for it

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse
    )
}


## The cacheSolve function checks if the inverse of the matrix has already been (using getinverse) and if so, simply returns it. Otherwise, calculates and stores it using the setinverse function

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)){
          print("returning cached...")
          return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        x$getinverse()
}

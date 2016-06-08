# The following functions allow for the inverse of a matrix to be cached
# so it's value can be looked up for future use rather than recomputed.




# This function creates a list of helper functions as well
# as the cache placeholder used to store the inverse of the matrix.
# 
# The helper functions perform the following tasks:
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse
# 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) invx <<- inverse
  getinv <- function() invx
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}




# This function calculates the inverse of the matrix stored in the previous
# function after first checking to see if the inverse has already been calculated.
# If so, it retreives the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse and updating the value of the cache.

cacheSolve <- function(x, ...) {
  invx <- x$getinv()
  if(!is.null(invx)) {
    message("\ngetting cached data\n")
    return(invx)
  }
  data <- x$get()
  invx <- solve(data, ...)
  x$setinv(invx)
  invx
}





# Test with the following example:


# Make matrix
set.seed(1234)
temp <- matrix(rnorm(100), 10, 10)

# Create functions and (NULL) variable in cache
funs <- makeCacheMatrix(x = temp)

# Calculate inverse and store in cache
cacheSolve(x = funs)

# This time inverse is pulled from cache
cacheSolve(x = funs)


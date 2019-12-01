# makeCacheMatrix and cacheSolve are two functions that work together to calculate, solve, and store matrix inverse calculations
# so that the result will be available for subsequent functions without having to recalculate the inverse each time.
# The functions take advantage of the lexical scoping in R to make the code more compact. Lexical scoping determines how R will
# search for the values of objects. The values will be stored in the parent environment so that each environment has
# a distinct copy of the object values defined within it. An environment is created with the assign operator <- and that environment
# would have its own definitions of all the functions entered to the right of the assign operator.
#
# In this instance cacheSolve has references to objects that are created in makeCacheMatrix. For instance when
# Matrix1 <- makeCacheMatrix( matrix ( c(2, -3, 4, 0), nrow = 2, ncol = 2 ) ) is assigned and then cacheSolve( Matrix1 ) is run
# cacheSolve will retrieve object values defined in makeCacheMatrix. Furthermore cacheSolve will check if the inverse matrix has
# already been calculated, in which case it will just retrieve it from memory, or the function will calculate the inverse and
# store it for later use.


# makeCacheMatrix sets up the parent environment and stores the values for the environment to be used by the cacheSolve. 
# makeCacheMatrix uses lexical scoping internally several times by refererring to x and m objects in subsequent functions
# that were defined by earlier functions. makeCacheMatrix also uses the <<- superassignment operator which assigns the value
# in the parent environment and not the current environment of the function. In the end makeCacheMatrix returns a list of values
# which are assigned the same names as the internal functions so that the values can be referenced using the $ operator by the 
# cacheSolve function.
#
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrx <- function(solve) m <<- solve
  gematrx <- function() m
  list(set = set, get = get,
       setmatrx = setmatrx,
       gematrx = gematrx)
}


# The cacheSolve is intended to operate on an environment already set up by the makeCacheMatrix function. If this environment is
# set up cacheSolve will retreive values from makeCacheMatrix with the m <- x$gematrx() reference. Then cacheSolve checks if the
# inverse matrix has been calculated. If the inverse has been calculated the result will be fetched from memory and if not the
# inverse will be calculated and stored in memory.
#
cacheSolve <- function(x, ...) {
  m <- x$gematrx()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrx(m)
  m
}

Matrix1 <- makeCacheMatrix( matrix ( c(2, -3, 4, 0), nrow = 2, ncol = 2 ) )


# Calculate inverse matrix and store in memory
cacheSolve(Matrix1)

#     [,1]       [,2]
# [1,] 0.00 -0.3333333
# [2,] 0.25  0.1666667


# Retrieve previous calculated matrix from memory
cacheSolve(Matrix1)

makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  set <- NULL
  set <- function(y) {
    x <<- y
    set <<- NULL
  }
  get <- function() x
  SetSolvedSet <- function(solve) set <<- solve
  GetSolvedSet <- function() set
  list(set = set, get = get,
       SetSolvedSet = SetSolvedSet,
       GetSolvedSet = GetSolvedSet)
}
 
cacheSolve <- function(x, ...) {
  set <- x$GetSolvedSet()
  if(!is.null(set)) {
    message("Matrix Inverted")
    return(set)
  }
  data <- x$get()
  set <- solve(data, ...)
  x$SetSolvedSet(set)
  set
}

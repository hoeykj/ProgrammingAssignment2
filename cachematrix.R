## I've created two functions — makeCacheMatrix and cacheSolve — in R which can create a matrix and compute its inverse. The functions will also
## alert a user if the matrix has already been cached, but will still produce the proper output. The functions are able to be tested with the 
## SOLVE() function in R, which computes the inverse of a matrix.

## This is the makeCacheMatrix function, which makes a matrix object in R. This function first sets the value of the matrix, then it sets the value of the inverse of the matrix. 

makeCacheMatrix <- function (p = matrix()){
  i <- NULL
  set <- function(matrix){
      p <<- matrix
      i <<- NULL }
  get <- function() p
  setInv <- function(inverse) i <<- inverse
  getInv <- function() i
  list (set = set, get = get, setInv = setInv, getInv = getInv)
}


## This is the cacheSolve function. This function, paired with the makeCacheMatrix function, computes the inverse of each item in a matrix. This can be checked by using the SOLVE() 
## function in R. Also, this function will notify the user if a particular matrix has already been cached. It will report "already cached!" to the user, but the function will still give the 
## correct output. 

cacheSolve <- function(p, ...) {
  i <- p$getInv()
  if(!is.null(i)) {
    message ("already cached!")
    return(i)
  }
  data <- p$get()
  i <- solve(data, ...)
  p$setInv(i)
  i
}

#####################################################################################
# Syed F. Naeem
# 18-8-2016
# Programming assignment#02
#####################################################################################

#The first function, `makeCacheMatrix` creates a special " square matrix", which is
#really a list containing a function to
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the matrix inverse
#4.  get the value of the matrix inverse

#The second function, 'makeCacheMatrix' calculates the inverse of the special 
#"square matrix" created with the above function. However, it first checks to see if the
#inverse has already been calculated. If so, it `get`s the inverse from the
#cache and skips the computation. Otherwise, it calculates the inverse of
#the data (square matrix) and sets the value of the inverse in the cache via the `setInverse`
#function.

#Lines 65 - 81: Test cases are also included to do calculations

##Ist Function
## makeCacheMatrix creates and caches a square matrix
makeCacheMatrix <- function(x = matrix()) {
  matrixDim <- dim(x)
  #first check if it is a square matrix
  if(matrixDim[1] == matrixDim[2]){
    m <- NULL
    setMatrix <- function(y = matrix()) {
    x <<- y
    m <<- NULL
  }
  setInverse <- function(z = matrix()) {
    m <<- z
  }
  getMatrix <- function() x
  getInverse <- function() m
  }
  else{
    return(message('Please enter square matrix!'))
  }
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       getInverse = getInverse, setInverse = setInverse)
}


##2nd Function
## cacheSolve creates inverse of a square matrix from cache
## it then stores its inverse in cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  
  data <- x$getMatrix()
  m <- solve(data)
  x$setInverse(m)
  message('The matrix inverse:')
  m
}

#####
#####Testing of the functions
#####
#####

#2x2 matrix
A = matrix( c(1, 2, 3, 4), nrow=2, ncol=2) 
myMatrix <- makeCacheMatrix(A)
myMatrix$getMatrix()
#to compute and cache the inverse matrix
cacheSolve(myMatrix) 
#to retrieve the inverse matrix from cache
cacheSolve(myMatrix) 

#3x3 matrix
B = matrix( c(1, 1, 4, 0,3,1,4,4,0), nrow=3, ncol=3) 
myMatrix <- makeCacheMatrix(B)
myMatrix$getMatrix()
#to compute and cache the inverse matrix
cacheSolve(myMatrix) 
#to retrieve the inverse matrix from cache
cacheSolve(myMatrix) 
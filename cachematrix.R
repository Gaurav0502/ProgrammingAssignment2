## There are two main functions makeCacheMatrix and cacheSolve.
##The former one sets and retrieves the matrix and its inverse.
##The later one calculates the inverse and caches it.

#A function used to cache the matrix and its inverse
makeCacheMatrix <- function(mt = matrix())
{
  inverseMatrix <- NULL
  #A function which returns the matrix.
  getMatrix <- function(){ mt }
  #A function which sets the matrix to the input matrix.
  setMatrix <- function(inputMatrix)
  {
    mt <<- inputMatrix
    inverseMatrix <<- NULL
  }
  #A function which returns the inverse of the matrix.
  getInverse <- function(){ inverseMatrix }
  #A function which sets the inverse of the matrix to the input matrix. 
  setInverse <- function(invertedMatrix)
  {
    inverseMatrix <<- invertedMatrix
  }
  
  #List with all the functions identified by a mnemonic.
  list(setMatrix = setMatrix,getMatrix = getMatrix,
       getInverse = getInverse,setInverse = setInverse)
}

#A function which calculates and caches the inverse of the matrix.
cacheSolve <- function(mt,...)
{
   #Fetching the inverse of the matrix.
   invertedMatrix <- mt$getInverse()
   if(!is.null(invertedMatrix))
   {
      message("getting cached data.")
      return(invertedMatrix)
   }
   #Calculation of Inverse.
   invertedMatrix <- solve(mt$getMatrix())
   #Caching Inverse of the matrix.
   mt$setInverse(invertedMatrix)
   invertedMatrix
}

## makeCacheMatrix is a function that caches the inverse of the matrix
## cacheSolve is a function to see if the inverse of the matrix has been cached 
## if the inverse of the matrix has already been cached, it returns the inverse
## if not, it calculates the inverse of the matrix and stores it

## This function caches the matrix and its inverse

makeCacheMatrix <- function(thematrix = matrix()) {   ##Global
  inversematrix <- NULL
  getthematrix <- function() thematrix
  setthematrix <- function(thenewmatrix) {
    thematrix <<- thenewmatrix
    inversematrix <<- NULL    ##Define locally
  }
  getinversematrix <- function() inversematrix 
  setinversematrix <- function(newinversematrix) {
      inversematrix <<- newinversematrix   ##local
  }
  list(getthematrix = getthematrix, 
       setthematrix = setthematrix,
       getinversematrix = getinversematrix,
       setinversematrix = setinversematrix)
}


## Output of makeCasheMatrix()
cacheSolve <- function(thematrix, ...) {
  ## Testing to see if set to inverse
  maybeTheInverse <- thematrix$getinversematrix()
  if(is.null(maybeTheInverse)) {    ##is there nothing in the cashe for matrix
      message("computing inverse")
    realinversematrix <- solve(thematrix$getthematrix())
    thematrix$setinversematrix(realinversematrix)
    return(realinversematrix)
  }
  else {   ##Is there something in the cashe
      message("Returning Cache")
    return(maybeTheInverse)
  }
}


##> source("cacheMatrix.R")
##> a <- makeCacheMatrix()
##> a$setthematrix(matrix(1:4, 2, 2))
##> cacheSolve(a)
## computing inverse
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(a)
## Returning Cashe
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
>

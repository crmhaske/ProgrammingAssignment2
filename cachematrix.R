######################################
######     Christie Haskell     ######
######       Sept. 8, 2015      ######
######                          ######
######     cachematrix.R v.1    ######
######################################

#Revision history
#-- none --

#Script contains two functions:
#makeCacheMatrix: contains 4 functions to set and retrieve a square matrix and its inverse
#cacheSolve: returns the matrix inverse from cache, or calculates the matrix inverse, stores it in cache and returns it

#Input: a square matrix
makeCacheMatrix <- function(x = matrix()) {
  
  inv.x <- NULL #Initalize matrix inverse
  
  #Function to set the matrix to cached x and initialize its cached inverse
  set <- function(y) {
    x <<- y
    inv.x <<- NULL
  }
  
  get <- function() x                             #Retrieve the matrix from cache
  setinv <- function(solved) inv.x <<- solved     #Cache the matrix inverse
  getinv <- function() inv.x                      #Retrieve the matrix inverse from cache
  
  #Store the functions in order to retrive them
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

#Input: a cached matrix from the function makeCacheMatrix
#Output: the inverse of the matrix
cacheSolve <- function(x, ...) {
  
  inv.x <- x$getinv() #Retrieve the matrix inverse
  
  #If the matrix inverse exists, retrieve it from cache and return the inverse
  if(!is.null(inv.x)) {
    
    message("Retrieving from cache")
    return(inv.x)
  
  #Otherwise, calculate the inverse, cache it, and return the inverse
  } else {
    
    message("Calculating")
    data <- x$get()                #Retrieve the cached matrix
    inv.x <- solve(data, ...)      #Calculate the inverse of the matrix
    x$setinv(inv.x)                #Cache the matrix inverse
    return(inv.x)
    
  }
}
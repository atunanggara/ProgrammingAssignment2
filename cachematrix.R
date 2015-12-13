## Caching the inverse matrix of a vector, assuming 
## that all matrix is always invertible

## makeCacheMatrix creates a special vector that 
## 1. set the value of matrix
## 2. get the value of matrix
## 3. get the inverse value 
## 4. set the inverse value 

makeCacheMatrix <- function(x = matrix()) {
	     m <- NULL
	     set <- function(y) {
	              x <<- y
		      m <<- NULL
}
	     get <- function()x
	     setInv <- function(solve) m <<- solve
	     getInv <- function() m 
	     list(set = set, get=get, 
		  setInv = setInv,
                  getInv = getInv)
}


## This function calculates the inverse of the vector created with the above 
## function. It will check whether the inverse has been computed before. If
## it has, it will get the inverse from the cache and skips the computation
## If there is no computed Inverse, it compute the inverse and sets the value
## of the inverse in the cache via the setInv function


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getInv()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
}
	data <- x$get()
	m <- solve(data)
	x$setInv(m)
	m
}

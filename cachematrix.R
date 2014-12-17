##Matrix inversion is a rather time consuming task when computing it repeatedly. 
##Instead, caching the inverse of a matrix might be more efficient to do. 

##Create makeCacheMatrix, which is a special matrix object that can cache its inverse. Function 
## contains:
##1. M is a variable which will be used to save the inverse matrix. 
##2. Get() function to obtain raw datamatrix which needs to be inversed.
##3. setmatrix() to assign the inversed matrix of x to m. 
##4. getmatrix() to obtain the cached inverse matrix.
makeCacheMatrix <- function(x = matrix()) {

	##Set the value of makeCacheMatrix. 
	m<-NULL
  	set<-function(y){
  		x<<-y
  		m<<-NULL
	}

	##Gets the value of makeCacheMatrix
	get<-function() x
	
	##Set the value of the inverse of makeCacheMatrix
	setmatrix<-function(solve) m<<- solve
	
	##Get the value of the inverse of makeCacheMatrix
	getmatrix<-function() m
	list(set=set, get=get,
  		setmatrix=setmatrix,
  		getmatrix=getmatrix)
}

## This function actually inverse the matrix of x. The function is based on two steps:
## Checking if the inverse matrix has been found/created. If so, the results are returned. 
## If not, then the inverse of x is computed, saved to cached, and returned. 

cacheSolve <- function(x, ...) {

	##Checking if inversed matrix was created
   	m<-x$getmatrix()
   	if(!is.null(m)){
      	message("getting cached data")
     		return(m)
   	}

	##Get value of m from cached data
    	matrix<-x$get()
   	m<-solve(matrix, ...)
   	x$setmatrix(m)
    	m
}

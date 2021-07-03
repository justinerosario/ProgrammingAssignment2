## Creates a function that will allow caching the inverse of the matrix.

## To create a  matrix that can cache its inverse using the code "makeCacheMatrix"

makeCacheMatrix <- function(x = matrix()) {
	inv<- NULL
	set<- function(y){
		             x<<-y
		             inv<<-NULL
	                  }
    get<-function()x
    setInverse<-function(inverse)inv<<-inverse
    getInverse<-function() inv 
    list (set= set, get= get, setInverse= setInverse,getInverse= getInverse)
    }


## This is the function where it computes the inverse of the matrix created and returned by the makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x%getInverse()
        if(!is.null(inv)){
        	message("getting cached data")
        	return(inv)
        }
        data<-x$get()
        inv<-solve(data,...)
        x$setInverse(inv)
        inv  
}

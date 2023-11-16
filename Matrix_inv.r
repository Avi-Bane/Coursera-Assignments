#Ques:Write the following functions:

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
#Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.

#For this assignment, assume that the matrix supplied is always invertible.

#############################################################################################################################################################################################################################################################
#############################################################################################################################################################################################################################################################

#ANSWERS: Below are a pair of functions that cache the inverse of a matrix.
# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	# initialize the inverse property
	INV <- NULL
	
	# set the matrix
	set <- function(matrix){
		x <<- matrix
		INV <<- NULL
	}
	
	# get the matrix
	get <- function() x
	
	# set the inverse of the matrix
	setInverse <- function(inverse) INV <<- inverse
	
	# get the inverse of the matrix
	getInverse <- function() INV
	
	# return a list of the methods
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
	
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        INV <- x$getInverse()
        
        # return the inverse if it's already set
        if(!is.null(INV)){
        	message("getting cached data")
        	return(INV)
        }
        
        # get matrix from previous function
        mat <- x$get()
        
        # calculate the inverse using matrix multiplication
        INV <- solve(mat,...)
        
        # set inverse to the object
        x$setInverse(INV)
        
        # return the matrix
        INV
}
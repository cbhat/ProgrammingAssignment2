## Put comments here that give an overall description of what your
## functions do

# there are 2 functions - make CacheMatrix and CacheSolve.
# MakeCacheMatrix allows you to store or retrieve the matrix passed in as the input argument or its inverse.
# CacheSolve computes the inverse of the matrix provided. If the cache is not empty, it returns the cached value. If not,
# the inverse of the input matrix is computed and returned.
# There are no checks performed on the input matrix to make sure that it is invertible. It is assumed to be invertible.


## Write a short comment describing this function
# This function create a vector of 4 functions
#
#  Set - Stores the input matrix in a global variable
#  Get - Retrieves  the input matrix
#  Setinverse - Stores the inverse matrix in the parent environment
#  Getinverse - Retrieves the inverse matrix from the parent environment.

makeCacheMatrix <- function(x = matrix()) {

		base::assign("inv" , NULL, envir = parent.frame() )
		
		set <- function(y) {
			x <<- y
			base::assign("inv" , NULL, envir = parent.frame() )
			#inv <- NULL
			
		}
		
		get <- function() x
		
		setinverse <- function(inverse ) {
				print( "Setting Inverse")
				print(exists("inv",  envir = parent.frame() ))
				base::assign("inv",inverse, envir = parent.frame() )
		}
		
		getinverse <- function() {
		
			
				base::get("inv", envir = parent.frame())
				
			 }
		
		
		list ( set = set, get = get, 
				setinverse = setinverse, 
				getinverse = getinverse )
				

}
envir = .GlobalEnv

## Write a short comment describing this function
#  This function performs matrix inversion and stores the result in the cache. When a new request is made to compute the inversion of a matrix,
# it checks the cache to maek sure that the inversion of the matrix is not available. If it is, it returns the cached value.

cacheSolve <- function(x, y) {
        ## Return a matrix that is the inverse of 'x'
		

		do.call(x$set, list(y))
		
		print( do.call(x$get,list()))
		
		
		inv <<- do.call(x$getinverse, list())

		
		if ( ! is.null(inv)) {
			print ( "Returning Cached matrix")
			return (inv)
		
		}
		mat <- do.call(x$get, list())
		
	
		r <- solve(mat)

		do.call(x$setinverse, list(r))
		print(do.call(x$getinverse, list()))

		r
}

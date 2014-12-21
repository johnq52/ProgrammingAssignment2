## Functions calculate the inverse of a square matrix.  

## Function is used to initialize square matrix.  Methods in function are used to calculate inverse of square matrix.

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL      #reset to NULL when makeCacheMatrix is called
        set <- function(y) {    #allows a new value to be substituted for prior object
                x <<- y
                m <<- NULL
        }
        get <- function() x    # returns value of original matrix
        setinverse <- function(solve) m <<- solve   #called by cacheSolve during the first function call
		                                            #stores value using superassignment                  
        getinverse <- function() m  #gets inverse when the inverse of the matrix has been calculated.
        list(set = set, get = get,  #This is a list of internal functions to give calling function access
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function is used to calculate square matrix that was defined in the makeCacheMatrix function.  If the 
## inverse was already calculated, function retrieves stored value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	 m <- x$getinverse()   ##gets inverse of matrix if it has been calculated.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()    #gets original value of matrix.  The inverse has not yet been calculated.
        m <- solve(data, ...)   #calculates inverse of a square matrix.
        x$setinverse(m)         #calculated inverse of a square matrix
        m	
}

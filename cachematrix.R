#    makeCacheMatrix() is a function that creates a special object that supports
#    caching. A square & invertible matrix must be passed to the function. A list
#    of functions adds store and retrieve operations for the matrix and its
#    inverse for use by cacheSolve() or within a command line session. 
#

makeCacheMatrix <- function(parentMatrix = matrix()) {
        # parentMatrix is implicitly initialized in this, the makeCacheMatrix, environment. 
        # Initialize the inverse vector in this environment
        parentInverse <- NULL

            # Define four anonymous functions as follows:
	      #   set()    - Stores or replaces parentMatrix value & clears parentInverse vector   		
            #   get()    - Returns original parentMatrix object from parent environment
            #   setinv() - Stores inverse vector value from CacheSolve() in parent environment  
            #   getinv() - Returns cached inverse from parent environment - may be null if not cached
          
	      set <- function(localMatrix) {		
                parentMatrix <<- localMatrix
		    parentInverse <<- NULL
            }
            get <- function() {  	
	          return(parentMatrix)
	      }			
            setinv <- function(localInverse) {
                parentInverse <<- localInverse
	      }                        
            getinv <- function() { 
                return(parentInverse)	                              
            }
        # Construct and return a named list of the anonymous functions. These appear
        # as a list callable functions within a resulting makeCacheMatrix() object,
        # exposing them as "public".
     
        list (set = set,
	        get = get,
              setinv = setinv,
	        getinv = getinv) 
}

#     CacheSolve() Returns a previously cached inverse, if present, to reduce processing 
#     time. If not, uses $get() to retrieve a new matrix and computes the inverse with solve()
#     and returns it. Function is dependent on both the special mcmObject and the functions
#     from the makeCacheMatrix() object.

cacheSolve <- function(mcmObject, ...) {

        # Retrieve the current inverse value within makeCacheMatrix object. 
        localInverse <- mcmObject$getinv()

        # If value is not null, return the cached inverse. Otherwise, compute / cache / return the inverse.
        if(!is.null(localInverse)) {
             message("Inverse was cached - getting data.")
             return(localInverse)
        }
	  else {
             localInverse <- solve(mcmObject$get())
             mcmObject$setinv(localInverse)
             return(localInverse)
        }
}




## This is a solution for Programming Assignment 2 course id: rprog-035

## makeCacheMatrix sets and gets the matrix and it's inverse. It's nothis but a function nesting setter and getters.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL				#Assign inverse null	
        set <- function(y) {
                x <<- y				
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) 
		inv <<- inverse
        getInverse <- function() inv								
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve computes inverse of a matrix. It first tried to search the cache if the inverse is present.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  inv <- x$getInverse()		      #Get the cached inverse
        if(!is.null(inv)) {   		# Check if it exists
                message("getting cached data") 	#If cached data present return inverse
		    return(inv)
        }
        print("new computation")
	  data <- x$get() 		#If not in cache then compute the inverse
        inv <- solve(data)
        x$setInverse(inv)		#Store this computed inverse for future reference
        inv
	  print(inv)
}

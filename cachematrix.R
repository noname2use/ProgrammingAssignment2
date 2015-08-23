## The first function, makeCacheMatrix creates a special "matrix" object 
## that stores or caches its inverse. The second function, cacheSolve
## then computes the inverse of the special "matrix" object that is returned by makeCacheMatrix. 
## If the inverse  has aleady calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

## This function creates a special "matrix object" which is essentially a list containing a function 
## that can set the value of the matrix, get the value of the matrix, set the value of the 
## inverse matrix and then get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
 	i <- NULL				##creates m in the local environment within function(x = matrix())
        	set <- function(y) {
               		x <<- y			## sets the value of x as y in the parent environment function(x = matrix())
                	i <<- NULL		## sets the value of i in the parent environment function(x = matrix())
        }
        get <- function() x
        setsolve <- function(solve) i <<- solve	## stores the value of i in the parent environment function(x = matrix()) as "solve"
        getsolve <- function() i			## gets the value of i
        list(set = set, get = get,			## stores set, get, set solve and get solve as a list that can be subsetted later in cacheSolve
             setsolve = setsolve,
             getsolve = getsolve)
}


## If there is an existing cached value of the inverse of 'x', the cached value will be returned.
## If not, the inverse of x will be calculated and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 	i <- x$getsolve()
        	if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
}

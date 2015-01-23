## refer comments inside functions for description of functions.
## makeCacheMatrix : creates container for matrix values and parent for functions.
## makeCacheMatrix$set(y) : assigns matrix y to internal variable x & sets  i to null
## makeCacheMatrix$setInverse(inverseIn) : receives inverse of matrix and stores in value i
## makeCacheMatrix$getevn : useful diagnostic function
## makeCacheMatrix$list : useful diagnostic function

makeCacheMatrix <- function(x = matrix()) {
##creates a special "matrix" object that can cache its inverse.
    i <- NULL  ##initialises inverse & sets to null. (inverse is set in setInverse function)
	## print("environment()=")
    ## print(environment())
    evn <- environment()
	## print("parent.env(evn)=")
    ## print(parent.env(evn))
	## set function : insert values into makeVector instance
    ## set parent variables x to input value & i to null
    set <- function(y) {
            x <<- y
            i <<- NULL
    }
    get <- function() x                         ## get - returns x
    setInverse <- function(inverseIn) i <<- inverseIn ## setmean - sets parent m to meanIn
    getInverse <- function() i                  ## getInverse - returns inverse
    getevn<- function() environment()           ## useful for diagnostics 
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse,
         getevn = getevn)                        ## useful for diagnostics
}
## Write a short comment describing this function
cacheSolve <- function(x) {
## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.

        ## Return a matrix that is the inverse of 'x'
		i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)                            ## if inverse was found, function exits here.
        }
		## code below only executed if i is null
        data <- x$get() ## gets vector using makeCacheMatrix child function get 
        i <- solve(data)## solve creates inverse of matrix
        ## set the inverseinside the makeVector object for future use
        x$setInverse(i)
        i   ## returns inverse		
}

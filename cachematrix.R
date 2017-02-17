## Put comments here that give an overall description of what your
## functions do

## This function create an object to cache the original matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
        set <- function(y) {
		    # I use <<- to assign a value to an object in a different environment
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(invmat) inv <<- invmat
        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## This function compute the inverse of a matrix if it has never stored or it retrieves the inverse if already computed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <-  solve(data)
        x$setInv(inv)
        return(inv)
}



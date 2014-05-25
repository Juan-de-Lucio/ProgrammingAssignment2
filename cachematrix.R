## The first function, makeCacheMatrix creates a "matrix" object that can cache its inverse.
## The first function,cacheSolve,calculates the inverse of the matrix.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
	inv_x <- NULL
	set <- function(y) {
		x <<- y
		inv_x <<- NULL
		}
	get <- function() x
	setinv<- function(inverse) inv_x <<-inverse
	getinv <- function() inv_x
	list(set = set, get = get,
	setinv = setinv,
	getinv = getinv)
	}
 
## The function cacheSolve calculates the inverse if the matrix was created with makeCacheMatrix.
## However, it first checks to see if the inverse matrix has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix,  sets the value of the inverse in the cache, and returns it.


cacheSolve <- function(x, ...) {
	inv_x <- x$getinv()
	if (!is.null(inv_x)) {
	message("getting cached inverse matrix")
	return(inv_x)
	} else {
	inv_x <- solve(x$get())
	x$setinv(inv_x)
	return(inv_x)
	}
	}


## SAMPLE:

##> x1 <- matrix(1:4,2,2)
##> x2 = makeCacheMatrix(x1)

##> cacheSolve(x2)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

##> cacheSolve(x2)
##getting cached inverse matrix
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5


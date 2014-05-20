## It's very similar to the example,
## the function makeCacheMatrix takes a matrix and returns a list of 4 functions
## it will also store, but not calculate, an inverse value of the matrix.
##
## the function cacheSolve takes the list from makeCacheMatrix,
## it checks if there is a cached inverse available (stored)
## if so, returns that cached value
## if not, calculate the inverse, sets it as a stored value, returns the inverse


## takes a matrix, returns a list of functions to be called:
##1.set a new matrix
##2.return the matrix
##3.set an inverse of the matrix
##4.return the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL #inv is initially NULL, no cached inverse
    
    set <- function(y) { #1
        x <<- y # with a new value for x,
        inv <<- NULL #inv is now not cached, so back to NULL
    }
    
    get <- function() x #2. returns x, (empty matrix by default)
    setinverse <- function(inverse) inv <<- inverse #3.sets cached inverse
    getinverse <- function() inv #4.returns cached inverse (or NULL)
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns an inverse of a matrix.  To do this,
## it takes the list of functions returned from makeCacheMatrix,
## from that list, it calls functions to:
## 1.return the cached inverse value
## 2.check if the cached value is NOT NULL
## 3a.return non-NULL inverse - or -
## 3b.calculate and return inverse value, plus cache it for later.

cacheSolve <- function(x, ...) {
        
    inv <- x$getinverse()#1. gets the stored inverse value
    
    if(!is.null(inv)) { #2.checks if inverse is NOT NULL
        message("getting cached inverse")
        return(inv) #3a. return non-NULL inverse
    }
    data <- x$get()
    inv <- solve(data, ...)#3b. calculate inverse
    x$setinverse(inv) #3b. cache new calculated (non-NULL) inverse
    inv #3b. return new calculated inverse
}

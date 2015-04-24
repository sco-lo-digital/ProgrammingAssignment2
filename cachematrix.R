## Scott Jacobs Assignment 2 for Coursera course R Programming
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        #z is the inverse, initially assigned value of NULL
        z <- NULL
        #set is the function changing the vector store in the main function
        #by using <<-, we substitute the vector x with that of y
        set <- function(y) {
                x <<- y
                z <<- NULL
        }
        #get is a function that returns the vector x from the main func
        get <- function() x
        #this stores the inverse value for cache sake
        setinverse <- function(inverse) z <<- solve
        #this returns the inverse for cache sake
        getinverse <- function() z
        #the following list stores the functions as function list
        #so when we assign makeCacheMatrix to an object, it is passed the 4 funcs
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cachesolve <- function(x, ...) {
        #this assigns z to the getinverse value from makeCacheMatrix
        z <- x$getinverse()
        #make sure this value is not NULL using !
        if(!is.null(z)) {
                message("getting cached data")
                #return the inverse
                return(z)
        }
        #data gets the vector stored in makeCacheMatrix
        data <- x$get()
        #z calcs the inverse of the vector
        z <- solve(data, ...)
        #x$setinverse(z) stores the inverse in the object generated with makeCacheMatrix
        x$setinverse(z)
        z
}
        ## Return a matrix that is the inverse of 'x'


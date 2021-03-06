## These functions allow to create and cache a matrix and its inverse
## 

## This function create the matrix to be cached

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) { # defining the set function to assign values to the matrix object 
                x <<- y
                m <<- NULL
        }
        get <- function() x # return the current value of the matrix
        setinverse <- function(inverse) m <<- inverse # this function save the current values of the inverse 
                                                      # to the variable that act as the cache.
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Calculate the inverse of a matrix of type makeCacheMatrix

cacheSolve <- function(x, ...) {

        m <- x$getinverse() #get the current inverse matrix
        
        # Evaluating that the current inverse matrix is not null
        # If it has already values then return it
        if(!is.null(m)) { 
                message("getting cached data")
                return(m)
        }
        
        data <- x$get() 
        m <- solve(data, ...) #Computing the inverse of the matrix
        x$setinverse(m) #Caching the matrix computed
        
        m ## Returning the matrix that is the inverse of 'x'
}

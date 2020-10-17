## The functions are used to create and inverse the matrix, store it to cache 
## and get it from cache if necessary to exclude additional computations.

## makeCacheMatrix() creates a list of functions needed to store inverted matrix
## to cache and get the inverted matrix from cache if it is already cached

makeCacheMatrix <- function(x = matrix()) {

        ## empty cache
        m <- NULL

        ## sets the matrix for cacheSolve()
        set <- function(y){
                x <<- y         ## sets the value to different environment
                m <<- NULL
        }

        ## gets the matrix for cacheSolve()
        get <- function() x

        ## stores inverted matrix to cache
        setinv <- function(solve) m <<- solve

        ## gets inverted matrix if it's already computed and stored
        getinv <- function() m

        ## list of functions returned by makeCacheMatrix()
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve() checks cache if inverted matrix already computed and get the
## cached data, otherwise computes it

cacheSolve <- function(x, ...) {
        
        ## cache
        m <- x$getinv()
        
        ## checks if inverted matrix already computed
        if(!is.null(m)) {
                
                ## gets from cache and skips computations
                message("getting cached data")
                return(m)
        }
        
        ## otherwise computes inverted matrix by solve() function
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        
        ## Return a matrix that is the inverse of original one
        m
        
}

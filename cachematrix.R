#makeCacheMatrix function defines the variable (placeholder) for the
#future inverse matrix and creates a list containing four subfunctions
#to set and get the value of the original and inverse matrixes.
makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        
        set <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(inv) invMatrix <<- inv
        
        getinv <- function() invMatrix
        
        list (set = set, get = get, setinv = setinv, getinv = getinv)
}

#cacheSolve function returns the inverse of the original matrix. 
#If the inverse matrix has been computed already, it returs it from the cache.
#If the inverse matrix has not been computed yet, it computes the inverse 
#and sets it in the cache by setinv function
cacheSolve <- function(x, ...) {
        
        invMatrix <- x$getinv()
        if (!is.null(invMatrix)) {
                message ("getting cached data")
                return(invMatrix)
        }
        
        data <- x$get()
        invMatrix <- solve(data)
        x$setinv(invMatrix)
        invMatrix
}
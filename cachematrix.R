
## makeCacheMatrix creates a Matrix object that can cache it's inverse
## also the function creates the closures to get and set x or the inverse of x

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m_inverse <- NULL
        set <- function(y) {
                x <<- y
                m_inverse <<- NULL
        }
        
        get <- function() x
        set_inverse <- function (inverse) m_inverse <<- inverse
        get_inverse <- function () m_inverse
        list( set = set, get = get,
              set_inverse = set_inverse,
              get_inverse = get_inverse)

}


## cachSolve computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_inverse <- x$get_inverse()
        if(!is.null(m_inverse)) {
                message ("getting cache data")
                return(m_inverse)
        }
        data <- x$get()
        m_inverse <- solve(data)
        x$set_inverse(m_inverse)
        m_inverse
        
}

#the makeMatrix function has a matrix as an input and caches it.


makeMatrix <- function(x= matrix()){
        #this function creates a special matrix object that can cache its inverse
        #The output of this function is a list
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


cacheSolve <- function(x, ...) {
        # this function gets the cache data and returns the inverse of
        # the matrix cached in "makeMatrix"
        m <- x$getinv()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data,...)
        x$setinv(m)
        m
}

                


## Put comments here that give an overall description of what your
## functions do

## This function will create a "special" matrix object that can cache its inverse.
##This "special" matrix is possible because of the <<- operator, which allows us to assign a value
##to an object in an environment that is diff from the current environment 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <-function(y){
                x <<-y
                m<<- NULL
        }
        get <-function() {x}
        setinv <- function(inv) {m<<-inv}
        getinv <-function() {m}
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Function calculates the inverse of the matrix from makeCacheMatrix. First checks
## to see if the mean has already been calculated and grabs from Cache if so. Otherwise it does the calc and cache's it

cacheSolve <- function(x, ...) {
       m <-x$getinv()
       if(!is.null(m)) {
               message("getting cached data")
               return(m)
       }
       data <-x$get()
       m<-solve(data)
       x$setinv(m)
       m
}

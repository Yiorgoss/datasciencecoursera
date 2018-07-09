## Two functions, to cache a matrices and check whether new matrices have to be
## calculated, or whether they are already cached.

## i begins as null, upon finding the inverse of the matrix, the i will be set
##      set as the inverse of the matrix, allowing for it to be retrieved
##      without much computational power required.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function(){
        x
    }
    setInverse <- function(inverse){
        i <<- inverse
    }
    getInverse <- function(){
        i
    }
    list(get=get, set=set, setInverse=setInverse, getInverse=getInverse)

}


## if inverse of matrix is NOT cached, get the inverse, put it into the cache,
##      and return the value

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setInverse(i)
    i
    #x$getInverse(i) also works.
}

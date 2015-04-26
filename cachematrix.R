

## THis function creates metrix objects to cache inverse 

makeCacheMatrix <- function(x = matrix()) {
        imatrix = NULL
        set = function(y) { 
                x <<- y
                imatrix <<- NULL
        }
        get = function() x
        setinv = function(inverse) imatrix <<- inverse 
        getinv = function() imatrix
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}



## This function calculates metrix inverse. If it already exists then return from the cache

cacheSolve <- function(x, ...) {
        imatrix <- x$getinv()
        
        # inverse exists
        # if (!is.null(imatrix) & identical(imatrix,x$get())){
        if (!is.null(imatrix)){ 
                message("exists so getting it from cache")
                return(imatrix)
        }
         
        nandin.data = x$get()
        imatrix = solve(nandin.data, ...)
        
        x$setinv(imatrix)
        
        return(imatrix)
}
## Function to test the metrix inverse with and without cache

testmatrixcache = function(nandin){
      
        passedmatrix = makeCacheMatrix(nandin)
        
        start.time = Sys.time()
        cacheSolve(passedmatrix)
        print(Sys.time() - start.time)
        
        start.time = Sys.time()
        cacheSolve(passedmatrix)
        print(Sys.time() - start.time)

}

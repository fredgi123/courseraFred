# There are  2 functions created. The first one store a special matrix and cache its inverse then the second retrieves
## ther inverse if it's been calculated already or computes the inverse otherwise.

## The first function "makeCacheMatrix" creates a matrix object and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invse<-NULL
        set<-function(y){
                x<<-y
                invse<<-NULL
        }
        get<-function()x
        ## set the value of the inverse
        setInverse<-function(inverse)invse<<-inverse
        ## gets the value of the inverse
        getInverse<-function()invse
        ## creates a list with the different value associated with functions
        list(set=set,get=get,
             setInverse=setInverse,
             getInverse=getInverse)
        
}


## The "cacheSolve" function computes the inverse of the matrix created by makeCacheMatrix. Shall the matrix
## already be calculated, the function will get the inverse matrix from the cache.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        invse<-x$getInverse()
        ## tests if an inverse already exists in the cache and return it if so altogether to a message
        if(!is.null(invse)){
                message("Retrieving data from cache")
                return(invse)
        }
        m<-x$get()
        ## computes the inverse of the matrix if this is the first time
        invse<-solve(m,...)
        ## sets the value of the inverse of the matrix in the cache if it is the first time
        x$setInverse(invse)
        ## return the inverse of the matrix if that is the first time it is calculated
        invse
}

##Testing the code
## Creating a special matrix "m"
m <- matrix(c(1,2,0,4),2,2)
## storing in cache
m1 <- makeCacheMatrix(m)
cacheSolve(m1) #inverse returned after computation (did not exist yet)
cacheSolve(m1) #inverse returned from cache as it now "exists" 


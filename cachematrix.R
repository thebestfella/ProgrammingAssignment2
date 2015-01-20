#-------------------------------------------------------------
#   @Objective: it's a function to create an object
#               that can read and set the original 
#               and the inversed matrix.
#   @author:    Peter Wang
#   @date:      Jan, 20, 2015
#-------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) 
{
    #variables
    inverse <- NULL;
    
    #definitions of methods to the object
    set<-function(y)
    {
        #stored to cached, 
        #inverse is reset to NULL when the x is set
        x <<- y
        inverse <<- NULL
    }
    get <- function () x
    setInverse <- function(inv) inverse<<-inv
    getInverse <- function() inverse
    
    #list of methods belongs to the object
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


#-------------------------------------------------------------
#   @Objective: it's a function to cache the inversed 
#               matrix by first reading the inverse 
#               matrix from object and check if it's null, 
#               if not it will return the inversed matrix. 
#               if yes then the original matrix will be 
#               retrived and the inversed matrix will be 
#               calculated and stored back to the object, 
#               then the inverse matrix will be returned.
#   @author:    Peter Wang
#   @date:      Jan, 20, 2015
#-------------------------------------------------------------
cacheSolve <- function(x, ...) 
{
    #read the "inverse" matrix from object x
    inverse <- x$getInverse()
    
    #if it's not null then return the inverse 
    if(!is.null(inverse))
    {
        message("getting cached data!")
        return (inverse)
    }
    
    #if it's null then calculate the inverse then store the
    #inverse into cache before returning the inversed matrix
    else
    {
        message("calculating and stored to cached data")
        data <- x$get()
        inverse <- solve(data)
        x$setInverse(inverse)
        return(inverse)
    }
    ## Return a matrix that is the inverse of 'x'
}

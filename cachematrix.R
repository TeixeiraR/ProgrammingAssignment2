#Assuming that the matrizes that we are working are invertible, 
# the makeCacheMatrix function return a list,created in a "new environment", 
#that have four elements containing:
	
#--The set function create a new environment given a code name and have 
#the object m defined in this environment;
#--The get function stored the "value" of the element (matrix) to be cache;
#--The setinv stored the "value" of the inverse matrix of the matrix cached;
#--getinv function have the "value" assignment to m in this environment that
#is stored in setinv.


makeCacheMatrix <- function(x = matrix()) {
		m <- NULL                             
                                                                               
  		set <- function(y) {                  
                                       
   						x <<- y                             
    						m <<- NULL                          
                                        
 					 }
 
		 get <- function() x                   
                                                                              
  		 setinv <- function(solve) m <<- solve  
                                        
  		 getinv <- function() m                                       
                                       
  		 list(set = set, get = get, setinv = setinv, getinv = getinv)

}



#--The cacheSolve function "calculate" the inverse of the special "matrix" 
#created with the makeCacheMatrix function.
#--First start to check if the inverse matrix has already been calculated in 
# the "new environment" above defined.
#--If so, it gets (returns) the inverse matrix from the cache and skips 
#the computation. 
#--Otherwise, it calculates the inverse matrix of the matrix and stored 
#the "value" of the inverse matrix in the cache via the setinv function.



cacheSolve <- function(x, ...) {
   			m <- x$getinv()                      
                                      
  		      if(!is.null(m)) {                                
    						message("getting cached matrix")
    						return(m)
  						}
 			
			 data <- x$get()                     
                                                                             
  			 m <- solve(data, ...)                
                                       
  			 x$setinv(m)                          
                                       
  			 m                                     
      
}

##This function caches the inverse of a matrix.
##The first function, makeCacheMatrix creates a special "matrix", 
##The second function calculates the inverse of the special matrix.
##Step 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix<-function(x = matrix()){ 
        m<-NULL 
        set<-function (y){   
        ##set the value of the matrix and clear the old value in the cache.    
                x<<-y    
                m<<-NULL 
               }   
        get<-function() x    
        ##Define function to set inverse.    
        setinverse<-function(inverse) m<<- inverse    
        getinverse<-function() m    
        ##Return a list with the above four functions    
        list(set=set, get=get,         
             setinverse=setinverse,        
             getinverse=getinverse )
}
##Step2. Create cacheSolve: This function computes the inverse of the special "matrix" returned
##by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache.
cacheSolve<- function(x,...){ 
        m<-x$getinverse()  
        if(!is.null(m)){    
                message ("getting cached data")    
                return (m)  
        }  
        data<-x$get()  
        ##Computing the inverse of the matrix  
        m<-solve(data, ...)  
        x$setinverse(m) 
        m
}

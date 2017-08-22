
#create a matrix object that caches its inverse for future reference 
 

makeCacheMatrix <- function(x = matrix()) #creates a function where the argument is a matrix 
    { 
    inv <- NULL #initializes a variable that will hold the value of the matrix's inverse 
    set <- function(y) 
        {  
    x <<- y #set x to the y argument in function 
    inv <<- NULL 
        }
    
 get <- function() x #define the get function to return the value of the matrix 
 setinverse <- function(inverse) inv <<- inverse #sets the inverse value 
 getinverse <- function() inv #gets the value of the inverse 
 list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) #returns the values for 'get', 'setinverse', 'getinverse'
    } 
 

 
 cacheSolve <- function(x, ...)  #crete a function to compute the inverse of the matrix if it is not cached
     { 
       inv <- x$getinverse() #sets the value equal to the inverse found from previous formula
       if(!is.null(inv)) 
           { #if the inverse is not null (i.e - a cached inverse exists) 
             message("getting cached data") #show that RStudio is pulling the cached matrix 
             return(inv) #return the matrix inverse 
           } 
       #if the inverse is null (not previously cached) 
       data <- x$get() #get the matrix data 
       inv <- solve(data, ...) #take the inverse of the matrix 
       x$setinverse(inv) #set the inverse of the matrix to the inv variable
       inv #return the inverse of the matrix 
     } 

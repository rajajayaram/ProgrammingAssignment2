#cachematrix.R - written by Raja Jayaraman 
## Put comments here that give an overall description of what your
## functions do
  
## Write a short comment describing this function
  
##This function makeCacheMatrix gets a matrix as an input
#it sets the value of the matrix,gets the value of the matrix
#set the inverse Matrix and get the inverse Matrix. 
 
  
#used <<- operator to assign a value to an object in an environment different from the current environment

#require(MASS) # for ginv function as an alternative to solve because it does not have issues with singular non singular matrices

#matrix as an input
makeCacheMatrix <- function(x = matrix()) {
matrixinverse <- NULL
      
#matrix inverse set function to set the value of the matrix
setthematrix <- function(y) {
            x <<- y
            matrixinverse <<- NULL
}

#get function to  the get the value of the matrix        
getthematrix <- function() x

#set the value of the matrix inverse
settheinverse <- function(inverse) matrixinverse <<- inverse

#get the value of the matrix inverse
gettheinverse <- function() matrixinverse                     

list(setMatrix = setthematrix, getMatrix = getthematrix,setInverse = settheinverse, getInverse = gettheinverse)
        
}
  
## Write a short comment describing this function

## cacheSolve function description 
## The function cacheSolve takes the output of the previous matrix function makeCacheMatrix(matrix) as input 
# It checks whether inverse matrix from makeCacheMatrix(matrix) has any value stored in it or not stored
# In case that the inverse matrix from makeCacheMatrix((matrix) is NULL, it gets the original matrix 
# and set the inverse of matrix by using the solve function from MASS package(ginv from MASS package can also be used to be compatible singular matrices and produce no errors)
# In case inverse matrix from makeCacheMatrix((matrix) has some value in it (applicable after 1st execution)
# it returns a message  "Getting Cached Inverse Matrix" 
# and returns the cached object


cacheSolve <- function(x, ...) {
        
#get the cached value of the inversed matrix from the makeCacheMatrix function

matrixinverse <- x$getInverse()

#if matrixinverse is not NULL
if(!is.null(matrixinverse)) {              
message("Getting Cached Inverse Matrix")   #message: Getting Cached Inverse Matrix 
return(matrixinverse)                      #returning the inverse of the matrix
}
                  
#if value of the matrix inverse is NULL then  
newmatrix <- x$getMatrix()            #get the original matrix data 

matrixinverse<-solve(newmatrix, ...) #getting to use solve function (but this cannot deal with singular & non singular matrices)

#matrixinverse <- ginv(newmatrix)      #alternatively can use the  ginv function in MASS package(since it can deal with both singular & non singular matrix) to inverse the matrix

x$setMatrix(matrixinverse)            #set the matrix inverse

return(matrixinverse)                 #return the matrix inverse
## returning a matrix inverse in matrixinverse variable that is the inverse of 'x' which is received as a parameter in cacheSolve
}
    
## mainprogram to test the written functions
## to run this program with ginv uncomment the require(MASS) and uncomment the line where ginv is used and comment the line where solve function is used

## creating and testing a 2*2 matrix 
testmatrix <- matrix(c(1:4),2,2)
testmatrix

cachedmatrix <- makeCacheMatrix(testmatrix)
cachedmatrix$getMatrix()
cachedmatrix$getInverse()

cacheSolve(cachedmatrix) # 1st time setting 
cacheSolve(cachedmatrix) #2nd time getting from cache

## creating and testing a 3*3 matrix
testmatrix <- matrix(1:9,3,3)
testmatrix
    
cachedmatrix <- makeCacheMatrix(testmatrix)
cachedmatrix$getMatrix()
cachedmatrix$getInverse()
      
cacheSolve(cachedmatrix) # 1st time setting
cacheSolve(cachedmatrix) #2nd time getting from cache
                
## creating and testing a 4*4 matrix

testmatrix <- matrix(1:16,4,4)
testmatrix

cachedmatrix <- makeCacheMatrix(testmatrix)
cachedmatrix$getMatrix()
cachedmatrix$getInverse()

cacheSolve(cachedmatrix) # 1st time setting
cacheSolve(cachedmatrix) #2nd time getting from cache
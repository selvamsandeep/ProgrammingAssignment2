## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.
## 

##1 setMat set the elements for matrix
##2.getMat get the elements for matrix
##3.setInv set the Inverse matrix
##4.getInv get the Inverse matraix

makeCacheMatrix <- function(x = matrix()) {
    
    i<- NULL
    setMat<-function(y){
        x <<- y
        i <<- NULL
    }
    getMat<-function()x
    setInv<-function(solve)i<<-solve
    getInv<-function()i
    
    list(setMat=setMat, getMat=getMat, 
         setInv=setInv, getInv=getInv)    

}


## cacheSolve  function calculate the inverse of matrix 
## if it is square Matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ## first check whether Martix Inverse computed 
    i<-x$getInv()
    
    if(!is.null(i)){
        message("getting chached MatInv")
        return (i)
    }
    
    ## get the matrix 
    mat<- x$getMat()
    
    ## compute Inverse matrix if it square matrix
    if(ncol(mat)== nrow(mat)){
      i<- solve(mat)
    }else{
        message("Input invertable square Matrix")
    }
    ## set the invese matrix
    
    x$setInv(i)
    i
    
}

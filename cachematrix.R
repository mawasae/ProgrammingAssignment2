## My functions are similar to that described in coursera example... 
## in the function below , m is set to null when matrix has never been computed but if it is computed before thenn the setinverse function changes the value of m in the parent environment to computed value.
makecachematrix=function(x=matrix()){
       m=NULL

       getx=x
       setx=function(y){
              x<<-y
              m<<-NULL
       }
       setinverse=function(inverse){
              m<<-inverse
       }
       getinverse=m
       list(getx=getx,setx=setx,setinverse=setinverse,getinverse=getinverse)
        
        ## This function will provide 4 functions as mentioned in above list.. m is set to null whenever the matrix is not calculated for the first time and will always have a value when computed before.
}       
cachesolve=function(x=matrix()){
      A= makecachematrix(x)
      inverse=A$getinverse
      if(!is.null(inverse)){
             
             return(inverse)
      }
      B=makecachematrix(x)
      matrix=B$getx
      mat=solve(matrix)
      makecachematrix(x)$setinverse(mat)
      mat
}

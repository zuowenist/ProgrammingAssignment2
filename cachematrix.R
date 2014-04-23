## Basically clone from makeVector() and cachemean(),add a bit of warning functions
## that can distinguish two types of uninversiable matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        if(dim(x)[1]!=dim(x)[2]){m<-0}  
        # judge if x is a square matrix.If not,m<-0 then cacheSolve() will output corresponding warning massage. 
        else if(det(x)==0){m<-1}        
        # judge if the det of x is zero.If so,m<-1,then cacheSolve() will output corresponding warning massage.
        else{
                m<-NULL
                set<-function(y){x<<-ym2
                                m<<-NULL}
                get<-function() x
                solve<-solve(x)
                setSolve<-function(solve) m<<-solve
                getSolve<-function() m
                list(set = set, get = get,setSolve = setSolve,getSolve = getSolve)
                }
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##Or retrieve the inverse if it has been calculated.And print warning massage if the inverse matrix isn't exist.

cacheSolve<-function(x,...){
         if(is.list(x)){
                m<-x$getSolve()
                if(!is.null(m)){message("getting cached data")
                        return(m)}
                data<-x$get()
                m<-solve(data,...)
                x$setSolve(m)
                m}
         else if(x==0){message("NOT A Square Matrix,Inverse Matrix isn't exist.")}
         else if(x==1){message("DET is Zero,Inverse Matrix isn't exist.")}
}

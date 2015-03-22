## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## COURSERA
## Johns Hopkins Bloomberg School of Public Health
## R Programming Course (rprog-012)
##
## Student: Alvaro Lemos (alvarolemos@gmail.com)
##
## Programming Assignment #2
## 
## Description: This assignment takes advantage of R scoping rules (lexical) 
##              to avoid time-consuming computations. This is done by caching
##              the value that these computations would return.
##
##              The first function (makeCacheMatrix) uses the mentioned scoping
##              rules to cache the inverse matrix of a matrix that was either
##              passed as an argument to this function, or set by a function
##              defined inside this one. The second function (cacheSolve) gets
##              the cached value and returns it or, in case it's not cached
##              yet, calculates it and returns its value.
##
##              Apologize me if you found my comments rather long. I did it
##              because I found this subject a bit complex and this
##              assignment's description a bit confusing, so I tried to be
##              as clear as possible.
##
##              Thank you very much for taking your time to review my work!! :)
##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

## This function defines and returns a set of functions to set and get a
## matrix and its inverse matrix. Also, its environment is used to cache the
## value of the inverse matrix, avoiding repeated time-consuming computations.
##
## Its usage is: cacheObj <- makeCacheMatrix(someMatrix)
## Then it is used by the 'cacheSolve' function: cacheSolve(cacheObj)
makeCacheMatrix <- function(mat = matrix()) {

        ## The 'inverse' matrix of 'mat' is initially set to NULL
        cachedInverse <- NULL
        
        ## Function that returns the 'mat' matrix. It can be either the
        ## one provided when the 'makeCacheMatrix' function was called
        ## or a new one set with the 'setMatrix' function
        getMatrix <- function() {
                return(mat)
        }
        
        ## Assigns a new matrix 'newMat' to the 'mat' matrix and NULL to its
        ## inverse matrix, 'cachedInverse', because it hasn't been calculated
        ## yet. Since the assingment operator used in both operations is '<<-',
        ## these assingments happens in the 'makeCacheMatrix' function
        ## environment, which is the parent environment of this function
        setMatrix <- function(newMat) {
                mat <<- newMat
                cachedInverse <<- NULL
        }

        ## Returns the 'cachedInverse' matrix of 'mat'. Since there's no formal
        ## or local variable called 'cachedInverse' defined in this functions's
        ## environment, R gets it in its parent environment, which is the
        ## 'makeCacheMatrix' environment
        getInverse <- function() {
                return(cachedInverse)
        }
        
        ## Caches the 'inverse' matrix of 'mat' in the 'cachedInverse' matrix
        ## inside the 'makeCacheMatrix' environment, which is the parent of
        ## this functions's environment, since the '<<-' operator was used
        setInverse <- function(inverse) {
                cachedInverse <<- inverse
        }
        
        ## Returns the four functions defined above
        list(getMatrix = getMatrix,
             setMatrix = setMatrix,
             getInverse = getInverse,
             setInverse = setInverse)
}


## This function's argument, 'cacheObj', is a return of the 'makeCacheMatrix',
## so it's got the four functions defined in it. It's purpose is to either
## cache a inverse from 'cacheObj' environment or, in case there's no inverse
## matrix there yet, it calculates and caches it
cacheSolve <- function(cacheObj, ...) {
        
        ## Gets the inverse of the matrix cached in the 'cacheObj'
        cachedInverse <- cacheObj$getInverse()
        
        ## If the inverse of the matrix is already cached, returns it
        if (!is.null(cachedInverse)) {
                print("Getting cached inverse matrix...")
                return(cachedInverse)
        }
        
        ## In case it's not cached, calculates, caches and returns it
        else {
                localInverse <- solve(cacheObj$getMatrix())
                cacheObj$setInverse(localInverse)
                print("Caching the inverse matrix...")
                return(localInverse)
        }
}
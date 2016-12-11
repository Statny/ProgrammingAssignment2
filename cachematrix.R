## This function creates a square matrix object that can catch its inverse.
## This function return a list containing (set, get, setinverse and getinverse)
## to be computed by the next function named "cacheSolve". 
## No result is attend at this step. To complete the operation, you must use 
## "cacheSolve". The only goal of "makeCacheMatrix" is to be use after into 
## "cacheSolve".

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                ## this <<- sign allow you to call from an other environement
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The function "cacheSolve" apply the inverse of a matrix "x". 
## If the calculs is not know, he will compute it.
## If I call back again "cacheSolve(a)" it take the previous calculation   
## the function will use the cache and re-open it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }       
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

## solve(x) is doing the same thing but don't cache or save anything.
## With a huge matrix, it become very useful to save time


# In TEST

# Step to follow

### 1. first create a matrix that can be inversed , I've looked through wikipedia 
### to find some example of square matrix revers to be sur that the calculs 
### of my function are good.

### 2. Put it in your text file and save file. 
### below see example
## x <-matrix(c(4, 1, 6, 3, 2, 5, 2, 3, 3), nrow = 3, ncol = 3, byrow = TRUE)

### 3. RUN  source
## source('C:/Users/Stephanie/Desktop/DATA SCIENCES/R programming/Coursera-Rcoding/examen sem 2/programming assignment 2.R')

### 4. RUN (NOTE, you can pan all the next run and run it together)
## x <-matrix(c(4, 1, 6, 3, 2, 5, 2, 3, 3), nrow = 3, ncol = 3, byrow = TRUE)
### create x in your global environnement

### 5. RUN
## a <-makeCacheMatrix(x)
### create a in your global environnement

### 6. RUN
## summary(a)
### let you know that all your work is ok, you've got your list of four functions
### Length Class  Mode    
### set        1      -none- function
### get        1      -none- function
### setinverse 1      -none- function
### getinverse 1      -none- function

### 7. RUN
## cacheSolve(a)
### All what you are hoping for ! The inverse, great work, you've got it !
###      [,1] [,2] [,3]
### [1,]  1.8   -3  1.4
### [2,] -0.2    0  0.4
### [3,] -1.0    2 -1.0

### 8. RUN
## x
### See my matrix original
###      [,1] [,2] [,3]
### [1,]    4    1    6
### [2,]    3    2    5
### [3,]    2    3    3


# The major interest of this function

## If i call back again "cacheSolve(a)" the computation as been 
## donne the previous time and don't need to be compute again, the 
## function will use the cache and re-open it.


### 9. RUN
# cacheSolve(a)
# getting cached data 

## Woooh

## you could see at this step the use of the 
## condition "if(!is.null(inv))" /  "message("getting cached data")"
## And the result !                                           

# [,1] [,2] [,3]
# [1,]  1.8   -3  1.4
# [2,] -0.2    0  0.4
# [3,] -1.0    2 -1.0

## I've worked really hard on that only because my first matrix was not invertible

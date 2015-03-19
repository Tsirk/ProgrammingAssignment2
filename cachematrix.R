## Put comments here that give an overall description of what your
## functions do

## the following function cretes a set of functions, to be called from the cacheSolve function and are ment to store, print or cache the solved matrix value in the excecuting environment 

makeCacheMatrix <- function(x = matrix()) {
  print(environment()) # for testing purposes prints the environment of the excecuting environment
  evn <- environment()
  s<-matrix()  #creates a variable for a solved matrix
  set<-function(y){
    x<<-y
    s<<-matrix()
  } # creates function for setting a value for input matrix
  get<-function(){
    print(x)
  } # creates function for priniting the input matrix value
  setsolve<-function(solve){
    s<<-solve(x)
    print(environment()) ##in here for testing purposes, prints the name of the environment of setsolve function. This happens only if there is no cached value and one has to be calculated
    getsolve<-function(){
      print(s)
    } #creates a function for printing the inverse matrix
    getevn<- function() environment()
    list(set=set,get=get,setsolve=setsolve,getsolve=getsolve,getevn=getevn)
    #creates a list of available functions in makeCacheMatrix function
  }
  
  ## The following function calculates the inverse matrix to a given one or retrieves an inverse matrix stored in a variable in the excecuting environemt if there exists one
  
  cacheSolve <- function(x, ...) {
    s<-x$getsolve() #gives s the value from makeCacheMatrix function
    if(!is.na(s[1,1])) #checking whether cached s is empty or not. As "s" it is a matrix it only checkes the first position, otherwise it would run but with a warning
    {
      message("getting cached data")
      return(s)    
    } #prints the solved matrix from cache if it existed
    s<-solve(x$get()) # from here the else part of the function,  calculates the inverse matrix 
    x$setsolve(s)  #caches the value
    s #prints freshly calculated solved matrix
    
    ## Return a matrix that is the inverse of 'x'
  }
  
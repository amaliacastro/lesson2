#Week 1 session 2

#ask: how to tablate in mac?
# check R 3.0.2 is correctly installed

vector <- c(1,2,3,4)
vector
library(raster)
r<-raster(nrow =20, ncol=20)
plot(r)
random <- rnorm(n=20)
random

data(cars)
plot(cars)

library(sp)
data(meuse.grid)
?meuse.grid  #explanation about the dataset. Help to build self-contained variables to start working
#Copied from the help:
data(meuse.grid)
coordinates(meuse.grid) = ~x+y
proj4string(meuse.grid) <- CRS("+init=epsg:28992")
gridded(meuse.grid) = TRUE
spplot(meuse.grid)

##########################################
##### Replace values in a VECTOR by NA ### 
##########################################
vector<-c(1,2,3,4,3,4,6,7)
vector
vector[vector==3] <- NA
vector[vector==4] <- NA
vector
# Replace NA by another value (requires another syntax because NA is not a number)
vector[is.na(vector)] <- 0
vector
#Replace several values at the same time
vector<-c(1,2,3,4,4,5,3,4,6,7)
vector
vector[vector %in% c(3,4)] <- NA  #doesnt look for a specific order of the numbers
vector

##########################################
######## Control flow example  ########### 
##########################################

hello <- function(name){
  out <- paste("Hello", name)
  return(out)
}
hello("sven")
hello(3) #it runs but it makes no sense
hello(r) #try to concatenate a character and a raster gives error

#improve the function so taht it takes only characters
hello <- function(name){
  if(is.character(name)){
    out <- paste("Hello", name)    
  } else if(is.numeric(name)){
    out <- paste("Hello", name)
    warning("The function expected and object of class character but still works")
  } else {  #produce an error message by stoping the code
    stop("The function expected and object of class character")
  }  
  return(out)
}
hello("to myself")
hello(5) #it runs but it makes no sense
hello(r) #try to concatenate a character and a raster gives error



##########################################
########### Error handling  ############## 
##########################################

square <- function(x){
  out <- x*x
  return(out)
}
square(3)
list <- list(1,2,3,4,5,6,7,8,9)
out<-c()
#square(list) # error: we need a for loop for lists

for (i in 1:length(list)){
  out[i] <-square(list[[i]]) 
}
out


out2 <-c()
list2 <- list(1,2,3,4, "wageningen", 5,6,7)
for (i in 1:length(list2)){
  out2[i] <- square(list2[[i]])
}
out2  #it STOPS when it finds a character, it only takes numeric

trysquare <- function(x){
  s <- try(square(x))
  return(s)
}

out2 <- c()
list2 <- list(1,2,3,4, "wageningen", 5,6,7)
for (i in 1:length(list2)){
  out2[i] <-trysquare(list2[[i]]) 
}
out2  # signals error bc only takes numeric but DOESNOT STOP


#############################
###### Version control ######
#############################
# Git is installed but SVN no (no need to do it)
# Create one repository for each R project.


plot(1)

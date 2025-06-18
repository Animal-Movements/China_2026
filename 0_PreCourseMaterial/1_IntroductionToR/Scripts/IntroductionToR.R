## ----eval = F-------------------------------------------------------
## getwd()


## ----eval = F-------------------------------------------------------
## setwd("C:/Documents/R_Practice")
## # Note: There should be no need to set your working directly when working in a .Rproj.  This code is provided here only for reference.


## ----eval=F---------------------------------------------------------
## # Example
## table <- read.csv(file="Data/TheDataIWantToReadIn.csv", header=TRUE) # read a csv table stored in the data folder


## ----Data Types, message = FALSE, warning = FALSE, eval = F---------
## city <- 'Nairobi'
## class(city)
## 
## number <- 3
## class(number)
## 
## Integer <- as.integer(number)
## class(Integer)
## 
## double <- 56.2
## class(double)
## is.numeric(double)
## 
## logical <- 3 > 5
## logical


## ----eval=F---------------------------------------------------------
## city <- "front royal"
## city
## 
## (numbers <- c(1,3,5,12))
## summary(numbers)


## ----eval=F---------------------------------------------------------
## # This is a comment
## 
## # Combining commands using ;
## a <- 3; b <- 6; c <- a+b
## a
## b
## c
## 
## # Multiple our numbers object by 3
## numbers * 3


## ----vectors, eval=F------------------------------------------------
## 1:20
## c(1,2,3,4,5)
## seq(0,100,by=10)
## rep(1:5,5)
## rep("A rolling stone gathers no moss",4)


## ----eval=T---------------------------------------------------------
# Create vectcor
x <- c(1,3,8,21,48,56,4,29,182,5)
x
# Using vector notation, we can extract any number in the sequence.  For instance, x[4] will return the 4th number in the sequence
x[4]


## ----Question1, eval=F, echo=FALSE----------------------------------
## x[5]
## x[1:5]
## x[c(2,4)]
## x[-c(2,8)]


## ----Matrix1--------------------------------------------------------
# Create matrix
test_matrix <- matrix(data = x, nrow = 4, ncol = 5)
test_matrix

# Note, you can assign any name to an object (as below), but it is best practice to choose meaningful names. 
superman <- matrix(data = x, nrow = 4, ncol = 5)


## ----Matrix notation------------------------------------------------
#test_matrix(row,column)
test_matrix[1,5]


## ----Matrix, eval=FALSE, echo=FALSE---------------------------------
## test_matrix[,5]
## test_matrix[4,]
## test_matrix[3, c(2,4)]


## ----Matrix add, eval=TRUE, echo=TRUE-------------------------------
countries <- c("United States", "Pakistan", "Ireland", "China")
cbind(test_matrix,countries)

#Note that I am have not changed/overwritten the contents of test_matrix.  I could, but I'd have to change my code to
#test_matrix <- cbind(test_matrix,countries)

test_dataframe <- data.frame(test_matrix,countries)
test_dataframe

# Have I changed the file type?
class(test_dataframe)

# Rename columns, also see the colnames() function
names(test_dataframe) <- c("Val1", "Val2", "Val3", "Val4", "Val5", "Countries")
test_dataframe


## ----Dataframe, eval=F, echo=T--------------------------------------
## # Use Vector notation or column headings to reference the dataframe
## test_dataframe[3,5]
## test_dataframe[,5]
## test_dataframe$Val5[3]
## test_dataframe$Val5
## test_dataframe[,"Val5"]
## 
## # Summarize dataframe
## nrow(test_dataframe)
## ncol(test_dataframe)
## dim(test_dataframe)


## ----Vector functions, eval=F, echo=T-------------------------------
## sum(x)
## summary(x)
## range(x)


## ----calculations, eval=F-------------------------------------------
## # Additional, subtraction, multiplication, and division
## 4+2
## 6*8
## (842-62)/3
## 
## # Exponentiate
## 2^3
## 
## # Min and Max
## vector_numbers <- c(2, 3, 4, 10)
## max(vector_numbers)
## min(vector_numbers)


## -------------------------------------------------------------------
# What is the maximum value of a vector of values?
3 > max(c(2,3,4,5))


## ----install, eval = F----------------------------------------------
## install.packages("swirl")


## ----install2, message=FALSE, warning = FALSE-----------------------
library(swirl)


## ----install3, eval = F---------------------------------------------
## install_from_swirl("R Programming")


## ----install4, eval=F-----------------------------------------------
## swirl()


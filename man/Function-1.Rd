% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/function1.R
\name{Function-1}
\alias{Function-1}
\alias{Function_1}
\title{Function 1 creates a new data frame}
\usage{
Function_1(dataset)
}
\arguments{
\item{dataset}{}
}
\value{
returns a data frame containing data like names of columns number of unique and NA values and the class of each variable
}
\description{
Function 1 creates a new data frame
}
\examples{
A <- c("J","e","d","H","a","m","m","o","u","d")
B <- c(1,2,3,4,1,2,3,4,4,6)
C <- c(1,2,3,2,1,2,1,2,3,3)
D <- c("a","b","c","c","d","d","e","f","g","f")
E <- c(1,4,5,6,7,4,5,42,8,11)
f <-c(TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,FALSE,FALSE,TRUE)
Example1 <- data.frame(A,B,C,D,E,f)
A1 <- c("g","f","d","d","l","n","z","w","ud","d")
B2 <- c(11,22,33,44,11,22,33,44,44,66)
C3 <- c(1,4,9,4,1,4,1,8,33,1089)
D4 <- c("a","b","c","c","d","d","e","f","g","f")
E5 <- c(FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,TRUE)
f6 <-c(TRUE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE)
Example2 <- data.frame(A1,B2,C3,D4,E5,f6)
examp1 <- Function_1(Example1)
examp1
examp2 <- Function_1(Example2)
examp2
}

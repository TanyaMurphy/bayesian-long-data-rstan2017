##--------------------------------------------------------------##
##                 Getting Started With R                       ##
##                        John Fox                              ##
##   Introduction to the R Statistical Computing Environment    ##
##                 ICPSR, York University                       ##
##                          2017                                ##
##--------------------------------------------------------------##

# Basics: Interacting with the R Interpreter

    # arithmetic operators

2 + 3 # addition
2 - 3 # subtraction
2*3   # multiplication
2/3   # division
2^3   # exponentiation

    # precedence of operators, order of evaluation

4^2 - 3*2
1 - 6 + 4

    # use parentheses to clarify or modify evaluation

(4^2) - (3*2)
4 + 3^2

    # use spaces to clarify

-2--3
-2 - -3

    # calling functions, specifying arguments

log(100) # natural log
log(100, base=10) # log base-10
log10(100) # equivalent
log(100, b=10) # argument abbreviation

args("log") # arguments of the log() function

help("log")    # documentation
?log  # equivalent 
example("log") # execute examples in help page

log(100, 10) # specifying arguments by order

   # creating vectors

c(1, 2, 3, 4)  # c() (combine) function
1:4            # integer sequence operator (equivalent)
4:1            # descending
-1:2           # negative to positive

seq(1, 4)            # seq() (sequence) function, equivalent to 1:4
seq(2, 8, by=2)      # specify interval between elements
seq(0, 1, by=0.1)    # non-integer sequence
seq(0, 1, length=11) # specify number of elements
args(seq.default)    # seq also has date methods

    # arithmetic on vectors

c(1, 2, 3, 4)/2
c(1, 2, 3, 4)/c(4, 3, 2, 1)
log(c(0.1, 1, 10, 100), base=10)

    # recycling vectors

c(1, 2, 3, 4) + c(4, 3)    # no warning
c(1, 2, 3, 4) + c(4, 3, 2) # warning

    # creating variables (named objects) by assignment

x <- c(1, 2, 3, 4) # assignment
x # print


x/2            # equivalent to c(1, 2, 3, 4)/2
(y <- sqrt(x)) # parentheses to assign and print trick
plot(x, y)

    # assignment is destructive

(x <- rnorm(100))  # 100 standard normal random numbers

summary(x)  # summary(): a generic function

    # character data

(words <- c("To", "be", "or", "not", "to", "be"))
paste(words, collapse=" ")
table(words)

    # logical data

(logical.values <- c(TRUE, TRUE, FALSE, TRUE))
!logical.values # negation

    # coercion

sum(logical.values)      # number of TRUEs (coercion to numeric)
sum(!logical.values)     # number of FALSEs (TRUE-> 1, FALSE -> 0)
c("A", FALSE, 3.0)       # coerced to character
c(10, FALSE, -6.5, TRUE) # coerced to numeric

    # simple indexing

x[12]             # 12th element
words[2]          # second element
logical.values[3] # third element
x[6:15]           # elements 6 through 15
x[c(1, 3, 5)]     # 1st, 3rd, 5th elements 

x[-(11:100)] # omit elements 11 through 100

v <- 1:4
v[c(TRUE, FALSE, FALSE, TRUE)] # logical indexing

    # comparison and logical  operators

1 == 2       # equal to
1 != 2       # not equal to
1 <= 2       # less than or equal to
1 < 1:3      # less than   
3:1 > 1:3    # greater than
3:1 >= 1:3   # greater than or equal to

TRUE & c(TRUE, FALSE)                        # logical and
c(TRUE, FALSE, FALSE) | c(TRUE, TRUE, FALSE) # logical or
TRUE && FALSE  # unvectorized and (for programming)
TRUE || FALSE  # unvectorized or

    # examples

(z <- x[1:10])      # first 10 elements of x
z < -0.5            # is each element less than -0.5?
z > 0.5             # is each element greater than 0.5
z < -0.5 | z > 0.5  #  < and > are of higher precedence than |
abs(z) > 0.5        # absolute value, equivalent to last expression
z[abs(z) > 0.5]     # values of z for which |z| > 0.5

    # user-defined functions

mean(x)  # of 100 random-normal numbers

sum(x)/length(x)  # equivalent


myMean <- function(x){  # defining a function
    sum(x)/length(x)
}

myMean(x) # of 100 random normal number
y         # from sqrt(c(1, 2, 3, 4))
myMean(y)
myMean(1:100)
myMean(sqrt(1:100))

mySD <- function(x){  # another user-defined function
    sqrt(sum((x - myMean(x))^2)/(length(x) - 1))
}

mySD(1:100)
sd(1:100) # check

mySD
myMean

letters
mySD(letters)
traceback()  # call stack to point of error

# An Illustrative Data Analysis: Duncan's Occupational Prestige Regression

library("car")      # load car package (for data and functions)
head(Duncan, n=10)  # first 10 cases
dim(Duncan)         # rows and columns
View(Duncan)        # in the RStudio data viewer
summary(Duncan)     # invoking the summary() generic function
help("Duncan")      # codebook for the data set

    # Examining the Data

with(Duncan, hist(prestige))

scatterplotMatrix( ~ prestige + education + income, 
    id.n=3, data=Duncan)

    # Duncan's regression

(duncan.model <- lm(prestige ~ education + income, data=Duncan))

summary(duncan.model)  # more detailed report

    # some regression diagnostics

        # distribution of the (studentized) residuals

densityPlot(rstudent(duncan.model))
qqPlot(duncan.model, id.n=3)
outlierTest(duncan.model)

        # influence diagnostics

influencePlot(duncan.model, id.n=3)

influenceIndexPlot(duncan.model, id.n=3)

avPlots(duncan.model, id.n=3, id.method="mahal")

        # nonlinearity diagnostic

crPlots(duncan.model)

        # nonconstant-spread diagnostics

spreadLevelPlot(duncan.model)
ncvTest(duncan.model)
ncvTest(duncan.model, var.formula= ~ income + education)

    # refit without ministers and conductors

whichNames(c("minister", "conductor"), Duncan)
duncan.model.2 <- update(duncan.model, subset=-c(6, 16))
summary(duncan.model.2)
compareCoefs(duncan.model, duncan.model.2)

##--------------------------------------------------------------##
##          Reading and Manipulating Data in R                  ##
##                        John Fox                              ##
##   Introduction to the R Statistical Computing Environment    ##
##                 ICPSR, York University                       ##
##                          2017                                ##
##--------------------------------------------------------------##

# Data in R packages

library("car")
head(Duncan)  # from car, via "lazy data"
class(Duncan)
str(Duncan)   # internal structure of object
objects()     # Duncan not in workspace

data("Animals", package="MASS")  
search()   # MASS package not loaded
objects()  # Animals in workspace
head(Animals)
str(Animals)
    
    # use of data() may be necessary for a loaded package
    # if it doesn't support lazy data

# Entering data at the keyboard

cooperation <- c(49, 64, 37, 52, 68, 54, 61, 79, 64, 29,
                 27, 58, 52, 41, 30, 40, 39, 44, 34, 44)

cooperation <- scan() # equivalent, alternative
49 64 37 52 68 54 61
79 64 29
27 58 52 41 30
40 39 44 34 44
 
    # must enter empty line to end scan()

    # generating patterned data

(condition <- rep(c("public", "anonymous"), c(10, 10)))
(sex <- rep(rep(c("male", "female"), each=5), 2))
str(sex)

rep(5, 3)
rep(c(1, 2, 3), 2)
rep(1:3, 3:1)

    # defining a data frame (data set)

(Guyer <- data.frame(cooperation, condition, sex))

Guyer <- data.frame(  # equivalent
    cooperation = c(49, 64, 37, 52, 68, 54, 61, 79, 64, 29, 
                    27, 58, 52, 41, 30, 40, 39, 44, 34, 44),
    condition = rep(c("public", "anonymous"), c(10, 10)),
    sex = rep(rep(c("male", "female"), each=5), 2)
)
str(Guyer)
Guyer$sex
str(Guyer$sex)

# Reading plain-text data from a file

    # whitespace-separated values: Duncan.txt

Duncan <- read.table("Duncan.txt", header=TRUE)  # Duncan in current directory
getwd()  # current directory, setwd() to change
View(Duncan)

file.choose()  # returns character string with path to file
(Duncan <- read.table(file.choose(), header=TRUE))  # equivalent

    # reading data from a URL

Duncan <- read.table(
    "http://socserv.mcmaster.ca/jfox/Courses/R/York-R-course//Duncan.txt",
    header=TRUE)
head(Duncan)
    
    # comma-separated values: Duncan.csv

Duncan <- read.csv("Duncan.csv")
str(Duncan)  # note, occupation is a factor
rownames(Duncan) <- Duncan$occupation
Duncan$occupation <- NULL  # remove variable
head(Duncan)

# Reading data from a spreadsheet

library("rio")

Duncan <- import("Duncan.xlsx")
str(Duncan)
rownames(Duncan) <- Duncan$occupation
Duncan$occupation <- NULL
Duncan$type <- factor(Duncan$type, levels=c("bc", "wc", "prof"))
str(Duncan)

    # rio also supports importing data from SPSS, SAS, Stata, etc.
    # see help(package="rio")

# Working with data in data frames

    # indexing variables

Duncan$prestige
mean(Duncan$prestige)
mean(Duncan[ , "prestige"]) # equivalent, column by name
mean(Duncan[ , 4]) # equivalent, column by number

    # using with()

with(Duncan, mean(prestige, trim=0.1))

    # using the data argument, when available

(mod.duncan <- lm(prestige ~ income + education, data=Duncan))

    # it's generally a bad idea to attach a data set to the search path

attach(Duncan)  # don't do this!
search()
prestige  # found in Duncan
detach("Duncan")
search()
prestige  # fails!

# Handling missing data

head(Freedman)
nrow(Freedman)
head(Freedman$density, 20)  # first 20 values
sum(is.na(Freedman$density))

    # applying an anonymous function over columns (coordinate 2)

apply(Freedman, 2, function(x) sum(is.na(x)))

    # NAs propagate simply for elementwise 
    # arithmetic operators and functions

head(Freedman$density^0.5, 20)
head(log10(Freedman$density), 20)
head(with(Freedman, population*nonwhite/100), 20)

    # using the na.rm argument for simple statistics

median(Freedman$density)
median(Freedman$density, na.rm=TRUE)

        # mean(), sd(), quantile(), etc., are similar

    # plotting and modeling functions typically ignore cases with NAs

with(Freedman, {
    plot(density, crime)
    showLabels(density, crime, row.names(Freedman), 
               id.n=5, id.method="x") # from car package
})

plot(crime ~ log10(density), data=Freedman)
abline(lm(crime ~ log10(density), data=Freedman))
plot(residuals(lm(crime ~ log10(density), data=Freedman)))
abline(h = 0, col = "red")

# Transforming and recoding variables

(percent.coop <- 100*Guyer$cooperation/120)
remove("percent.coop") # from the workspace

    # define new variable in a data set

Guyer$percent.coop <- 100*Guyer$cooperation/120  
head(Guyer)

    # modify existing variable

Guyer$cooperation <- with(Guyer, 
        log(percent.coop/(100 - percent.coop)))
head(Guyer)

remove("Guyer")

head(Guyer) # from the car package

Guyer <- within(Guyer, {  # equivalent, alternative
    percent.coop <- 100*Guyer$cooperation/120 
    cooperation <- log(percent.coop/(100 - percent.coop))
})
head(Guyer)

    # using cut()

Guyer$coop.4 <- cut(Guyer$percent.coop, 4)  # 4 equal-width bins
summary(Guyer$coop.4)

Guyer$coop.fourths <- with(Guyer,
    cut(percent.coop,  # at the quartiles
        quantile(percent.coop, c(0, 1/4, 1/2, 3/4, 1)),
        include.lowest=TRUE,
        labels=c("low", "low.med", "high.med", "high")))
summary(Guyer$coop.fourths)

    # using recode() from the car package; see ?recode
    # notice single quotes around lo:50... expression
(Guyer$coop.2 <- recode(Guyer$percent.coop, 
            ' lo:50="low"; 50:hi="high" ', as.factor=TRUE))
xtabs(~ coop.2, data=Guyer)  # more general than summary()

# Using "Pipes"

library("magrittr")  # provides pipe operator 
                     # (Rene Magritte: "Ceci n'est pas une pipe")

x <- rnorm(10)

    # direct function call

sort(x)  
sort(x, decreasing=TRUE)

    # piping into a function

x %>% sort()  
x %>% sort(decreasing=TRUE)

    # piping into an argument other than the first

TRUE %>% sort(x, decreasing=.)
TRUE %>% sort(x, .)  # equivalent


    # A problem from r-help

(concept_df <- data.frame(concept=c("butan acid ", 
    "nano diamond particl", "slurri composit", 
    "composit ph polis", " inorgan particl ",  
    "grind liquid", "liquid formul", "nanoparticl", 
    "size abras particl", "agent malic acid")))

(chemical_df <- data.frame(chemical=
    c("basic", "alkalin", "alkali", "acid",  " ph ", "hss")))

    # Problem: Find names in concept in the first 
    #          data frame that match names in
    #          chemical in the second data frame; 
    #          label the corresponding rows
    #          in the first data frame as "chemical".

        # solution using direct function calls

            # a "regular expression": 
            #  \\b = a word boundary  | = or

(match_string <- paste0("\\b", 
    paste(chemical_df$chemical, collapse="\\b|\\b"),
    "\\b"))  

            # test:

grepl(match_string, 
    c("butan acid ", "nano diamond particl", "butanacid "))

concept_df$category <- ifelse(
    grepl(match_string, concept_df$concept), 
    "chemical", "")
concept_df

        # solution using pipes (due to Georges Monette)

(chemical_df$chemical %>%
    paste(collapse = '\\b|\\b') %>%
    paste0('\\b', . ,'\\b') ->  # note forward assign
    match_string)

concept_df$concept %>%
    grepl(match_string, .) %>%
    ifelse('chemical', '') ->
    concept_df$category
concept_df

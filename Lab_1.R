#' ---
#' title:      "Lab 1: Mixed Models"
#' subtitle:   "ICPSR Summer Program at York University"
#' date:       "July 17 to 21, 2017"
#' author:     "Georges Monette"
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: false
#'     theme: readable
#'     fig.width: 12
#'     fig.height: 10
#' bibliography: mm.bib
#' link-citations: yes
#' ---
#' (Updated: `r format(Sys.time(), '%B %d %Y %H:%M')`)
#' 
#+ include=FALSE
knitr::opts_chunk$set(comment="  ", error = TRUE, cache = TRUE)
if(.Platform$OS.type == 'windows') windowsFonts(Arial=windowsFont("TT Arial")) 
interactive <- FALSE  # do not run interactive code
#'

##'
##' # Mixed Models Exercises Lab 1 ----
##'

##'
##' ## Setup ----
##'
#'
#' To install packages visit John Fox's 
#' http://socserv.mcmaster.ca/jfox/Courses/R/York-R-course
#' 
#' Update spida2 frequently with:

# devtools::install_github('gmonette/spida2')

library(spida2)  
library(p3d)
library(car)
library(nlme) 
library(lattice)
library(latticeExtra)

##' 
##' #  Introduction to Lab 1 ----
##'   

#' 
#' At first, it's easier to learn by following a textbook example where nothing 
#' goes wrong. However, when you analyze real data lots of things tend to go 
#' wrong -- or, more positively, many interesting things happen -- and you wish 
#' you had experience with more realistic examples. These examples try to do 
#' both.
#' 
#' The first example is a gentle analysis in which we use the basic ideas of 
#' multilevel models with mixed models.  We learn how to formulate and test 
#' general linear hypotheses that allow us to ask a much wider range of 
#' questions than those accessible through standard output.
#' 
#' The second example, using a different model with the same data, illustrates 
#' what to do when 'everything' goes wrong, oops, I meant "when a lot of 
#' interesting things happen."
#' 
#' In the second example, the first model does not converge and we have to 
#' figure out what to do. We need to test random effects variance parameters 
#' that are not tested validly with the usual methods and we learn how to use 
#' simulation for this purpose.
#' 
#' Our analysis of BLUPS reveals problems with the model whose correction we 
#' consider.
# 
#' The second sample analysis is quite long but it is amply annotated
#' so that you can follow much of it on your own.

##'
##' ##  Data: Math Achievement and SES ----
##'

#'
#'  Math Achievement and SES in a sample of Public and Catholic high schools
#'
#'  Goal: Study the relationship between SES and Mathach in Public vs
#'  Catholic schools
#'

##'
##' ### Exploring data ----  
##'

?hsfull
dim( hsfull )
some( hsfull )   # a random selection of rows

#' A 'uniform quantile' plot. Data is lined up from shortest to tallest.

xqplot( hsfull )  # gives an idea of distribution, outliers, granularity, etc. at a glance

#' A 'normal quantile' plot. Data is lined up from shortest to tallest, x-axis
#' stretched to have shape of ideal normal.  If a variable is close to normal,
#' its quantile plot will be close to a diagonal

xqplot( hsfull , ptype='normal') # normal quantiles on x-axis

##'
##' ## Data: Levels and structure ----
##'

#'
#' Data structures, part 1: moving up one level
#'
#' 'hsfull' has one student per row.
#' 
#' It is a **long** file combining variables of all levels. If we want
#' information on schools, we can create a data set that has one row per school,
#' keeping only those variable that are invariant within schools, i.e. Level 2
#' variables

hsup <- up( hsfull, ~ school )  # one row per school with Level 2 variables 
some( hsup )
dim( hsup )
#'  
#'  gicc: levels of variables in hsfull
#'  
#'  Generalized intraclass correlation:
#'  
#'  -  intraclass correlation (variance between / total variance) or ranks for 
#'  numerical variables
#'
#'  -  Goodman-Kruskal's tau for factor and character variables
#'  
#'  Interpretation:
#'
#'  -  1 means it's a level-2 or school-level variable, 
#'  -  0 means that its mean is the same for each school, e.g. if it's balanced
#'       (really just the similarily of the means, not the distribution/spread)
#'  
gicc( hsfull, ~ school )
tab_( hsfull, ~ school)         # school size: 'tab_' is a variant of 'tab' with no totals
tab_( hsfull, ~ school ) %>% c %>% tab # distribution of school sizes
tab_( hsfull, ~ school ) %>% c %>% tab_ %>% plot

# c within the pipe is equivalent to this:
# x <- tab_( hsfull, ~ school) 
# x2 <- c(x)
# tab(x)
# tab(x2)

#' Level-2 variables
hsup <- up( hsfull, ~ school)
xqplot( hsup )

#' To speed up calculations, we will use a (pre-)randomly selected quarter of the
#' schools.
#' 
#' However, since it's a good idea to know that it's easy to select a random
#' subset of schools, __not students__, here's how it's done.
#' 
##' 
##' # Selecting a random subset of clusters ----
##' 
#' 
#' We can't randomly sample from the long file if we want each cluster
#' (school) to be either all in or all out.
#' 
#' There are a few ways of selecting all the observations from a sample of
#' clusters. The following seems fairly intuitive.
#' 
#' We first create the school level file and take a sample of school from that
#' file. We then merge the sample file with the longfile. Merge will match with
#' variables that have the same name. By default, it only uses records that
#' match in both files, so it produces the result we want.


hsup <- up ( hsfull, ~ school)  # as above
dim(hsup)                       # one row per school

#'
#' For speed we'll use a randomly selected subset of half the schools
#'
#' This has another interesting consequence: we can do a split-half
#' analysis in which the model developed on one half of the clusters
#' is validated with the other half
#' 
#' The following code shows how to split the clusters into two halves
#' although we'll be using two 'preselected' halves
#'
selected <- sample( 1:nrow( hsup ) , round( nrow( hsup )/2 )) # row indices
selected
# for half the schools
hsu1 <- hsup[ selected, ]      # first half of schools
hsu2 <- hsup[ - selected, ]    # other half

hs1 <- merge( hsu1, hsfull )    # long data for 1st half of schools
hs2 <- merge( hsu2, hsfull )    # long data for other half

dim(hs1)
dim(hs2)

length(unique(hs1$school))
length(unique(hs2$school))

#'
#' But we will use a smaller subset of 40 schools
#'
data(hs)
dd <- hs              # easier to type repeatedly

dim( dd )
some(dd)

##' 
##' # First look at variables ----
##'

summary( dd )

# Better -- same info +

xqplot( dd )       # 1 dim summary

#'
#' Think of a classroom photo with kids lined up from shortest to tallest
#'
#' Solid line at mean, dashed lines at +/- one standard deviation
#'
#' Find quantiles by selecting a proportion on x-axis and read of
#'     height of graph on y-axis (use the edge of sheet of paper)
#'     e.g. .8-quantile = 80th percentile of ses is ~ 0.8
#'
#' Note: with an ~normal distribution we expect mean -/+ std. dev.
#'     to be at approx the 32nd and 68th percentiles.
#'
library(Hmisc, pos = 1000) # to avoid masking functions in other packages
describe(dd)
#'
#' QUESTIONS: looking at univariate distributions for this data:
#'
#' Any problems with:
#'         NAs?
#'         Highly skewed distributions?
#'         Possible univariate outliers?
#'         Skewed factors: very rare category
#'
#' Any actions to take?
#'

library(car)
# The following is commented out to save time:
# scatterplotMatrix( dd , ellipse = TRUE)   # 2-dim summary

#'
#' QUESTIONS: looking at bivariate relationships
#' 
#'     Hint: focus on one column or one row at a time
#'
#'     Bivariate relationships that stand out?
#'     Bivariate outliers?
#'     Curvilinear bivariate relationships?
#'     Which pair has the 'strongest' bivariate relationship?
#'

##'
##' ## Looking at Level 2 variables (invariant within schools) ----
##'
ddu <- up( dd, ~ school)  # only variables invariant within schools
dim(ddu)
some(ddu)

xqplot( ddu )    # shows just variables invariant within schools
scatterplotMatrix( ddu , ell = T)   # between school variables
#'
#' Include derived Level 2 summaries of Level 1 variables
#'
#' - Level 2 means of Level 1 numerical variables
#' - Level 2 **modes** of Level 1 categorical variables (not very useful)
#'
ddall <- up (dd, ~school, all = T)   # Note categorical variable - get modal value
dim( ddall )
#'
#' To get distribution of proportions for factors -- not just modes
some(
  model.matrix( ~ Sex + Minority -1, dd)
)

# Adding Indicator matrices for factors

ddagg <- up(dd, ~school, agg = ~ Minority + Sex)
head(ddagg)

xqplot( ddagg )
spm( ddagg, ell = T  )

#' 
#' QUESTION:
#' 
#' -  What is the link between the barchart for Minority and proportion of
#'    'MinorityYes' 
#' -  Note that there is a relationship between minority and ses as
#'    well as between minority and mathach
#' 
#'
#' QUESTION:
#'    What to look for: What stands out in 'between-school' relationships?
#'    Classify variables:
#'         - Level 1 variables
#'         - Level 2 variables: natural (properties of cluster)
#'                and derived [or 'contextual'] (properties of the sample).
#'    Does this suggest any multilevel questions?
#'    Are there derived level 2 (contextual) variables that could be relevant?
#'

##'
##' ## Creating additional Level 2 ( and Level 1 ) variables with 'capply' ----
##'
#' 
#' An important part of modeling is being able to easily create the variables 
#' that capture the phenomena you want to study. Often, these variables are
#' not in the raw data set.
#' 
#' Examples:
#' 
#'  Sample size:
#'  
#'  capply(y, cluster, fun)  
#'
#'     1. divides 'y' into chunks for each value of 'cluster'
#'
#'     2. applies the function 'fun' to each chunk
#'
#'     3a. if 'fun' returns a vector of the same length as the chunk of 'y'
#'         then 'capply' returns that vector in the positions 
#'         corresponding to the values of 'y'
#'    
#'     3b. if 'fun' returns a single value, that value gets returned in
#'         every position of 'y'
#'         
#'
#' How many observations within each school?
#' 
dd$n <- capply( dd$school, dd$school, length)
head( dd )  # to see that new variable is constant in first school
some( dd )
dd %>% {up(.,~school)$n}  %>% tab
dd %>% {up(.,~school)$n}  %>% tab_ %>% plot
#'
#'  mean ses by school (sample contextual ses)
#'
dd$ses.m <- with( dd, capply(ses, school, mean, na.rm = TRUE ))
head( dd )

#'
#' ses centered within school (CWG = centered with groups)
#' This is the deviation of a student from the mean of the 
#' sample in her school
#' 

dd$ses.cwg <- dd$ses - dd$ses.m
head( dd )
layer <- latticeExtra::layer
xyplot(ses.cwg ~ ses.m, dd) + layer(panel.dell(...,col='red',lwd=3))
#'
#' Note that ses.cwg and ses.m are uncorrelated

#'
#' ses heterogeneity as measured by the IQR within schools
#'
dd$ses.iqr <- capply( dd$ses, dd$school,
                      function ( x ) {
                        qs <- quantile(x, c(25,75)/100)
                        qs[2] - qs[1]
                      }
)

#' Note: There is an "IQR" function to compute the inter-quartile range but this
#' does it from scratch to illustrate the use of an 'anonymous' function, i.e. a
#' function defined on the fly.
#' 
#' We could have done:

dd$ses.iqr <- capply(dd$ses, dd$school, IQR, na.rm = FALSE)

#' but we could modify the anomymous function it to 

dd$ses.trange <- capply( dd$ses, dd$school,  # where 'trange' stands for
                                             # 'truncated range'
                         function ( x ) {
                           qs <- quantile(x, c(10,90)/100, na.rm = FALSE)
                           qs[2] - qs[1]
                         }
)

#' consider variations like

dd$ses.sd <- capply( dd$ses, dd$school, sd, na.rm = FALSE)

some( dd )
head( dd )

##'
##' ## Tranformations of Level 1 variables within groups ----
##' 
#' capply will also create a Level 1 variable if the FUN returns a a vector of
#' length greater than 1. The vector gets recycled to match the length of the
#' argument, i.e. the number of the rows corresponding to a particular id.
#' 
#' The following example shows the calculation of ses rank within schools:
#' 
dd$ses.rank <- capply( dd$ses, dd$school, rank , na.last = 'keep' )
some( dd )
head( dd )

#' Using the rank instead of the raw value
#' could be useful for highly skewed variables where you don't
#' want extreme values to have too much influence
#'
#' The following example shows the use of 'capply' to compute a value
#' that depends on more than one variable in the data frame. If the first
#' argument is a data frame, then 'capply' splits the data frame into
#' data frames with just the rows belonging to each id. Using the 'with' function
#' on these data frames allows you to write an expression as the
#' fourth argument which becomes the second argument of with.
#'
#' ses discrepancy of Minority in school (requires more than one variable)

dd$minority.diff <- capply( dd, ~ school, with,
                            mean( ses[ Minority == "Yes" ] , na.rm = T) -
                              mean( ses[ Minority == "No" ] , na.rm = T) )
#'
#' Beware: this way of using 'capply' can be slow with very large files
#' like the NPHS long file.
#'
some( dd )
head( dd )
#'
#'   
##'
##' # Looking at data in 3D ----
##' 
#' Visualizing the school level data again

xqplot( up( dd, ~ school) )
# spm( up( dd, ~ school ))

#' QUESTIONS:
#'
#'   1) Would sample size be a reasonable proxy for school size if we
#'        did not have school size?
#'   2) Can you think of other ways of summarizing or visualizing the data:
#'        what do you see?
#'
#'
#'  1d - 2d -> 3d
#'
#' We can see 3 continuous variable + 1 categorical variable (really 4d)

library( p3d )
Init3d(family = 'serif', cex = 1.2)
dda <- up(dd, ~ school, ~ mathach + ses + Sex)
head(dda)
Plot3d ( mathach ~ ses + Sex_Female | Sector, dda)
fg()
#' Note gender composition of Catholic schools
#' 
# just for fun
fit <- lm( mathach ~ ses * Sex_Female * Sector + 
             I(ses*(Sector == 'Catholic')*Sex_Female^2), dda)
summary(fit)
Fit3d ( fit )
if(interactive) Id3d(pad=2)
#'
#' EXERCISE:
#'
#' Try other possibilities. Tell us what you find!
#'
##'
##' ## Looking at Level 1 and Level 2 data using Lattice graphics ----
##' 
#'
#'  Preparing for multilevel modelling:
#'  
#'        Visualizing data at Level 1 and Level 2
#'
#'
#' From the mixed model to the hierarchical models:
#'
#'  With software like HLM, MlWin, you need to use separate data sets for the
#'  different levels. With nlme and SAS PROC MIXED, we use combined long data
#'  set with all the variables together.
#'
#'  We first need to separate the Levels of the model.
#'
#'
#'   1) Level 1 model:
#'
#'      mathach ~ 1 + ses | school
#'
#'   2) Level 2 model:
#'
#'           Using coefficients of Level 1 Model:
#'                 B.0   ~ 1 + Sector
#'                 B.ses ~ 1 + Sector
#'
#'           Combining with Level 1:
#'
#'              ~     1 * ( 1 + Sector )
#'                 + ses * ( 1 + Sector )
#'
#'
#'   3) Mixed model: Simplify combined model
#'
#'      mathach ~ 1 + ses + Sector + ses:Sector
#'
#'      OR (using Rs rules for generating marginal effects)
#'
#'      mathach ~ ses * Sector
#'
#'
#'  4) Random effects model:
#'
#'          ~ 1 + ses | id     # usually contained in Level 1 model
#'
#'          Often, a simpler model is used, e.g. ~ 1 | id
#'
#'   5) Contextual variable:
#'
#'      Mean ses | school  i.e cvar( ses , school)
#'
#'      Mixed model with contextual variable:
#'
#'      mathach ~ 1 + ses + Sector + ses:Sector + cvar( ses, school) + ...
#'

xyplot( mathach ~ ses | school,  dd )

# Make an informative label and ordering for schools

dd$id <- factor( paste( substring( dd$Sector, 1,1), dd$school, sep =''))
some( dd )

# ordering used for panels in xyplot:

dd$id <- reorder( dd$id, dd$ses)    # or

dd$id.sec <- reorder( dd$id, dd$ses + 1000* (dd$Sector == "Catholic"))


#' Note: If you are irritated by caps in variable names use:
#' 
#'             names(dd) <- tolower( names(dd) )
#' 
#' Make sure this won't create duplicates. Variable names are case sensitive in
#' 

gd()    # sets 'nice' defaults

xyplot( mathach ~ ses | id, dd,  layout = c(9,9),
        panel = function( x,y ,..., type) {
          panel.xyplot( x, y, ... )
          panel.lines( dell( x, y, radius = 1:2), type = 'l',...)
          panel.lmline( x, y, ...)
          panel.lines( lowess( x, y),..., type = 'l')
        })

#' If you like a particular panel function, you can make it yours:
#' R is easy to customize:

mypanel <- function( x,y ,..., type) {
  panel.xyplot( x, y, ... )
  panel.lines( dell( x, y, radius = 1:2), type = 'l',...)
  panel.lmline( x, y, ...)
  panel.lines( lowess( x, y),..., type = 'l')
}

#' Use panels and groups to see data in different ways:

xyplot( mathach ~ ses | Sector, dd,   groups = id,
        panel = panel.superpose,
        panel.groups = mypanel)
#' too much!

#' Just data and fitted line:
mypanel2 <- function( x,y ,..., type, alpha) {
  panel.xyplot( x, y, ... ,alpha = .3)
  panel.lmline( x, y, ..., alpha = 1)
}

xyplot( mathach ~ ses | Sector, dd,   groups = id,
        panel = panel.superpose,
        panel.groups = mypanel2)
#' fitted lines generally higher and flatter in Catholic sector

xyplot( mathach ~ ses | Sector * cut(ses.m, 5), dd, groups = id,
        panel = panel.superpose,
        panel.groups = mypanel2) %>% 
  useOuterStrips
#'
#' Note:
#' 
#' - fitted lines generally higher and flatter in Catholic sector
#'
#' Center point and fitted lines

mypanel3 <- function( x,y ,..., type) {
  panel.xyplot( mean(x), mean(y), ... )
  panel.lmline( x, y, ...)
}

xyplot( mathach ~ ses | Sector * cut( ses.m, 5), dd,   groups = id,
        panel = panel.superpose,
        panel.groups = mypanel3) %>% 
  useOuterStrips

##'
##' ## Visualizing fitted lines in beta space ----
##'

#' Fit an OLS model in each school

# fit.list <- lmList( mathach ~ ses | id, dd)

# may produce an error due to missing values in some variables in 'dd' that
# 'lmList' does not use. To work around this problem, you can use:

fit.list <- lmList( mathach ~ ses | id, dd[,c('mathach','ses','id')])
fit.list
levels( dd$id )

beta <- expand.grid( id = levels( dd$id ))
beta <- cbind( beta, coef( fit.list))
some ( beta )
beta <- merge( beta, up(dd, ~ id))

xyplot( `(Intercept)` ~ ses | Sector * cut( ses.m, 3), beta,  groups = Sector,
        panel = function(x, y, ..., type){
          panel.xyplot( x,y , ...)
          if ( length(x) > 3)panel.lines( dell( x, y), ..., type = 'l')
        },
        main = 'beta space'
)

##'
##' ### Looking at between group effect ----
##'

xyplot( mathach ~ ses | id, dd,  layout = c(9,9),  groups = Sector,
        panel = mypanel, auto.key = T)

dda <- up(dd, ~ id, all = T)
some(dda)

xyplot( mathach ~ ses, dda ,   groups = Sector,
        panel = mypanel, auto.key = T)

#'
#' Seeing both together:
#'

dda$id <- 'summary'
gd(pch='.', cex =2)
xyplot( mathach ~ ses | id, rbind(dd,dda),  layout = c(9,9),  
        groups = Sector,
        panel = mypanel, auto.key = T)
#'
#' EXERCISE:
#' 
#'    Try groups = Sex or Minority and try '| id.sed
#'    Do these plots reveal anything useful?
#'
#'
##'
##' ## Fitting a mixed model ----
##' 
#'
#'   Question to investigate:
#'
#'         mathach ~ ses  in differenct Sectors
#'
#'   Level 1 model:
#'
#'        mathach ~ 1 + ses
#'
#'   Level 1 + 2:
#'
#'         mathach ~ 1 + ses + Sector + Sector:ses
#'
#'   Full random model:
#'
#'         ~ 1 + ses | id
#'
dd$id <- dd$school
fit <- lme( mathach ~ ses * Sector, dd, random = ~ 1 + ses | id)

#' If this fails to converge, increase the number of iteration 

##'
##' ## Convergence problems ----
##' 

fit <- lme( mathach ~ ses * Sector, dd, random = ~ 1 + ses | id,
            control = list(msMaxIter=200, msVerbose=T))

summary( fit )


wald(fit, 'ses')      # overall test for 'ses'
wald(fit, 'Sector')   # overall test for 'Sector'  

#' Note that this model tacitly assumes that the 
#' between-school effect of ses is the same as the
#' within-school effect of ses
#'
##'  
##' ### In passing: handling NAs ----
##'
#' This is a big topic but note that deleting rows with NAs
#' does not delete entire clusters, only
#' units within clusters. This is appropriate under a wider
#' range of assumptions than deleting clusters -- as one
#' would have to do with a repeated measures analysis.
#' More on this in Lab 4.R

#' Syntax

fit <- lme( mathach ~ ses * Sector, dd, random = ~ 1 + ses | id,
            na.action = na.exclude,
            control = list(msMaxIter=200, msVerbose=T))

#'
#' Note: generally 'na.exclude' is preferable to 'na.omit'
#' It allows residuals and predicted values to match the original
#' data frame, not just the observations that were not missing.
#'
#'  In passing: What to do if the model does not converge
#'

#  We will revisit this when we need it:
#
#  Summary:
#
#  1) If Iteration limit reached: Increase iterations.
#
#     ?lmeControl
#     lmeControl
#
#     fit <- lme( mathach ~ ses * Sector, dd, random = ~ 1 + ses | id,
#         control = list( maxIter = 200, msMaxIter = 200, niterEM = 100,
#                   msVerbose = TRUE , returnObject = TRUE ))
#     summary( fit )
#
#  2) If singular convergence, the frequent cause is that the
#     random model is more complex than necessary. I.e. the variability
#     from cluster to cluster that you are trying to account for with
#     the random part of the model is actually accounted for by the
#     'epsilons', the within-cluster variability. Consequently a
#     variance is estimated to be 0 or the G matris is non-singular,
#     i.e. it's a flat pancake.
#
#     It could be a flat like a pancake because of scaling OR centering.
#
#     Try:
#     
#     1) Global center x in RE model (not necessarily at the mean but
#        that's a good first try. The 'ideal' place is the point of
#        minimum variance of the response. Plot OLS lines to get an idea.
#
#     2) Also try centering within clusters (CWC or CWG). The RE model
#        is not equivalent but it could often be better depending on
#        the process. Use AIC to compare.,
#
#     3) If neither works, start with simple RE model and add components
#        using a forward stepwise approach. Use 'anova' and 'simulate'
#        to test additional components.  If a model does not converge,
#        use
#            ..., control = (...., returnObject = TRUE, msVerbose = TRUE)
#        to judge whether the likelihood ratio converges even though the
#        parameters don't. In this case, anova can give a usable p-value
#        that can be adjusted with 'simulate'.
#
#     fit.big <- lme( ...., control = list( maxIter = 200,
#                   msMaxIter = 200, niterEM = 100,
#                   msVerbose = TRUE , returnObject = TRUE ))
#     anova ( fit.simple, fit.big )
#     zsim <- simulate( fit.simple, nsim = 1000, m2 = fit.big)
#     plot( zsim )
#
#        to test whether 'fit.simple' is adequate.
#
#     Handling non-convergence is discussed in greater detail in
#     Lab Session 3
#
##'
##' ## Hausman Test: Is the between effect different from the within effect? ----
##'
#'
#' First diagnostics:  Hausman Test
#' 
#' After fitting a model: diagnostics
#'
#   Historically: 
#      The Hausman test asks whether the mixed model correctly 
#      estimates the effect of ses
#
#   But in fact:
#       Do we need a contextual variable? i.e.
#       Is the between-school effect of ses not significantly different
#          from the within-school effect of ses
#   If so, then we should include a separate effect for the between-school
#   effect of ses and we will be able to estimate the within school effect
#   correctly.
#
#' Model with contextual variable for ses, i.e. each school's mean ses:
#' Note: we can use the cvar function for convenience or we can
#' create a variable in the data frame, i.e. ses.m above.
#' We will use cvar because it's always easy to use even if the
#' variable has not been defined.
#'
#' Moreover, cvar works correctly with factors generating mean
#' values of indicators for each level of the factor omitting the
#' reference level. We will illustrate this later.
#'
##'
##' ## Fitting a model with a contextual mean ----
##'

hs$id <- hs$school
fitc <- lme( mathach ~ (ses + cvar(ses,id))* Sector, hs,
             random = ~ 1 + ses | id )
summary( fitc )
wald( fitc )
wald( fitc, -1)     # overall test of FE model
wald( fitc, 'cvar')
wald( fitc, 'ses')  # overall test of all 'ses' effects
wald( fitc, 'Sector')  # overall test of all 'Sector' effects
wald( fitc, ':')  # overall test of all interaction effects
                  # i.e. are lines parallel between sectors

##'
##' ## Role of contextual variable for ses ----
##' 
#
#     Including cvar(ses, id) has two important consequences:
#
#     1) It unbiases the estimated effect of ses so it estimates
#        the WITHIN-SCHOOL effect of ses unbiased by the between
#        school effect of ses
#
#     2) It provides an estimate of the 'contextual effect' of ses
#        and of the BETWEEN-SCHOOL (= compositional) effect of ses which
#        is equal to the sum of the contextual and the within-effect
#
##'
##' ## Interpreting the model with contextual effect ----
##'

wald( fitc )

#'  The coefficient for cvar(ses,id) is very significant
#'  
#'  Interpretation of the coefficients of in this model:
#'
# > wald( fitc )
# numDF denDF  F.value p.value
#     6    76 555.2492 <.00001
#     -- This is a test that ALL parameters (including the intercept)
#        are equal to 0. It is generally of no interest. The 
#        command 'wald(fitc, -1)' tests all parameters except the
#        intercept and provides a test that model as a whole is
#        not predictive.
#
# numDF denDF  F.value p.value
#     6    36 407.3664 <.00001
# 
# Coefficients                Estimate Std.Error   DF   t-value p-value
# (Intercept)                13.356053  0.375374 1935 35.580620 <.00001
#
#             -- E(Y) when ses = 0 for a Catholic school 
#                mean ses = 0
#
# ses                         1.699397  0.309554 1935  5.489821 <.00001
#
#             -- Expected difference in E(Y) associated with
#                a difference of 1 unit of individual ses 
#                within a given Catholic school. Often called the
#                'WITHIN-SCHOOL' effect of ses.
#
# cvar(ses, id)               2.407785  1.062930   36  2.265235 0.02961
#
#             -- Expected different in E(Y) associated with
#                a difference in a Catholic school mean ses of 1 unit
#                with individual ses constant. Often call the 
#                'CONTEXTUAL' effect of ses.
#
#                The sum of the 'WITHIN-SCHOOL' and the 'CONTEXTUAL'
#                effects is the 'COMPOSITIONAL' or 'BETWEEN-SCHOOL' effect
#
# SectorPublic               -0.934345  0.562878   36 -1.659942 0.10561
#
#             -- Public intercept minus Catholic intercept
#
# ses:SectorPublic            1.173092  0.467702 1935  2.508206 0.01222
#
#             -- coef of 'ses' in Public schools minus value in
#                Catholic schools
#
# cvar(ses, id):SectorPublic  1.414896  1.465217   36  0.965656 0.34066
#
#             -- ditto for coefficient of 'cvar(ses, id)'
#
##'
##' ## Estimating the compositional effect (between effect) ---- 
##'

wald( fitc )

#' All 'possible' effects of ses:
#' 
#' They're actually 'infinite' consisting of ALL linear combinations
#' of the row space of the following matrix which has dimension 4.
#' 
#' SO: infinite in number but finite in dimension
#' 
L <- rbind(
  'Within-school effect of ses | Cath'         = c(0, 1, 0,   0, 0, 0),
  'Contextual effect of ses | Cath'            = c(0, 0, 1,   0, 0, 0),
  'Between-school effect of ses | Cath'        = c(0, 1, 1,   0, 0, 0),
  'Within-school effect of ses | Pub'          = c(0, 1, 0,   0, 1, 0),
  'Contextual effect of ses | Pub'             = c(0, 0, 1,   0, 0, 1),
  'Between-school effect of ses | Pub'         = c(0, 1, 1,   0, 1, 1),
  'Within-school effect of ses | Pub - Cath'   = c(0, 0, 0,   0, 1, 0),
  'Contextual effect of ses | Pub - Cath'      = c(0, 0, 0,   0, 0, 1),
  'Between-school effect of ses | Pub - Cath'  = c(0, 0, 0,   0, 1, 1))
wald( fitc, L)
#'
#' We can ask 'subquestions' with multiple dfs
#'
#' Question: Is there evidence of an effect of ses in Catholic schools?
#' 
wald(fitc, list('ses in Catholic schools' = L[1:3,]))
#'
#' Question: Is there evidence of an effect of ses in Catholic schools?
#' 
wald(fitc, list('ses in Public schools' = L[4:6,]))
#'
#' Question: Is there evidence of a difference between Public and Catholic?
#' 
wald(fitc, list('Difference between effect of ses between Sectors' = L[7:9,]))
#' or
wald(fitc, ':')
#'
#' Note: that the estimated different in contextual effect is
#' larger than the difference in within effect, yet the p-values
#' are 'reversed'.
#' 
#' This illustrates the fact that there is generally more information
#' -- i.e. smaller SEs -- for within effects than for contextual effects
#' -- although not always.
#' 
#' Although the interaction in not significant - there is much less power to 
#' detect between-school effects than within-school effects - depending on the 
#' purposes of the analysis, it might not be reasonable to assume that the slopes 
#' are equal just because the p-value does not provide evidence that they are 
#' not.
#' 
#' QUESTIONS:
#'
#'  1) How would you test whether there is ANY difference between the
#'     two sectors -- not just in the effect of SES.
#'  
#'  2) Why does the second to last F test have 2 numDFs although there are
#'     three lines in the L matrix?
#'  
##'
##' ## Visualizing the fitted model ----
##'
#'
#' We create a data frame with predictor values for which we want to
#' visualize the predicted values for the model.
#'
#' Since the data have two levels, we create a data set with
#' school characteristics and deviations of students within schools

pred <- expand.grid(ses.mean = c(-1,0,1), 
                    Sector = levels(hs$Sector), # this ensure that the factor is created
                    ses.dev = seq(-1,1,.1))     # properly
pred$ses <- with(pred, ses.mean + ses.dev)
pred$id <- with(pred, paste0(Sector," mean ses: ", ses.mean))
pred
#'
#' We can now use the wald function to generate predicted
#' values and standard errors
#'
#' The 'getX' function generates the design matrix over the 
#' data frame pred
#' 
#' Note that it is possible to use the 'predict' method but it 
#' does not provide standard errors
#' 
round( head( getX(fitc, pred)),2)

ww <- as.data.frame( wald( fitc, getX(fitc, pred)) )
head(ww)
xyplot( coef ~ ses | id, ww, type = 'l')
gd(col = 1:6, lwd = 2)  # select colours
xyplot( coef ~ ses | Sector, ww, groups = id, type = 'l')
xyplot( coef ~ ses | Sector, ww, groups = id, 
        auto.key = T, type = 'l')
xyplot( coef ~ ses | Sector, ww, groups = id, 
        auto.key = list(columns = 2, lines = T, points = F), type = 'l')
xyplot( coef ~ ses , ww, groups = id, 
        auto.key = list(columns = 2, lines = T, points = F), type = 'l')
gd(col=rep(1:2,each = 3), lty = rep(1:3,2), lwd = 2)
xyplot( coef ~ ses , ww, groups = id, 
        ylab = 'predicted math achievement',
        auto.key = list(columns = 2, lines = T, points = F), type = 'l') +
  layer(panel.abline(v=c(-1,0,1), lty = 2))
#'
#' EXERCISE:
#' 
#' - Show how the estimated coefficients are related to the last
#'   plot.
#'
##'
##' ## Plotting error bars ----
##'
#'
#' The data frame created by wald includes the information necessary to
#' plot error bounds on the predicted lines
#'
#' The 'panel function' panel.fit will produce the error bars.
#' The easiest way to use it is with 'layers' provided by
#' the 'latticeExtra' package
#' 
#' In the call to xyplot, provide arguments for the following
#' parameters:
#' 
#' - fit: the fitted value for which a line or curve will be plotted
#' - upper: the upper boundary of the error bar
#' - lower: the lower boundary
#' - subscripts = T (so xyplot will pass the necessary information to 
#'   the panel function)
#'
#' Then you need to add a '+' at the end of the 'xyplot' command and
#' follow that with:
#' 
#'    layer(panel.fit(...))   if you did not use groups, and
#'    
#'    glayer(panel.fit(...))  if you did use groups
#' 
head(ww)

xyplot( coef ~ ses | Sector, 
        ww, groups = id, 
        ylab = 'predicted math achievement',
        fit = ww$coef,
        upper = ww$U2, lower = ww$L2, 
        subscripts = T,
        sub = 'predicted mean with appx. 95% error bars (NOT prediction bars)',
        auto.key = list(columns = 2, lines = T, points = F), type = 'l') +
  glayer(panel.fit(...))

xyplot( coef ~ ses | ses.mean, 
        ww, groups = id, 
        ylab = 'predicted math achievement',
        fit = ww$coef,
        upper = ww$U2, lower = ww$L2, 
        subscripts = T,
        layout = c(1,3),
        sub = 'predicted mean with appx. 95% error bars (NOT prediction bars)',
        auto.key = list(columns = 2, lines = T, points = F), type = 'l') +
  glayer(panel.fit(...))

xyplot( coef ~ ses | paste0('Mean school ses = ',ses.mean), 
        ww, groups = id, 
        ylab = 'predicted math achievement',
        fit = ww$coef,
        upper = ww$U2, lower = ww$L2, 
        subscripts = T,
        layout = c(1,3),
        sub = 'predicted mean with appx. 95% error bars (NOT prediction bars)',
        auto.key = list(columns = 2, lines = T, points = F), type = 'l') +
  glayer(panel.fit(...))

##'
##' # EXERCISES ----
##'
#'  
#'  1. Starting with the model above, test whether Sex improves prediction
#'     in the model. Consider including a contextual variable for Sex. What
#'     is its interpretation?
#'
#'  2. Is is reasonable to use the contextual variable for Sex as the
#'     only regressor for the contextual effect of Sex or should other
#'     variables be used as well?  Can you create these variables and
#'     include them in your analysis? What do they reveal?
#'
#'  See [Visualizing the coeficients](Visualizing_the_Coefficients.pdf)
##'
##' ## Using CWG instead of raw SES ----
##'

fitcd <- update( fitc, . ~ (cvar(ses,id) + dvar(ses,id)*Sector))
summary( fitcd )
summary( fitc )
wald(fitc, L)
#'
#' QUESTIONS:
#'
#'  1. what's the same and what's different?
#'  2. How are the various effects of ses related
#'  3. Create an L matrix to estimate the
#'     3 effects of ses with fitcd as in the previous example
#'
#'
#' Note: replacing ses with dvar(ses,id) in the RE model does
#' give an **mathematically** equivalent model but **NOT**
#' a **numerically** equivalent model. 
#' 
#' That is, with calculations using infinite precision the 
#' the two models would give equivalent results but 
#' _with finite precision_ the 'dvar(ses,id)' model will,
#' in general, converge more easily.
#'
#'
##'
##' ## CWG vs raw in RE model ----
##'
#'  
#' Alternative but non-equivalent RE parametrization
#' 
#' Using 'dvar(ses,id)' or raw 'ses' in the FE model
#' produces an equivalent model.
#' 
#' This is not so in the RE model. The
#' 
#' 'random = ~ 1 + ses | id'  (1)
#' 
#' model is essentially different from the 
#' 
#' 'random = ~ 1 + dvar(ses,id) | id'  (2)
#' 
#' model. 
#' 
#' In model (1) the 'point of minimum variance' of school
#' regression lines corresponds to a _common_ value of ses 
#' for all schools.
#' 
#' In model (2) the 'point of minimum variance' of school
#' regression lines has a consistent value 
#' _relative_ to each school's mean ses.
#' 

fitca <- update( fitc, random = ~ 1 + dvar( ses, id ) | id)
summary( fitca )
summary( fitc )
#'
#' Which is better? It will depend on the true state of nature.
#' 
#' What do the data tell us?
#'
#' Neither model is nested in the other -- in fact they have the
#' same DFs -- so we can't get a p-value for a test. We can
#' compare with AIC:  
#' 
anova( fitca, fitc )
#'
#'  fitca looks better, barely! 
#'  
#'  More importantly, it seems to make more sense.
#'
##'
##' # EXERCISES ----
##'
#' 1. Now that we have another Level 2 variable ( cvar(ses, id) ),
#'    we could consider whether there are interesting interactions
#'    between it and other variables.
#'    
#'    Fit a model with interactions between cvar(ses,id) and other
#'    variables.
#'
#' 2. Is there evidence that these interactions are non-zero in the
#'    population?
#'    
#'      -  BEWARE: Do not merely look at the individual p-value
#'         Either remove effects one at a time or test effects
#'         simultaneously with 'wald' or anova (need to refit with
#'         method = "ML" )
#'
#'      -  What do you conclude wrt interactions?
#'
##'
##' # REML vs ML ----
##'
#' Previous fits with REML (default)
#' 
#' Here we use a 'sub-par' model (no contextual effect) to
#' illustrate convergence issues.
#' 
#' FE: ~ 1 + ses + Sector + ses:Sector
#' 
#' RE: ~ 1 + ses | id
#' 
#' Fitting with 'REML'
#' 
#' Maximum likelihood optimization is done only on the RE model,
#' i.e. the G matrix and sigma -- or G and R.
#' The FE model is then estimated using GLS.  This makes
#' optimization easier because there are fewer parameters --
#' in this case 4: 
#' 
#' 1. 2 SD parameters for REs: 'beta_1' and 'beta_ses' (diagonal of G)
#' 2. covariance parameter for REs: between 'beta_1', and 'beta_ses'
#'    (off-diagonal of G)
#' 3. the SD parameter for individual error
#'
#' Fit with 'ML'
#' 
#' Maximum likelihood optimization done on all paramters together.
#' Generally more numerically difficult.
#' 
#' Pros and Cons:
#' 
#' __REML:__ better estimates of variances, good for Wald tests. But,
#' can use Likelihood-Ratio Tests (LRT with anova) only to
#' compare models with identical FEs and nested RE models. 
#' However, beware that LRTs for RE models can be problematic
#' if the null model is on the 'edge' of the full model.
#' 
#' __ML:__ Not so good estimates of variances, not so good for Wald tests.
#' But LRTs can compare models that are nested wrt to FEs and or
#' REs.    
#' 
#' REML fit (default):              
fit <- lme( mathach ~ ses * Sector, dd, 
            random = ~ 1 + ses | id,
            na.action = na.exclude,
            control = list(msMaxIter=200, msVerbose=T))
#'
#' ML fit:
fit.ml <- lme( mathach ~ ses * Sector, dd, 
               random = ~ 1 + ses | id,
               na.action = na.exclude, 
               method = "ML",
               control = list(msMaxIter=200, msVerbose=T))
#'
#' equivalent:
fit.ml <- update(fit, method = 'ML')   
#'
#' Here ML takes more iterations but gives up with 
#' 'singular convergence' perhaps because the estimated 
#' RE variance is closer to singular, which, combined with
#' the larger number of parameter, leads 'nlminb' to 
#' declare singular convergence: the peak is too flat.
#'
#' Note: increasing msMaxIter won't help.
#' 
#' REML maximizes only over RE parameters, 
#' while ML maximizes over all parameters,
#' hence more challenging optimization 
#' 
#' To see the fit at the last iteration, use 
#' 'returnObject = TRUE':
#' 
fit.ml <- lme( mathach ~ ses * Sector, dd, random = ~ 1 + ses | id,
               na.action = na.exclude, 
               method = "ML",
               control = list(msMaxIter=200, 
                              msVerbose=T,
                              returnObject = TRUE))
summary(fit.ml)
#' 
#' Shows high correlation in the RE model between b_1 and b_ses.
#' This suggests that the 'point of minimum variance' is far from
#' the observed values of ses and that the random true school 
#' regression lines seem close to parallel.
#' 
#' Possible remedies:
#' 
#' - drop random slope
#' - recenter ses
#' - add a contextual effect and CWG random effect: ~ 1 + dvar(ses,id) | id
#' - explore different models
#' - change the criterion for lme to declare singularity, 
#'   i.e. excessive flatness near the likelihood peak with 'sing.tol' 
#' 
fit.ml <- lme( mathach ~ ses * Sector, dd, 
               random = ~ 1 + ses | id,
               na.action = na.exclude, 
               method = "ML",
               control = list(msMaxIter=200, msVerbose=T,
                              sing.tol=1e-20))
summary(fit.ml)

##'
##' ### Notes on testing: ML vs REML ----
##'
#' The default for lme is fitting with 'Residual Maximum Likelihood' (REML)
#' -- it has other interpretations but everyone agrees on 'REML' --
#' With REML, maximum likelihood is applied ONLY to the parameters for
#' random effects, i.e. the G matrix and sigma (or R). The Betas play
#' no direct role, they are only estimated with Generalized Least Squares
#' as an afterthought.  This means that the likelihood with REML is only
#' informative for the the RE model: the G and R matrices. You can't
#' compare two models that have different FE models.
#'
#' Full maximum likelihood (ML) does look at Betas, G and R together. So the
#' likelihood with ML can be used to test two models with 
#' nested FE models.
#'
#' The consequence is that:
#'
#' If two models have been fitted with REML, the must have identical FE models
#' to compare them with 'anova': p-values, AIC, BIC.
#'
#' If two models have been fitted with ML, you can use 'anova' regardless
#' of the FE or the RE models --- PROVIDED they have the same response 
#' variable. This includes requiring that they have the same set of
#' observations, hence the same omitted rows due to missing values.
#'
#' Wald tests can be used for both ML or REML fits. They might be more accurate
#' with REML fits. However, Wald test tend to useful mainly for the FE part of
#' the model. 'wald' in 'spida2' only works for the FE model.
#' To compare two RE models (with identical FE models) LRT is generally better
#' than Wald. However, classic p-values tend to be too large in many cases
#' and can be adjusted through simulation.
#'
#' Summary:
#' 
#' - Wald is ok for the FE model whether ML or REML
#'   
#' - anova (with simulation adjustment) is ok for the RE model
#'              provided the FE models are identical
#'              
#' - anova can be used to test different FE models or
#'              models that differ in both FEs and REs
#'              ONLY IF THE MODELS ARE fitted with ML.
#'
##'
##' # Diagnostics ----
##'
##'
##' ## Diagnostics with Level 1 residuals ----
##'
#'
#'  Level 1 residuals
#'
#' Level 1 standardized residuals versus yhat:
#' 
plot(fitc) 
plot(fitc) + layer(panel.loess(x,y, lwd=3,col='red'))
#' Not as generous as plot for 'lm'.
#' 
#' In contrast with least-squares, it is possible to have a
#' non-flat least-squares regression here.  
#'
diag.df <- data.frame( resid = resid(fitc), fitted = fitted(fitc))
summary( lm( resid ~ fitted, diag.df))   
#' 
#' It could be a
#' symptom of lack of independence between school-level
#' random effects and level 1 errors or between schoo-level
#' random effects and other predictors. One possible cure
#' is adding a contextual effect if there isn't one.
#' 
#' Normality:
qqnorm( fitc  )
qqnorm( fitc , abline = c(0,1), id = .01 )
#' BEWARE: data on horizontal axis SO distribution is
#' slightly platykurtic. Variance might be overestimated with
#' extreme values thereby overajusting.
#'
##' 
##' ## Scale - location plot: ----
##' 
#'
#' Using sqrt(abs(resid)) produces an approximate normal if everything is ok
plot( fitc , sqrt( abs( resid(.))) ~ fitted(.))
plot( fitc , sqrt( abs( resid(.))) ~ fitted(.)) + 
  layer(panel.loess(x, y, lwd = 2, col = 'red'))

plot( fitc , 
      sqrt( abs( resid(.))) ~ fitted(.)| Sector, 
      id = .01, sub = "Scale-Location Plot") +
  layer(panel.loess(x, y, lwd = 2, col = 'red'))

plot( fitc , sqrt( abs( resid(.))) ~ cvar(ses,school)| Sector, 
      id = .01, sub = "Scale-Location Plot") +
  layer(panel.loess(x, y, lwd = 2, col = 'red'))

#' other views of residuals: 

plot( fitc, Sex ~ resid(.) | Sector, id = .01)
plot( fitc, id ~ resid(.) | Sector, id = .01)

#' Omitted variables:

plot( fitc , resid(.) ~ fitted(.)| Sector*Sex, id = .01)

#' Why MM residual plots don't look like OLS residual plots:
#'
#' Evil exam question: Performing OLS regression diagnostics, you find that
#'       the residuals are correlated with the predicted values. What would
#'       this signify and what action should you take?
#'       
#' Answer: at end of script.
#'
#' With MM residuals:
#'
#' Why are residuals not uncorrelated with predicted values ( a canon of OLS )?
#'
#' Reason: if we were using OLS on the pooled data, resids would be uncorrelated.
#'
#' But MMs is roughly like doing OLS on pooled data,
#' then adding an additional fit at the cluster level.
#'
#' If unaccounted between cluster effects are in the same
#' direction as within cluster effect then higher residuals with higher fitted values
#' will get a second fit, This shrinks the high positive OLS residuals and
#' stretches the corresponding fitted values creating a negative slope
#' in the residual vs fitted plot.
#'
##'
##' ## Diagnostics with Level 2 residuals ----
##' 
#'
#' Make cvar(ses,id) a variable in dd
#'
dd$ses.m <- with( dd, cvar( ses, id))
fitc <- lme( mathach ~ (ses + ses.m) * Sector, dd,
             random = ~ 1 + ses | id )
#'
#' - 'ranef(fitc)' produces Level 2 errors: u in each school
#' - 'coef(fitc)'  produces Level 2 BLUPS: beta = gamma + u
#' 
some( coef ( fitc ) )     # BLUP in each cluster
some( ranef ( fitc ) )    # RE in each cluster

#' Note: coef( fitc ) =  fixed.effect + random.effect

#' pairs( cbind( coef(fitc), ranef( fitc)) )   # can you interpret what this says?

re <- ranef( fitc, aug = T)   # creates data frame with up( dd, ~ school) variables
some( re )
plot( re )     # special plot method

qqnorm( fitc, ~ ranef(.) | Sector, id = .05)

Init3d()
Plot3d( `(Intercept)` ~ ses + ses.m | Sector, re)  # note funny names in backquotes
Ell3d()

#'
#' EXERCISE:
#' 
#' - Plot random effects against variables you feel might reveal a pattern
#' - Experiment with the alternative RE model.
#'
##'
##' ## Influence diagnostics -- drop one row or one cluster at a time ----
##' 
#' Time consuming but very useful: drop one diagnostics
#'
#' Here's a quick dropone function
dropone <- function(fit) {
  data <- getDate(fit)
  index <- 1:nrow(data)
  lapply(index, function(i) fixef(update(fit,data=data[-i,])))
}
#'
#' Here's a fancier one that will drop clusters and 
#' return NA if there's an error in fitting
#' 
dropone <- function(fit, form = NULL, FUN = fixef, data = getData(fit),...) {
  if(is.null(form)) {
    by <- factor(1:nrow(data))
    data$by <- by
    dframe <- data
  } else {
    by <- model.frame(form, data)
    by <- do.call(paste, c(by, sep='/'))
    data$by <- factor(by)  # bad if by already exists in data
    dframe <- up(data, form)
  }
  values <- dframe$by
  names(values) <- values
  ret <- lapply(values, function(v) {
    ret <- try(update(fit, data = data[by != v, ,drop=FALSE],...))
    if(class(ret) == 'try-error') NA else FUN(ret)
  })
  ret <- do.call(rbind,ret)
  colnames(ret) <- paste0('b_',colnames(ret))
  cbind(dframe, ret)
}
#' Ideas for improvement:
#' 
#' - optionally standardize relative to SEs
#'
#' The following takes about 5 minutes so it's been precooked
#' so we can take it out of the oven.
#'  
# system.time(
#    fits.dropone <- dropone(fitc)
# )
#'
#' Take fits.dropone out of the oven:
con <- url('http://blackwell.math.yorku.ca/ICPSR2017/fits.dropone.rda')
load(con, verbose = T)
close(con)
#'
#' We will look at 'drop one' by school:
#'
system.time(
  fits.dropschool <- dropone(fitc, ~ id) # only 6 seconds
)
#'
#' EXERCISE: (not relevant here)
#' 
#' - If some models don't converge, rerun fitc with 
#'   larger msMaxIter, returnObject = T, sing.tol = 10e-20
#'   to see if all drops will converge.
#'
scatterplotMatrix(fits.dropschool[,grep('^b_',names(fits.dropschool))])
#'
#' What does this show?
#' 
#' Which points represent Catholic schools and which Public schools?
#'
names(fits.dropschool)
Init3d()
Plot3d(`b_(Intercept)` ~ b_ses + `b_ses:SectorPublic` | Sector,
       fits.dropschool, sphere.size = 1.5)
if(interactive) Id3d() # to identify schools
id_miss <- is.na(fits.dropschool$b_ses)
categ <- ifelse(dd$id %in% c('7688','7172'), 'out', 'ok')
subset( fits.dropschool, id %in% c('7688','7172'))
gd()
xyplot(mathach ~ ses | paste(id), dd, groups = categ) +
  glayer(panel.lmline(x,y, lwd = 2, col = 'red'))
# Not surprising!

##'
##' ## Looking at the model ----
##' 
#' An invaluable exercise: Plotting the fitted model
#'   
#' i.e. hand made effect plots
#'
##'
##' ### Predicted response ----
##' 
dd$yhat <- predict( fitc, level = 0)  # population level prediction
dd$yhat.sch <- predict(fitc, level = 1) # school level prediction
#'
#' NOTE: the 'predict' method in 'nlme' numbers levels in the
#' reverse order to the conventional one. 
#' 
#' In a model with two levels: 
#' 
#' - Level 1: student, within
#' - Level 2: school
#' 
#' nlme refers to:
#' 
#' - level 0: population
#' - level 1: school
#' - level 2: studentf
#' 
xyplot( yhat ~ ses | Sector, dd)
Init3d()
Plot3d( yhat ~ ses + ses.m | Sector, dd)
with( dd, Points3d( ses, yhat2, ses.m ))
Axes3d()
dd$yhat2 <- predict(fitc, level = 1)  # school level with BLUPs

Plot3d( yhat2 ~ ses + ses.m | Sector, dd, groups = id)
Plot3d( yhat2 ~ ses + ses.m | Sector, dd)
Fit3d(fitc)
Plot3d( mathach ~ ses + ses.m | Sector, dd)
fixef(fitc)
Fit3d(fitc)

# Nice! But we would to look at yhat ~ ses fixing a few values for
# ses.m so we could consider the gap between sectors
#
# We would like to select a few values of ses.m
# e.g. -.5, 0, .5
# and a range of values of ses for each ses.m
# Say ranging from ses.m -1 to ses.m + 1
#
# i.e. ses.d from -1 to + 1
#

ps <- c(0,10,25,50,75,90,100)/100   # the default only shows quartiles

quantile ( dd$ses, ps )
quantile ( dd$ses.m, ps )
quantile ( dd$ses - dd$ses.m, ps )

# So let's show what happens for a school with mean ses: -.5, 0, .5
#
# each with students ranging from 1 below to 1 above the school mean
#
# First step: refit the model in a way that depends only on each
# observation, i.e. does not need 'safe prediction'
#
# To do this be sure to replace cvar(ses, id) with ses.m is you aren't already using it
#
# First add it to the data frame if it isn't there

dd$ses.m <- with( dd, cvar( ses, id ))

fitn <- update( fitc, . ~ ses * Sector + ses.m )
summary( fitn )   # should be exactly the same

##'
##' ## Creating prediction data ----
##'


# Create prediction data frame with all combination of
# of predictor values.
#
# Step 1: Variables that do not 'depend' on each other: Note;
#         ses depends on ses.m because the range of ses will be
#         higher of lower depending on ses.m. However, the range
#         of deviations will be the same.
#

pred <- expand.grid(
  Sector = levels( dd$Sector),   # ensures correct order for factors
  ses.m = c( -.5, 0, .5),   # want more if effect is not linear
  ses.d = c(-1, 0, 1)      # ditto
)
pred

# Step 2:  (if needed)
#   Create raw variables for model, if they don't exist already:

pred$ses <- pred$ses.m + pred$ses.d
pred

# Step 3: predict response

pred$mathach <- predict( fitn, pred, level = 0) # level = 0 to get population
# estimates instead of school BLUPS

# Step 4: Plot

xyplot ( mathach ~ ses , pred, groups = factor(ses.m):Sector, type = 'l',
         auto.key = T)

td( lwd = 3, cex = 1.5)
xyplot ( mathach ~ ses | Sector , pred, groups = factor(ses.m):Sector, type = 'b',
         auto.key = list( columns = 3, lines = T, points = T))

xyplot ( mathach ~ ses | ses.m , pred, groups = factor(ses.m):Sector, type = 'b',
         layout = c(1,3),
         auto.key = list( columns = 3, lines = T, points = T))

#
#  EXERCISE:
#     1) copy a sketch of this graph ( or print it)
#        and show where each coefficient belongs on the graph
#     2) what other features of the graph would be interesting to
#        estimate?
#     3) On the graph, indicate:
#             a) the contextual effect of ses in the Catholic sector; in the
#                Public sector.
#             b) the compositional effect of ses in each sector.
#     4) How would you express these effects in terms of estimated coefficients.
#
#
#  How to ask and answer linear questions
#

# Estimate the within school effect of ses in each Sector
#
# Note: If constructing L matrices is confusing, write out the
#       model on paper with betas and variables:
#
#    E(mathach) = b0 + b.ses x ses +b.SectorPublic x SectorPublic
#                    +  b.ses.m x ses.m + b.int x ses x SectorPublic
#
# Then write out the model in each Sector making the appropriate sub
# substitutions:
#
# In the Catholic Sector: (because SectorPublic = 0)
#
#    E(mathach) = b0 + b.ses x ses
#                    +  b.ses.m x ses.m
#
# In the Public Sector:
#
#    E(mathach) = b0 + b.ses x ses +b.SectorPublic x 1
#                    +  b.ses.m x ses.m + b.int x ses x 1
#    gathering terms:
#               =  (b0 + b.SectorPublic)
#                  +(b.ses + b.int) x ses
#                    +  b.ses.m x ses.m
#
#    so the intercept is b0 + b.SectorPublic
#    and the slope wrt ses is b.ses + b.int
#
# This should help in formulating various questions in terms of
# estimated coefficients.
#
#
wald( fitn )

L <- list( "Within school effect of ses" =
             rbind( "Catholic" = c(0,1,0,0,0),
                    "Public" = c(0,1,0,0,1),
                    "Pub-Cath" = c(0,0,0,0,1))
)
L
wald( fitn, L ) # why does numDF show 2 dfs although there are 3 rows in L

L.context <- list( "Contextual effect of ses" =
                     rbind( "Catholic" = c(0,0,0,1,0),
                            "Public" =  c(0,0,0,1,0),
                            "Pub-Cath" = c(0,0,0,0,0)))

L.context
wald( fitn, L.context )

L.comp <- list( "Compositional effect of ses" =
                  rbind( "Catholic" = c(0,1,0,1,0),
                         "Public" = c(0,1,0,1,1),
                         "Pub-Cath" = c(0,0,0,0,1)))
L.comp
wald( fitn, L.comp )

##'
##' ## Plotting effects with confidence bounds ----
##'


#
# Graphs to convey confidence bounds
#


#
# Sector gap
#  depends on ses, not on ses.m
#  We would like to graph the gap at different levels of ses
#  and to indicate a 95% CI around the estimated gap
#

wald( fitn )

# L matrix for the gap at a value of ses  :
#
# 2) Lmatrix for Public:
#    c( 1, ses, 1, 0, ses )
#
# 3) Lmatrix for Catholic:
#    c( 1, ses, 0, 0, 0)
#
# 4) Public - Catholic:
#    c( 0, 0, 1, 0 , ses)
#

pred.gap <- expand.grid( ses = seq( -2, 2, .05))   # why so many?

L.gap <- cbind( 0, 0, 1, 0, pred.gap$ses)
L.gap
wald( fitn, L.gap )

pred.gap <- cbind( pred.gap, as.data.frame( wald( fitn, L.gap), se = 2))
some( pred.gap )

td ( lty = c(1,2,2), lwd = 2, col = 'black')
xyplot( coef + L2 + U2 ~ ses, pred.gap,
        type = 'l',
        xlim = c(-2,2),
        ylab = "Estimated Catholic - Public gap in MathAch (+/- 2 SEs)")

# Modifying a lattice plot after the fact
if(interactive){
  trellis.focus ("panel", 1, 1)         # trellis = lattice
  panel.abline( h = -6:2, col = 'gray')
  panel.abline( v = -2:2, col = 'gray')
  panel.abline( h = 0, lwd = 2)
  trellis.unfocus()
}
#
# Note that the coef +/- 2 SEs intervals are ordinary approx. 95%
# confidence intervals.  The joint coverage probability is lower.
#
# There is a discussion of adjustments for multiple hypotheses
# in a later example.
#
#

##'
##' ## Exercises ----
##'
# Choose the problem that seems most appropriate:
#
# 1)  Add the variable 'Sex' to analysis above. Note that Sex has
#     a contextual variable: Sex composition, a variable with
#     interestingly different features in Catholic vs Public school.
#
#     If you wish to gain some experience with convergence problems
#     use the full Level 1 model ( ~ ses*Sex | id ) for your RE model.
#     If this fails to converge, try to use the advice given above to
#     correct the problem. If you prefer not to bother for now,
#     just use the additive model:
#
#             random =   ~1 + ses + Sector |id
#
#     Try to estimate the Gender Gap under various circumstances:
#     ses and Sector if the effect of Sex interacts with these variables.
#
#     Which circumstances are associated with large gender gaps,
#     with small gender gaps?
#
#     Make graphs of the gender gap as a function of its moderators,
#     preferably with confidence bounds.
#
# 2)  Analyze the data in 'bdf' in 'nlme':
#     > data (bdf)
#     > ?bdf
#     on Language Scores of 8-Graders in The Netherlands
#
#     This is similar to the 'hs' dataset but different enough
#     to be interesting.
#
#     The response of interest is 'langPOST' a measure of language
#     achievement at the end of the school year.
#
#     Potential predictors include: ses, sex, denominat (Sector),
#     Minority, IQ.verb as well as a pretest: langPRE.
#
#     This raises the interesting question of the best use for the
#     pretest. Should it be used as a covariate or should you analyze
#     the gain score: langPOST - langPRE as the response.
#
#     As usual, the best course of action may depend on the questions
#     and intended interpretation of the results.
#
# 3)  Study the role of 'Minority' in the 'hs' data following the
#     script below
#
# 4)  Validate the model above with the other half of the data 'hs2'
#     What differences do you note?
#

##'
##' # Second example ----
##'
#
# Example 2:
#
#  Exploring ses and Minority
#
#
# Preliminary diagnostics:  Model-based Level 1 diagnostics
#      Within-cluster OLS diagnostics -- where possible
#

#
# Fit the Level 1 OLS model (fixed-effects model in the sense of Econonometrics:
#
# -- Use only the Level 1 model with an interaction with the cluster factor
# -- This fits an OLS model in each cluster
# -- The within-cluster model might not be estimable in some clusters (indeed in any!)
# -- Use OLS diagnostics on residuals, etc, to explore the Level 1 model
#

fit <- lm( mathach ~ (ses * Minority )* id , dd)    # (Level 1 model) * cluster
summary( fit )
coef( fit )

# Q: Why not just ses * Minority * school ?
#    Why are many coefficients estimated to be NA

# An equivalent but more informatively parametrized model:

fit <- lm( mathach ~ id / ( ses * Minority ) - 1 , dd)    # fits a model in each school
summary( fit )
coef( fit )
length(coef(fit))
# we could reshape coef(fit) in a data frame with one column
# for each coefficient but it's more natural to use the
# built-in lmList function -- although it presents a few problems

# Using lmList: fits an OLS model in each cluster
#
# Steps:
# 1) Work out the Level 1 model using only Level 1 variables

mathach ~ ses * Minority

# 2) select only the variables you need (otherwise lmList drops rows with NAs even
#    if the variable is not used. Note that we add the cluster variable because we
#    will need it.

ddf <- model.frame( mathach ~ ses * Minority + id , dd )
some( ddf )

# 3) fit the model with lmList

fitlist <- lmList( mathach ~ ses * Minority | id )
# Bug: doesn't work because Minority has only 1 level in some clusters

fitlist <- lmList( mathach ~ ses * (Minority == "Yes") | id , ddf)
# Turn minority into a dummy variable: if Minority had k levels you
# need k - 1 manually created dummy variables

summary( fitlist )
zz <- coef( fitlist )      # more useful than lm output
some(zz)
names(zz) <- c("int",'ses','Min', "sesxMin")
some(zz)

# Graphics:

xqplot( zz )
# scatterplotMatrix ( zz, ell = T)
Init3d( family= 'serif', cex = 1.2)
Plot3d ( ses ~ Min + sesxMin, zz)
if(interactive) Id3d( pad = 2 )

xyplot( mathach ~ ses | id,
        dd,
        groups = Minority,
        auto.key = T,    cex =1.2,
        subset = id %in% c("C6074", "C4253") )

#
# Each school has all but two students in one group
# The schools all of whose students are in the same group
#     or for which all but one student are in the same group
#     do not show up here because
#     they have NAs among the coefficients
#

tab( dd , ~ id + Minority)
# schools with only one Minority class produce NAs for Minority and Minority*ses
# schools all students but one in one Minority class produce NAs for Minority*ses
#    -- it takes at least 2 of each to estimate an interaction


# within school diagnostics

wald( fit, "Minority")

dd$cook <- cookd(fit)
dd$hat <- hatvalues(fit)
dd$rstud <- rstudent( fit )  # internally standardized -- should be close to N(0,1)

plot( fit )   # does major diagnostics

#
# Question:
#     Does the residual vs fitted value plot violate properties?
#     Is this connected to the 'flattening' seen in the Normal QQ plot?
#     And with the pattern seen in the scale-location plot?
#

dd$Minority.j <- jitter( as.numeric( dd$Minority=="Yes" ))

# look for curvilinearity to show linear not adequate

# scatterplotMatrix( ~ rstud + ses + Minority.j + I(ses*Minority.j), dd)

# look for any pattern to find heteroscedasticity, also positive outliers

# scatterplotMatrix( ~ sqrt(abs(rstud)) + ses + Minority.j + I(ses*Minority.j), dd)

xyplot( rstud ~ hat , dd, groups = Minority, auto.key = T)  # like plot(fit)

xyplot( rstud ~ hat | id, dd, groups = Minority, auto.key = T ,
        layout = c(9,9))

# Q: what kinds of points have high leverage and why?
#
# Note: very high leverage in a particular school
#       will not affect a mixed model as it would
#       an OLS analysis using only that particular
#       school
#

tab( ~ round(cook), dd)
dd[ is.na( dd$cook),]
tab( ~ round(cook) + (hat==1), dd)
dd$res <- resid(fit)
xyplot( rstud ~ hat, dd, groups = round(cook), auto.key = T)  # Tapered influence plot (not as good)

# Look into schools with high or NaN for cookd.
# Is there any pattern?

dd$Minority.p <- with(dd, capply( Minority == "Yes", school, mean))
dd$hat.max <- with(dd, capply( hat, school, max))

ddu <- up ( dd, ~ school)
some( ddu )
xyplot( hat.max ~ Minority.p, dd)

#
# Q: What's happening in this plot? (hat = 0,1 versus close to 0,1)
#    Why is the plot M shaped
#
# Note:
# A hat-value of 1 would be VERY problematic with an ordinary model but
# here schools with high hat-values will get little weight for the
# affected parameters
#

##
## Check scaling of Level 1 variables within clusters:
##

fit.scale <- lm( cbind( Minority, ses) ~ factor(school), dd)
plot( resid(fit.scale))
var( resid( fit.scale ))   # sds are not too far apart ( within factor of 10 ok?)

##
##  Summary: it's useful to examine the data at level 1 but the influence of
##  points in the Level 1 model is not that directly relatied to their influence
##  in the mixed model because the presence of points with very high leverage
##  at Level 1 is associated with a low weight for the cluster in the mixed model.
##


#
# Fitting a mixed model: refining the random effects model
#

# random intercept

fit <- lme( mathach ~ ses * Minority, dd, random = ~ 1 | id)
summary(fit)

# random intercept and slopes


fit2 <- lme( mathach ~ ses * Minority, dd, random = ~ ses * Minority | id)
summary(fit2)

#
# Dealing with non-convergence
#

# default number of iterations are insufficient

lmeControl

# can increase iterations:

system.time(
  fit2 <- lme( mathach ~ ses * Minority, dd, random = ~ ses * Minority | id,
               control = list(maxIter = 200, msMaxIter = 200, niterEM = 100,
                              msMaxEval = 500, msVerbose = TRUE,
                              returnObject=TRUE))
)

summary(fit2)
VarCorr(fit2)
str(fit2)
fit2$modelStruct
#
# 'singular convergence' means that the log-likelihood is so flat
# that the optimizing function can't tell where the summit is.
#
# Unfortunately, this often happens because the variance of a random
# effect is TOO SMALL!!
#
# The algorithm does not work with the variance itself but with
# the log of the variance.  As the variance gets close to 0,
# the log of the variance goes to -infinity. So the optimizing
# algorithm keeps moving towards negative infinity until the likelihood
# gets so flat that it gives up.
#
# With singular convergence increasing iterations won't help.
#
#
# The number of parameters with 4 random effects is 10: 4 variances and 6 covariances
#              (with 3 it's 6 = 3 + 2, with 2 it's 3 = 2 + 1, and with with 1 it's 1
# So this is quite a large random model for the number of relevant observations: 80
# It isn't practical to fit the full model and then pare it back, we need to
# start simple and add significant components, e.g. 'forward stepwise'
#

# 'simplest' model: only a random intercept

fit

##'
##' ## Building and testing the RE model ----
##' 

#
# add random ses or random Minority: generally CWG better but it depends on model
# Note: centering CWG is not the same statistical model as using the raw variable
# -- you might like to try both and compare although we can't test because these
# models are not nested
#

#
# Building the RE model using forward stepwise approach
#


# Add one random effect at a time and test using LRT
# Notes:
#   1)  LRT by itself is highly conservative because the null hypothesis is on
#       the boundary of the parameter space,
#          SO
#               IF the test rejects, trust it
#
#               IF the test does not reject, use simulation to calibrate the p-value
#
#   2)  We can't use Wald tests because the log-likelihood is just too far from
#       quadratic between the hypothesis and the MLE.
#
# We illustrate ( we are lucky that this data set gives us examples of both situations )
#

# random effect of Minority:

fit.m <- update( fit, random = ~ 1 + dvar(Minority, id) | id)
anova(fit, fit.m )  # the nominal p-value is approx. 0.0001 so we reject the null
# and consider that the variability in the Minority gap from
# school to school is larger that what could be explained
# simply by the random variability of students within schools


# random effect of ses:

fit.s <- update( fit, random = ~ 1 + dvar(ses,id) | id)
anova(fit, fit.s )  # the nominal p-value is 0.0717 so there's stronger evidence
# for a random effect of Minority



# random effecs of ses and minority
fit.sm <- update( fit.m , random = ~ 1 + dvar(Minority, id) + dvar(ses,id) | id )
summary( fit.sm )
getVarCov( fit.m )  # Null
getVarCov( fit.sm ) # Alternative
# note there are 3 more parameters in the Alternative model

cond( getVarCov( fit.sm ) )  # High but not astronomical

anova( fit.m, fit.sm )  # get nominal p = 0.1527 which suggests we might not
# need random effect of ses but we'll simulate to
# calibrate

#
# Simulation to adjust p-values
#

##'
##' ## Using simulation to calibrate p-values ----
##'

#
# Simulation: a subtle but simple idea:
#
#     Our OBSERVED NOMINAL P-VALUE is 0.1527
#
#     Generate random data sets under the null hypothesis many many times.
#     For each data set, compute the NOMINAL P-VALUE for the test
#     Use a graph to show the distribution of the NOMINAL P-VALUEs.
#     To approximate the TRUE P-VALUE use the EMPIRICAL P-VALUE from
#         the simulation. The EMPIRICAL P-VALUE is the proportion of
#         simulations that produced a NOMINAL P-VALUE smaller that the
#         OBSERVED NOMINAL P-VALUE.
#

anova( fit.m, fit.sm )
plot( sim.out <- simulate( fit.m, m2 = fit.sm, nsim = 100, method = "REML" ) )

# We should use 1000 with more time available
# when I did it a NOMINAL P-VALUE of 0.1527 on the graph corresponded
# to an EMPIRICAL P-VALUE close to 0.05

# Based on this I am tempted to go with fit.sm:
#       random = ~ 1 + dvar(Minority, id) + dvar(ses,id) | id

# Question: REQUIRES TYPING!
#
#    Fit a model using raw ses and Minority in the random effects model and
#    compare how it fits with the model using CWG deviations.
#
#    1) Which one seems to fit the data better?
#
#    2) Compare the condition numbers of the 2 G matrices? Which
#       G matrix is flatter?

##'
##' ## Simplifying the FE model ----
##'

#
# Do we need contextual effects? Analogue of the Hausman Test in Econometrics
#


# Starting model

summary(fit.sm)

# Model with contextual effects
# Should we go with all interactions? Or add one at a time?
# Depends in part on theory.

fit.sm.ac <- update( fit.sm, . ~ (. )*cvar(ses,id)*cvar(Minority,id))
summary( fit.sm.ac )

# The number of parameters, here is large with 3 between cluster regressors
# for tri-variate derived data.

#
# Simplifying the model
#
##'
##' ## Dos, don'ts and whys ----
##' 

#
# Simplifying the model -- different approaches:
#
# 1) ABSOLUTELY TERRIBLE:  drop all terms with big p-values.
#    Why bad? I) resulting model might violate principle of marginality
#             II) 2 or more terms might all have large p-values, yet
#                 jointly they could be highly significant
#             III) data-driven, ignores background theory and interpretability
#                 of the model.
#
# 2) STILL BAD: drop one term at a time with big p-values then refit:
#    Why bad? I and III
#
# 3) Drop "non-marginal" terms one at a time.
#    Getting better. Still a problem with III
#
# 4) Test whether you can drop "non-marginal" groups of terms (of 1 or more)
#    that have a theoretical meaning:
#
#    a) from the top: start with highest order interactions and move down,
#             e.g. test 4-way interactions, drop if not significant,
#                  then test 3-way, etc.
#
#    b) from the side: test whether you can drop all terms or all interactions
#       that involve a particular term.
#
#  Note that in 4, at each step, you are comparing two models:
#  the 'full' current model at each stage and a candidate smaller model.
#  In this comparison, the candidate model is the H0 model and the
#  current model is the alternative Ha model.
#
#  We illustrate 4(a) and (b). This approach can be laborious but is easy
#  with Wald tests:

##'
##' ## Wald or Likelihood Ratio Test ----
##' 

#
#  Technical note: An interesting trade-off:
#
#  We could use Wald tests [wald(...)] or LRTs [anova(fit1,fit2)] for to test
#  a hypothesis with multiple terms.
#  Generally, where feasible, LRT is considered better than Wald.
#
#  However to test fixed effects in a mixed model, we need to fit with
#  method = "ML" instead of method = "REML" (the default for 'lme')
#  and REML is considered better than ML.
#
#  So we have a choice of the LRT approach:
#
#  1) - LRT: refit the current (Ha) model with method = 'ML' - call it fit.Ha
#     - write and fit the smaller model with method = 'ML' - call it fit.H0
#     - compare the two models with 'anova( fit.H0, fit.Ha )'
#
#  2) - Wald: Use the REML model and use 'wald( fit.HA, pattern )' to test
#       the hypothesis that the coefficients that are candidates for
#       dropping could all be simultaneously 0.
#
#  Note that it is generally preferable to use 'wald' on a REML fit than
#  on a ML fit.  'anova' can only be used on a ML fit.
#
#
#  In the following, we use Wald. Using LRT is presented as an exercise
#  We illustrate approaches 4a and 4b:
#
# OUR GOAL: Make the model as simple as possible but not too simple (Einstein)
# Metagoal: Figure out what our goal means.

wald( fit.sm.ac )

# There is 1 4-way interaction which is not sig.
# We can use the summary output to test since it is a single term

# We refit with only up to 3 way interaction

fit.sm.ac3 <- update( fit.sm.ac, . ~ (ses + Minority + cvar(ses,id) + cvar(Minority, id))^3 )
summary(fit.sm.ac3)

# None of the 3-way interactions have very small p-values.
# Using 4a we might test all 3-way:

wald( fit.sm.ac3, ":.*:")      # tough call?

# 4b approach: Target cvar(Minority) or cvar(ses) interactions

wald( fit.sm.ac3, ":car(ses|cvar(ses.*:") # What's happening

?regexpr

wald( fit.sm.ac3,":cvar\\(ses|cvar\\(ses.*:")

# \ is an escape character for regular expressions
# it means: give the next character a meaning
# that is different than usual, i.e. if it is a
# special character, treat it literally. Here
# '(' is a special character in regular expressions,
# it defines sequences of characters to be
# treated as a group.
#
# Why two \s? Because the \ is also an escape
# character in strings: \n mean new line,
# \t mean tab. So \\ means \ !! which is what
# we need for the regular expression.


wald( fit.sm.ac3,":cvar\\(Min")

# Let's drop interactions with cvar(ses)

fit.sm.ac4 <- update( fit.sm.ac, . ~ (ses + Minority + cvar(Minority, id))^3 +cvar(ses,id))
summary( fit.sm.ac4 )

# retest interactions with cvar(Minority)

wald( fit.sm.ac4,":cvar\\(Min")

# We have:

fit.sm.ac5 <- update( fit.sm.ac, . ~ ses * Minority + cvar(Minority,id) +cvar(ses,id))
summary( fit.sm.ac5 )

# we could drop cvar(Minority) but before doing that let's
# just check specific terms that seem plausible

summary( update( fit.sm.ac5, . ~ . + Minority * cvar(Minority, id)))
summary( update( fit.sm.ac5, . ~ . + ses * cvar(ses, id)))
# nothing pops up

# we converge on:

ff <- update( fit.sm.ac , . ~ ses * Minority + cvar(ses,id))

summary( update( ff, . ~ . + ses*cvar(ses,id)))  # ses * cvar(ses,id) does not show up here either

summary( ff )


##'
##' ## Some Level 2 diagnostics ----
##'

#
# The equivalent of residuals at Level 2
#
# BLUPS and random effects
#


# Estimated coefficients at Level 2 (BLUPS)

coef( ff )

# Random effects (~ Level 2 residuals)

ranef( ff )

# Diagostic 1: look at distribution of residuals for anomalies, outliers

zran <- ranef( ff )
names( zran ) <- c("intercept", 'dMinority','dses')

Init3d(family='serif', cex = 1.2)
Plot3d( intercept ~ dMinority + dses, zran)
Ell3d( col='yellow' )
if(interactive) Id3d( pad=1 )

# Plot residuals against other variables:


zran$id <- factor( rownames(zran) )
zranm <- merge( zran , ddu)
some(zranm)

Plot3d( intercept ~ dMinority + dses | Sector, zranm)
#
#  This is highly revealing!! We should include Sector in the model
#  Why didn't we think of that??  Because we wanted to
#    illustrate the power of diagnostics??
#
#  In a non-pedagical analysis, at this point I would stop and
#  so something about 'Sector'
#
#  But it's being turned into an exercise.
#

# Plotting against other variables:
#   Note that dMinority and dses are somewhat redundant so we can
#   drop 1 for plotting:

Plot3d( intercept ~ dMinority + Minority.p | Sector, zranm)
Plot3d( intercept ~ dMinority + ses.m| Sector, zranm)
Plot3d( intercept ~ dMinority + n | Sector, zranm)  # relationship with dMinority! Interpret?

# QUESTION:
#    Explore other possible displays
#    Explore 'dropone'
#

##'
##' ## Visualizing the model ----
##' 

#
#  Visualizing model (hand-made effect plots)
#

summary( ff )

#
# Steps:
#
# 1) Identify Level 1 variables, Level 2 variables, raw and contextual,
#    and cross-level interactions.
#
#    If there are cross-level interactions the Level 1 model will look different
#    for different levels of the interacting Level 2 variables.
#
#    If there are contextual variables, then you may wish to study
#    contextual/compositional effects.
#
# 2) If there are variables or regressoss whose value depends on other rows in the
#    data frame, e.g. cvar(ses,id), poly(x,4), define 'independent' versions
#    and refit the model.

dd$ses.mean <- capply( dd$ses, dd$id, mean )

ff2 <- update( ff, . ~  ses * Minority + ses.mean)

summary(ff)
summary(ff2)   # identical except for variable names

# Step 3:  Find variability in Level 1 variables and in deviation of
#          Level 1 variables from from contextual Level 1 variables:

quantile( dd$ses.mean , c(0,5,10,25,50,75,90,95,100)/100)
# here extremes are of policy interest

quantile( dd$ses - dd$ses.mean,  c(0,5,10,25,50,75,90,95,100)/100)

pred <- expand.grid( ses.mean = quantile( dd$ses.mean, c(5,25,50,75,95)/100),
                     ses.dev = c(-1,1),   # use more if curvilinear
                     Minority = levels(dd$Minority))    # to make sure levels match dd

names(pred$ses.mean)

# Better labels:
pred$ses.m <- reorder( factor(paste("ses:", names( pred$ses.mean), 'ile', sep = "")), pred$ses.mean)

# Level 1 variables
pred$ses <- pred$ses.mean + pred$ses.dev    # create raw Level 1 var for prediction

some( pred )     # we have all the fixed effects variables in the fitted model

pred$mathach <- predict( ff2, newdata = pred, level = 0) # level = 0 is population

xyplot( mathach ~ ses | ses.m, pred, groups = Minority, type = 'l', lwd = 2,
        auto.key = list(columns = 2, lines = T, points = F))

# If there were interactions with  a contextual variable this would be more interesting

# To show contextual/compositional effect:

predc <- expand.grid( ses.dev = c(-1,0, 1),
                      Minority = levels(dd$Minority),
                      ses.mean = c(-.5, .5))    # to make sure levels match dd

predc$ses <- with( predc, ses.mean + ses.dev)

predc$mathach <- predict( ff2, predc, level = 0)

xyplot( mathach ~ ses ,predc, groups = Minority, type = 'b',
        auto.key = T)
# not quite what we want!


xyplot( mathach ~ ses ,predc, groups = Minority:factor(ses.mean), type = 'b',
        auto.key = list( columns = 4),
        panel = panel.superpose,
panel.groups = function(x,y,subscripts,col.symbol,col,...) {
  disp( list(...))
  disp(col)
  panel.lines( x, y,  ... )
  
}
)

# better. Note that ':' between two factors creates the
# 'interaction' factor of all combinations of the two factors
predc$grps <- with( predc, paste( ifelse(Minority=="Yes", "Min: ", "Maj: "), "ses = ", ses.mean, sep = ""))

predc$labs <- as.character( 1:nrow(predc))

gd( col = c('red','blue','red','blue') , lty = c(1,1,2,2), lwd = 2)
xyplot( mathach ~ ses ,predc, groups = grps , type = 'l',
        auto.key = list(columns = 2, lines = T, points = F),
        panel = panel.superpose,
        panel.groups = function(x, y, subscripts, col, cex, ...) {
          panel.lines( x, y,  ... )
          panel.text( x, y, labels = predc$labs[subscripts], col = 'black', cex = 1.2 ,...)
        }
)
##'
##' ## More effect plots ----
##' 

#
#  Various effects can be expressed as differences in the value of predicted
#  mathach on the fitted lines.
#
#  The within-school effect of ses in Minority students is: (6) - (5)
#  where (x) represent the y-value of point (x).
#
#  We can construct an L matrix to give the fitted values at each point

wald( ff2 )
L = rbind(
  #       Int   ses   MinYes   ses.mean   ses:MinYes
  "1" = c( 1 ,  -1.5,   0,      -0.5,           0 ),
  "2" = c( 1 ,  -0.5,   0,      -0.5,           0 ),
  "3" = c( 1 ,   0.5,   0,      -0.5,           0 ),
  
  "4" = c( 1 ,  -1.5,   1,      -0.5,         -1.5),
  "5" = c( 1 ,  -0.5,   1,      -0.5,         -0.5),
  "6" = c( 1 ,   0.5,   1,      -0.5,          0.5),
  
  "7" = c( 1 ,  -0.5,   0,       0.5,           0 ),
  "8" = c( 1 ,   0.5,   0,       0.5,           0 ),
  "9" = c( 1 ,   1.5,   0,       0.5,           0 ),
  
  "10" = c( 1 ,  -0.5,   1,       0.5,         -0.5),
  "11" = c( 1 ,   0.5,   1,       0.5,          0.5),
  "12" = c( 1 ,   1.5,   1,       0.5,          1.5)
)

L
wald( ff2, L )

# we can take difference of these rows to various effects

Llist <- list(
  "Min - Maj gap | ses = " = rbind(
    "-1.5" = c( 0, 0, 1, 0, -1.5),   # (4) - (1)
    "-0.5" = c( 0, 0, 1, 0, -0.5),   # (5) - (2)
    "0.5" = c( 0, 0, 1, 0,  0.5),   # etc.
    "1.5" = c( 0, 0, 1, 0,  1.5)),
  
  "Contextual effect of ses" = rbind(
    "constant" = c( 0, 0, 0, 1, 0)),  # (7) - (2) = (11) - (6) = ...
  
  "Compositional effect of ses" = rbind(
    "Maj" = c( 0, 1, 0, 1, 0),          # (8) - (2)
    "Min" = c( 0, 1, 0, 1, 1))          # (11) - (5)
)
wald( ff2, Llist)


#
# The minority-majority gap
#

# We could put some se lines around the fitted lines but these might not
# be as useful as in the case of lm or glm fits. The reason is that se lines
# around fitted lines bear a predictable relationshiop to the evidence for
# the difference of the two lines in the case of lm or glm fits.
#
# In the case of mixed models, the significance of the difference is not
# necessarily related to the se lines around the predicted value.  It is
# possible for se-intervals to have a large overlap although the DIFFERENCE
# of the two lines is highly significant.  This is because the se-lines
# reflect the variability of each line in the population. If the contrast
# between the two lines is a within-cluster contrast, then it might be
# estimated with high precision although each line has very large variability.
#
# We could construct an L matrix by hand to estimate the gap at many values of
# ses and then use the estimated value and its ses to construct the
# predicted value +/- 2 ses. Some functions are designed to make this easier
# by creating a matrix whose elements are evaluated on a data frame.
#
# We only need ses so let's generate a range:

dgap <- expand.grid( ses = seq(-2,2,.1))
some( dgap )

# note that a row of the L matrix to estimate the gap has the form:
#   c( 0, 0, 1, 0, ses)
# We use Lform  to generate this matrix from a data frame

?Lform

( L <- Lform ( ff2, list( 0, 0, 1, 0, ses), dgap) )  # L is generated from dgap

( ww <- wald( ff2, L ) )      # the wald test object

dwgap <- as.data.frame( ww, se =2 )   # the wald test object is turned into a data frame
some( dwgap )                         # with coef, coefp, coefm corresponding to
# estimated value +/- se x SEs

xyplot( coef + coefp + coefm ~ ses, dwgap, type = 'l')

# modify lines and color:

gd( lty = c(1,2,2), col = 'blue')
xyplot( coef + coefp + coefm ~ ses, dwgap, type = 'l')

# add labels and grid

xyplot( coef + coefp + coefm ~ ses, dwgap, type = 'l', lwd = 2,
        ylab = "Gap in Math Achievement (+/- 2 SEs)",
        xlab = "SES", xlim = c(-2,2),
        scale = list( x = list( at = seq(-2,2,.5))),
        sub = 'Minority minus Majority gap as a function of SES',
        panel = function(x, y, ...) {
          panel.superpose(x,y,...)
          panel.abline(v=seq(-1.5,1.5,.5), col = 'gray')
          panel.abline(h=seq( -6,0, 1), col = 'gray')
        })


##
##  Exercises
##
#
#  1) As we saw, 'Sector' appears to be an important predictor.
#     Consider models using ses and Sector.
#     Aim to estimate the between Sector gap as a function of ses
#     if there is an interaction between Sector and ses
#     Check for and provide for a possible contextual effect of ses.
#     Plot expected math achievement in each sector.
#     Plot the gap with SEs.
#     Consider the possibility that the apparently flatter effect of
#     ses in Catholic school could be due to a non-linear effect of ses.
#     How would you test whether this is a reasonable alternative
#     explanation?
#
#  2) Take the example further by incorporation Sex. Consider the
#     the 'contextual effect' of Sex which is school sex composition.
#     Note that there are three types of schools: Girls, Boys and Coed
#     schools. If you consider an interaction between Sector and
#     school gender composition, you will see that the Public Sector
#     only has Coed schools.
#
#


#
#  Visualize model
#


# Q: How would trajectories differ if 'fit2' or 'fit2c' gives the better fit

anova( fit2, fit2c)    # very slight edge for fit2c

# compare estimates of fixed effects:

wald( fit )
wald( fit2 )
wald( fit2c )

wald( fit, -1 )    # omit intercept
wald( fit2 , -1)
wald( fit2c, -1 )

wald( fit2c, "ses")

wald( fit2c, "Minority")

##'
##' ## Diagnostics ----
##'
##
##  Model Diagnostics
##

# Level 1

plot( fit2c )   # what's different from an OLS plot
plot( fit2c , id ~ resid(.))
plot( fit2c , resid(.) ~ fitted(.) | id, layout = c(7,7))
qqnorm( fit2c )    # method for lme odjects
qq.plot( resid(fit2c) )

qq.plot( resid( fit2c.c, level = 1))   # BLUPS of residuals
qq.plot( resid( fit2c.c, level = 0))   # Residuals from population fit

qqnorm( resid(fit2c))
qqnorm( fit2c, ~ resid(.) | id, strip = F)
qqnorm( fit2c, ~ resid(.) | Minority)



# Level 2

qqnorm( fit2c, ~ ranef(.) | Minority)
qq.plot( resid( fit2c.c, level = 1))   # BLUPS of residuals
qq.plot( resid( fit2c.c, level = 0))   # Residuals from population fit

?plot.ranef.lme
fit2c.r <-  ranef( fit2c, aug = T)
dim(fit2c.r)
some(fit2c.r)
plot( fit2c.r )
getVarCov( fit2c )
cond( getVarCov( fit2c ) )


Init3d()
Plot3d( `(Intercept)` ~ ses + `dvar(Minority, id)`, fit2c.r)
Plot3d( `(Intercept)` ~ ses + `dvar(Minority, id)` | Sector, fit2c.r)
Ell3d()
VarCorr(fit2c)


# The following is equivalent to Hausman's test in economics
# The idea is to test whether the between school effect is the
# same as the within-school (the difference between the two
# is the contextual effect). If not the mixed model will give a
# biased estimate of the within school effect and the Hausman test
# was used to decide between using a mixed model or a fixed effects model.
#
# Now we know better.
#
# We can achieve the same thing -- and more -- by simply including the
# contextual variable in the mixed model.
#
# So Hausman's test has been turned into a modelling decision.
#
# It was used to decide whether to fit a mixed model or a
# fixed effects model since the former, if there is a contextual effect,
# will yield biased estimates of the within group

##  Add contextual variables

system.time(
  fit2c.c <- update(fit2c, . ~ . +cvar(ses,id) + cvar( Minority, id))
)

# to make things much faster:


summary(fit2c.c)

# do we pass Hausman's test:

wald ( fit2c.c , "cvar")   # guess NOT, so what do we do. In the old days we would
# have deemed this a diagnostic to drop mixed models.

wald( fit2c.c , "Minority")
wald( fit2c.c , "ses")

# We'll keep fit2c.c for now, maybe add to it soon.

# Visualize marginal model

dd$ma.pop <- predict( fit2c.c, level = 0)  # population prediction
dd$ma.sch <- predict( fit2c.c, level = 1)  # within school BPLUB

# Note difference from conventional use of 'level':
#
# Bates and Pinheiro labelled the levels the 'WRONG' way:
# level 0 is the population level, level 1 is one down, the cluster level, level 2
#    the subjects.
#

xyplot( ma.pop ~ ses , dd, groups = Minority, auto.key = T)    # based only on fixed effects
xyplot( ma.sch ~ ses , dd, groups = Minority, auto.key = T)   # fixed effects + within school random effect

# Q: why is there not a single line for each minority status in the second plot?

# by school:

xyplot( ma.sch ~ ses | id , dd, groups = Minority, auto.key = T,
        layout = c(9,9), strip = F)


##'
##' ## Refining the model ----
##' 
#

# For pedagogical speed, we'll work with a simpler model

system.time(
  fit2c.c <- update(fit2c, . ~ . +cvar(ses,id) + cvar( Minority, id), random = ~ 1 | id)
)

# what do we do to  possibly build up the model??

# Include interaction with cvar variables ?

fit2c.ci <- update(fit2c.c, . ~ ses * Minority * cvar(ses,id)*cvar(Minority,id))
summary( fit2c.ci )


#
# Paring the model down:
#

# Different Strategies:
#              1) pare down high-order interaction
#              2) see if you can drop entire factors
#

# e.g. Can we drop "Minority" altogether (we already know the answer but let's ask)
# Could refit without minority and use anova -- but would need ML
# or could use wald -- not so bad with REML

wald( fit2c.ci, "Minority")   # what do you conclude?
wald( fit2c.ci, "ses")        # what do you conclude?

# can we drop contextual variables?

wald( fit2c.ci, 'cvar')

# 3 way and higher interactions?

wald( fit2c.ci, ":.*:")    # '.' is any character, '*' is any repeats possibly 0 of previous expression
# what would you decide?

fit2c.ci2 <- update( fit2c, . ~ (ses + Minority + cvar(ses,id)+cvar(Minority,id))^2, random = ~ 1 | id)
summary(fit2c.ci2)

# can we simplify further?
# Look at maximal interactions and look for patterns
# Could we drop cvar(ses interactions?

wald( fit2c.ci2, ":cvar(ses|cvar(ses.*:") # What's happening

?regexpr

wald( fit2c.ci2,":cvar\\(ses|cvar\\(ses.*:")
# \ is an escape character for regular expressions
# it means: give the next character a meaning
# that is different than usual, i.e. if it is a
# special character, treat it literally. Here
# '(' is a special character in regular expressions,
# it defines sequences of characters to be
# treated as a group.
#
# Why two \s? Because the \ is also an escape
# character in strings: \n mean new line,
# \t mean tab. So \\ means \ !! which is what
# we need for the regular expression.

# what does this suggest?

wald( fit2c.ci2 )

# refit the model:

ff <- update( fit2c, . ~ (ses + Minority + cvar(Minority,id))^2+ cvar(ses,id), random = ~ 1 | id)
wald( ff )
wald( ff, "cvar\\(M")
ff2 <- update( fit2c, . ~ ses*Minority + ses*cvar(Minority,id) + cvar(ses,id), random = ~ 1 | id)
wald( ff2 )

ff3 <- update( fit2c, . ~ ses + Minority + ses*cvar(Minority,id) + cvar(ses,id), random = ~ 1 | id)
wald( ff3, -1 )

ff3c <- update( fit2c, . ~ ses + Minority + ses*cvar(Minority,id) + cvar(ses,id))
wald( ff3c, -1 )

##'
##' ## Multilevel R squared ----
##'

#
# Frequent request: what it the R-squared for the model
# Answer: if there's any R2 there's more than 1
#
# When we compare a 'null' model with a 'full' model
# the improvement in fit could occur 'between' clusters
# or 'within' clusters to different extents.
#


ffnull <- update( fit2c, . ~ 1, random = ~ 1|id)

( vfull <- VarCorr( ff3 ) )
( vnull <- VarCorr( ffnull ) )

amr2 <- function( full, null ) {
  # roughly: proportion of variance 'explained'
  vf <- as.numeric(VarCorr ( full )[,1])
  vn <- as.numeric(VarCorr ( null )[,1])
  (vn - vf) / vn
}

amr2( ff3, ffnull) # proportion explained within and between
# Note 1: only really works for random intercept models
#    otherwise proportion explained is different for different
#    values of predictors
# Note 2: Could be negative at either level, e.g. if between 
#    model explains so well that it leaves less for the within 
#    or if the within-model creates intercepts with more variability.
#    Extremes are diagnostic of model violation, e.g. random intercept
#    not independent of X.

#
#  Model selection for 'causal insight' should be guided by good theoretical
#  insights as much as statistical fit
#
#  I'm not excited by this model because the terms retained are not of
#  a clearly different character (to me) than the ones that were dropped
#  It feels like the data drove the selection more than the theory
#
##'
##' ## Exercise ----
##'
# Write a program to implement the methods described in Snijders
# Bosker (2012) chapter 7.




##'
##' ## Visualizing the model and asking sharper questions ----
##'

summary(ff3c)

# see predicted values for different combinations of predictors
#
# Step 0: refit model with data-independent formula that does not
#         depend on ids
# Step 1: work out ranges of Level 2 predictor
# Step 2: work out ranges of Level 1 predictors conditional on Level 2
# Step 3: make a predictor data frame
# Step 4: predict response for predictor data frame
# Step 5: plot predicted response
# Step 6: formulate and test hypotheses
# Step 7: Present results graphically where appropriate
#

# Step 0
# cvar depends on values in each school
dd$ses.m <- with( dd, cvar( ses, id))
dd$Min.prop <- with ( dd, cvar( Minority, id))

fp <- update( ff3c,  mathach ~ ses * Min.prop + Minority + ses.m )
summary(fp)

# Step 1:
#  Level 2 predictors are ses.m, Min.prop
ddu <- up ( dd, ~ school )
head( ddu )
xqplot( ddu )

# To choose 3 'representative values of Min.prop might use .05, .2 and .9
# for ses.m from -1 to 1
quantile( ddu$Min.prop )
quantile( ddu$ses.m )

# Variability at Level 1 given Level 2

xqplot( with( dd, dvar( ses, id)))    # -1 to 1  get about 85%

# Generate between school predictors, within school deviations and predictors

pred <- expand.grid( ses.m = seq(-1, 1, 1), Min.prop = c( .05, .2, .9),
                     ses.d = seq(-1,1,.5 ),
                     Minority = levels( dd$Minority))   # to make sure factors in right order

# generate remaining within school variables

pred$ses <- pred$ses.m + pred$ses.d
# make labels
pred$Min.prop.lab <- factor( as.character( pred $ Min.prop ))

some( pred )

# predict response

pred$mathach <- predict( fp, pred, level = 0) # population level
some( pred )

xyplot( mathach ~ ses | Min.prop.lab * ses.m, pred,
        groups = Minority, type = 'l', auto.key = T,)

dim( pred )
some( pred )

wald( fp )

#
#  EXERCISE:
#     Add SE bands as in Example 1
#

##
## Please send questions or comments to Georges Monette <georges@yorku.ca>
##

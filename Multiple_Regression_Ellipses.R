#' ---
#' title:     "Ellipses in Multiple Regression"
#' subtitle:  "ICPSR Summer Program at York University"
#' date:      "July 17 to 21, 2017"
#' author:    "Georges Monette"
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
knitr::opts_chunk$set(comment="  ", error = TRUE)
# important to run this for rgl functions
if(.Platform$OS.type == 'windows') windowsFonts(Arial=windowsFont("TT Arial")) 
interactive <- FALSE  # do not run interactive code
#'
  
library(spida2)
library(MASS)  # for eqscplot
library(p3d)
data(coffee)
?coffee
head(coffee)
rownames(coffee) <- coffee$Occupation # ERROR: duplicate rownames not allowed

Init3d( family='serif',cex = 1.2)
Plot3d( Heart ~ Stress + Coffee, coffee,
        ylim = c(0,150),  xlim = c(0,180), zlim = c(0,180),
        col = 'blue', theta=-90, phi=0, fov= 0, xlab ='')
##
## Regression: Heart Damage on Coffee consumption ----
##
fitc <- lm (Heart ~ Coffee, coffee)
summary(fitc)
#
# Simple regression: least-squares line minimizes sum
#    of squared residuals:
#
Fit3d(fitc, lwd = 2, residuals = TRUE)
#
# The data ellipse: regression line goes through
#    points of vertical tangency
#
Ell3d()
#
# Having a look at points: Might there be some other
#    explanation than the possibility that coffee
#    consumption is harmful
#
Pop3d(4)
Id3d(labels = coffee$Occupation, pad = 2 ,col = 'blue') 
# To stop selecting points, draw a rectangle over empty region
fg()
#
# Third dimension into the screen:
#
Text3d(z = -5, y = 2, x = 180,
       text = "Stress " ,adj = 1, col='black')
Fit3d(lm(Heart ~ 0, coffee), residuals =T, col = 'gray')
#
# View all variales pairwise:
# - strong pairwise relationship between all pairs of variables
#
# What does Heart vs Coffee look like in 3d?
#
Fit3d(fitc, residuals = T, alpha = .5)
#
# What if we use a model that takes Stress into account
# Multiple regression model with Coffee and Stress:
#
fitm <- lm( Heart ~ Coffee + Stress, coffee)
summary(fitm)
#
# In 3d:
#
Fit3d(fitm, col = 'yellow', col.grid = 'black')
#
# - look down diagonal
# - both models come close to points but multiple regression
#   capture negative slope in direction of Coffee keeping 
#   stress constant. Note that negative slopes with respect
#   to Stress are over lines of constant Stress, i.e. 
#   the partial derivative of E(Heart) with respect to 
#   Coffee keeping Stress constant.
#
# Compare the two models:
#
wald(fitc)
wald(fitm)
#
# Note predictors are correlated
#
Pop3d(5)
Fit3d( fitc, alpha = .3)
Fit3d( fitm, col = 'yellow', col.grid = 'black' , alpha = .3)

Lines3d( zx = (elld <- dell(coffee$Coffee, coffee$Stress, radius = 1:2)),
         y='min', color = 'red', lwd = 2, alpha = 1)
Lines3d( zx = ellptc( elld, c(0,1), radius=10*c(-1,1)), 
         y = 'min', color = 'red', lwd = 2, alpha = 1)
# Looking down the intersection of the blue and yellow planes: what do you see? AVP for Heart vs Stress adjusted for Coffee
#
# Joint confidence ellipsoid for beta_coffee and beta_stress
#
windows()
par( mar =  c(5, 5, 4, 2) + 0.1)
eqscplot( 0,0, xlim = c(-2,2),ylim= c(-1,3), type = 'n',
          xlab = list( ~beta[Coffee], cex =2), ylab=list(~beta[Stress], cex = 2))
abline( h = 0 )
abline( v = 0 )
points( rbind(coef(fitm)[c('Coffee','Stress')]), pch = 16 , col ='red')
lines( cell(fitm), col = 'black', lwd=2)
lines( cem <- cell(fitm, df=1 ), col = 'red', lwd=2)
summary(fitm)
wald(fitm)
confint(fitm)
points( x = coef(fitm)["Coffee"], y=0, pch = 16, col = 'red')
lines( x = rep( coef(fitm)["Coffee"],2), y = c(0,  coef(fitm)["Stress"]), col = 'red')
lines( x = confint(fitm)["Coffee",], y = c(0,0), lwd =3, col = 'red')
# show formulas for ellipses

points( x = 0, y=coef(fitm)["Stress"], pch = 16, col = 'red')
lines( x = c(0,0) , y = confint(fitm)["Stress",], lwd =3, col = 'red')

# but where does the confidence interval for the effect of coffee not controlling 
# for stress come from
points( x =  coef(fitc)["Coffee"], y =0, pch = 16, col = 'blue')
lines( x = confint(fitc)["Coffee",], y = c(0,0), lwd =3, col = 'blue')

points( ellptc( cem , dir= c(1,0), radius = -1), col = 'black', pch = 16)
lines( elltan( cem, dir = c(1,0), radius = -1, len = 2))
lines( elltanc( cem, dir = c(1,0), radius = 0, len = 20))
lines( elltanc( cem, dir = c(1,0), radius = 1, len = 20))
lines( elltanc( cem, dir = c(1,0), radius = -1, len = 20))

# The 'shape' of the CE is deternined by Sigma(X)
# It is scaled by  d x RSE / sqrt(n)
# where d is a quantile from an appropriate distribution
# Its center is at Beta.hat

###
###  Measurement error ----
###

# What happens if we add measurement (or specification) error to stress
Init3d( family='serif',cex = 1.2)
Plot3d( Heart ~ Stress + Coffee, coffee,
        ylim = c(0,150),  xlim = c(0,180), zlim = c(0,180),
        col = 'blue', theta=-90, phi=0, fov= 0, xlab ='')
#
fitm <- lm(Heart ~ Stress + Coffee, coffee)
Fit3d(fitm, col = 'yellow', col.grid = 'black')
sd(coffee$Stress)
zz <- coffee
zz$Stress <- zz$Stress + 10* rnorm( nrow(zz)) # add random error with SD = 10
sd(zz$Stress)
# Validity:
cor(zz$Stress, coffee$Stress)
Fit3d(fit.me <- lm(Heart ~ Coffee + Stress, zz), col = 'red')
lines(cell(fit.me), col = 'red', lwd = 4)

#
# Adding more error: 
#
zz$Stress <- zz$Stress + 10* rnorm( nrow(zz))
cor(coffee$Stress, zz$Stress) # validity
Fit3d( fit.me <- lm( Heart ~ Coffee + Stress, zz), col = 'red')
lines( cell( fit.me), col = 'red', lwd = 4)
#
# Adding yet more error: 
#
zz$Stress <- zz$Stress + 10* rnorm( nrow(zz)) # add random error with SD = 10
cor(coffee$Stress, zz$Stress) # validity
Fit3d(fit.me <- lm(Heart ~ Coffee + Stress, zz), col = 'red')
lines(cell(fit.me), col = 'red', lwd = 4)
#'
#' Pretty soon, adding more error to the **confounding** variable
#' results in model that is almost the same as using no control
#' for confounding
#' 
Fit3d(fitc, alpha = .3)
#'
#' Summary:
#' 
#' Measurement error in a confounder biases the estimate of
#' the target coefficient towards its marginal (uncontrolled) value.
#' 
#' Measurement error in a target variable biases its coefficient
#' towards 0 (attenuation or dilution)
#' 
#'
###
###  Effect of outliers on regression ----
###
Init3d(family = 'serif', cex = 1.2)
hw <- subset(hwoutliers, Outlier == 'none')
Plot3d( Health ~ Height + Weight, hw, 
        col = 'blue', theta = -90, phi = 0, fov = 0)
pos(); fg()
Fit3d( fitw <- lm( Health ~ Weight,  hw),  col = 'blue', residuals = T, lwd = 2)
summary(fitw)
#
# Weight is not significant! No evidence of a problem
#
# Do classical diagnostics: what do they show?
#
# Residual vs X -- same as residuals versus yhat
# Residuals vs omitted variable ?
# Keep turning until AVP: Added-variable plot
# Reference: Fox 2015 Applied Regression Analysis
#

Lines3d( xz = elld <- dell( hw$Height, hw$Weight, radius = c(1,2)), 
         y = 'min', color = 'red')
Lines3d( xz = ellptc( elld, dir= c(1,0), radius = c(-10,10) ), 
         y = 'min', color = 'red') # Regression of height on weight

#
# If regression line for the regression of height on weight is vertical then
#    horizontal displacement is proportional to residual of height on weight
#    vertical blue residual is proportion to residual of health on weight
#    SO we have the AVP for health on height adjusting for weight
#

Fit3d( fithw <- lm( Health ~ Height + Weight , hw), 
       col = 'yellow', col.grid = 'black')

# The AVP is a 2-dimensional view of the effect of a variable in a high-dimensional regression

summary( fithw )
#
# Note that BOTH variables are highly significant in the multiple
# regression ALTHOUGH NEITHER is significant in simple regressions
#
windows()
par( mar =  c(5, 5, 4, 2) + 0.1)
eqscplot( 0,0, xlim = c(-2,2),ylim= c(-1,3), type = 'n',
          xlab = list( ~beta[Weight], cex =2), ylab=list(~beta[Height], cex = 2))
abline( h = 0 )
abline( v = 0 )
points( rbind(coef(fithw)[c('Weight','Height')]), pch = 16 , col ='red')
lines( cell(fithw)[,2:1], col = 'black', lwd=2)
lines( cem <- cell(fithw, df=1 )[,2:1], col = 'red', lwd=2)
summary(fithw)
wald(fithw)
confint(fithw)
points( x = coef(fithw)["Weight"], y=0, pch = 16, col = 'red')
lines( x = rep( coef(fithw)["Weight"],2), y = c(0,  coef(fithw)["Height"]), col = 'red')
lines( x = confint(fithw)["Weight",], y = c(0,0), lwd =3, col = 'red')

points( x =  coef(fitw)["Weight"], y =0, pch = 16, col = 'blue')
lines( x = confint(fitw)["Weight",], y = c(0,0), lwd =3, col = 'blue')

##
##  Effect of different types of outliers
##
#
#               Predictors         Fit
#    Type 1:    typical value      bad fit
#    Type 2:    atypical value     good fit
#    Type 3:    atypical value     bad fit
#
hwoutliers
Init3d( family = 'serif', cex = 1.2 )
Plot3d( Health ~ Height + Weight | Outlier , hwoutliers)

Id3d(labels = hwoutliers$Outlier,pad = 2)
fg()

##
## NOTE:
##   Leverage is distance in predictor space relative to 
##     predictor data ellipsoid: view data from
##     above and look at Mahalanobis distance
##     -- contrast with Euclidean distance
##

hw0 <- subset( hwoutliers, Outlier == "none")
hw1 <- subset( hwoutliers, Outlier %in% c("none","Type 1"))
hw2 <- subset( hwoutliers, Outlier %in% c("none","Type 2"))
hw3 <- subset( hwoutliers, Outlier %in% c("none","Type 3"))

with ( hw0 , Lines3d( xz = dell( Height, Weight), y = 'min', color = 'blue', lwd = 2))
with ( hw1 , Lines3d( xz = dell( Height, Weight), y = 'min', color = 'green', lwd = 2))
with ( hw2 , Lines3d( xz = dell( Height, Weight), y = 'min', color = 'orange', lwd = 2))
with ( hw3 , Lines3d( xz = dell( Height, Weight), y = 'min', color = 'magenta', lwd = 3))

Fit3d( fit0 <- lm( Health ~ Weight + Height, hw0 ) , col = 'blue', alpha=.2)
Fit3d( fit1 <- lm( Health ~ Weight + Height, hw1 ) , col = 'green', alpha=.2)
Fit3d( fit2 <- lm( Health ~ Weight + Height, hw2 ) , col = 'orange', alpha=.2)
Fit3d( fit3 <- lm( Health ~ Weight + Height, hw3 ) , col = 'magenta', alpha=.4)

par( mar =  c(5, 5, 4, 2) + 0.1)
eqscplot( 0,0, xlim = c(-2,2),ylim= c(-2,2), type = 'n',
          xlab = list( ~beta[Weight], cex =2), ylab=list(~beta[Height], cex = 2))
abline( h = 0 )
abline( v = 0 )

points( rbind(coef(fit0)[c('Weight','Height')]), pch = 16 , col ='blue')
lines( cell(fit0, df=1 ), col = 'blue', lwd=2)

points( rbind(coef(fit1)[c('Weight','Height')]), pch = 16 , col ='green')
lines( cell(fit1, df=1 ), col = 'green', lwd=2)

points( rbind(coef(fit2)[c('Weight','Height')]), pch = 16 , col ='orange')
lines( cell(fit2, df=1 ), col = 'orange', lwd=2)

points( rbind(coef(fit3)[c('Weight','Height')]), pch = 16 , col ='magenta')
lines( cell(fit3, df=1 ), col = 'magenta', lwd=2)

# Although only Type 3 outliers have a large influence on beta.hat
# Type 1 and Type 2 outliers have influence on inference

# Looking at diagnostic plots for each regression:
plot(fit0, which = 5)
plot(fit1, which = 5)
plot(fit2, which = 5)
plot(fit3, which = 5)
#
# Summary:
#
# Type    Discrepancy    Eff on Beta hat   Effect on SE  Effect on sig   E
# -----   -----------    ---------------   ------------  -------------
# 1       fit, not X     small             larger        lower
# 2       X, not fit     small             smaller       higher
# 3       both X & fit   large             ??            ??
#

#' ---
#' title:      "How Harmful is Smoking?"
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
knitr::opts_chunk$set(comment="  ", error = TRUE)
if(.Platform$OS.type == 'windows') windowsFonts(Arial=windowsFont("TT Arial")) 
interactive <- FALSE  # do not run interactive code
#'
#' Get the lastest versions:
#'
devtools::install_github('gmonette/p3d')
devtools::install_github('gmonette/spida2')

library(p3d)  
library(spida2) 

data(Smoking2)
dd <- Smoking2
head(dd)
dim(dd)

dd$Cigarettes <- dd$Smoking/365.25
dd$Life <- dd$LE
dd$Health_Exp <- dd$HE

Init3d(cex=1)

Plot3d( Life ~ Cigarettes + Health_Exp | Continent, dd)
spinto()
Axes3d()
fg()

fit <- lm( Life ~ Cigarettes, dd)
summary(fit)
wald(fit)

# What does the regression look like?

Fit3d(fit, lwd = 3)

# But the relationship is not really linear

fitsq <- lm( Life ~ Cigarettes+I(Cigarettes^2), dd)
summary(fitsq)
Fit3d(fitsq, lwd = 3, col = 'red')
Pop3d(4)

# choose points to identify - drag mouse square around a point
Id3d(pad=1)
# to escape from this mode, drag a square around empty space

fg()

# "Controlling" for Health $$

spinto(-90)

fit1 <- lm( Life ~ Cigarettes, dd)
summary(fit1)
Fit3d(fit1, alpha = .5)

fitlin <-  lm( Life ~ Cigarettes 
               + Health_Exp 
               ,dd)
Fit3d(fitlin, col = 'cyan')
#
# Controlled for Health $$ ?
# Have unconfounded the effect of Smoking??
# Hardly.
#
fith <- lm( Life ~ Cigarettes 
      + Health_Exp 
      + log( Health_Exp),dd)

summary(fith)
Fit3d(fith, col = 'red')
#
# Apparently: 
#
# There is less confounding as we improve
# the model for the potential confounder, Health_Exp,
# but we, presumably, are far from having
# found a reasonable model to estimate
# the causal effect of smoking: individually or
# ecologically.
#
# EXERCISE: See what happens when you control for other factors,
# e.g. Continent
#

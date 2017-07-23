#' ---
#' title:     "Lab 2 Longitudinal Models"
#' subtitle:  "ICPSR Summer Program at York University"
#' date:      "July 17 to 21, 2017"
#' author:    "Georges Monette"
#' output:
#'   html_document:
#'     toc: false
#'     toc_float: false
#'     theme: united
#'     fig.width: 12
#'     fig.height: 10
#' bibliography: mm.bib
#' link-citations: yes
#' ---
#' (Updated: `r format(Sys.time(), '%B %d %Y %H:%M')`)
#' 
#+ include=FALSE
knitr::opts_chunk$set(comment="|  ", error = TRUE)
#' Contents:
#' 
#' 1. LME model
#' 2. Hausman test:
#' 3. Adjusting for time
#' 4. Diagnostics: Level 1
#' a) Diagnostics for heteroskedasticity
#' b) Diagnostics for autocorrelation
#' 5. Diagnostics: Level 2
#' 6. Dropping observations
#' 7. Modeling autocorrelation
#' 8. Modeling heteroskedasticity
#' 9. Interpreting different kinds of residual plots
#' 10. Visualizing the impact of model selection
#' 11. Displaying data and fitted values together
#
#  Schizophrenia patients were assessed at 6 annual checkups.
#
#  At each checkup, the type of drug currently prescribed by the treating
#  physician was recorded.  There were 3 types of drugs: Typical, Atypical and
#  Clozapine.
#
#  Researchers are interested in using this observational data to study the the
#  effectiveness of the newest drug, Clozapine, particularly comparing it with
#  'Typical' drugs, the established standard.
#
#  There are a number of 'outcome' measures (i.e. measures of the
#  severity of the illness) but only one, 'neg', is of interest
#  for now.
#
# The punchline or the sites to see: it looks like clozapine causes more 
# neg symptoms compared to typical treatment, but that could be due to
# clozapine being prescribed more when symptoms worsen (reverse causation)
# Within patients, patients serve as their own control IF no confounders
# change over time. 
# What about the contextual effect of prescription over time?
# Mathematically, including the contextual effect "purifies" the 
# within-patient effect - to represent only the within-patient effect - 
# regardless of the substantive meaning of the contextual effect. I was 
# concerned that the contextual effect is equivalent to adjusting the 
# effect at earlier timepoints for something that hasn't happened yet with 
# the same possible problems of adjusting for mediators. Monette is pretty 
# sure that isn't a problem. Is it like estimating a latent variable that
# we assume is constant over the observation time within each person?


library(spida2)   # devtools::install_github('gmonette/spida2')
library(p3d)      # devtools::install_github('gmonette/p3d')
library(car)
library(lattice)
library(latticeExtra)
interactive <- FALSE

?Drugs
some( Drugs )

# Which drug seems best to reduce 'neg' symptoms

xyplot( neg ~  year | Subj, Drugs)

dd <- Drugs

dd$id <- reorder( dd$Subj, dd$neg)  # orders 'id's in order of average 'neg' symptoms
# choose nice color
pals('green')
pals('red')
display.brewer.all()
cols <- brewer.pal(3,'Dark2')
cols
pal(cols)
gd(3, col = cols) # uses ggplot2-like style, 3 is number of colors

xyplot( neg ~ year | id, dd , groups = drug, auto.key = list(columns=3))

#
# QUESTION:
#
#        Can we perform OLS fits on each cluster?
#        Note that the data are balanced with respect to time
#        but not with respect to drugs.
#
#        Also note that Clozapine is more frequently given later in the study
#

# compare drugs:

# Pooling the data

fit.lm <- lm(neg ~ drug, dd)
summary(fit.lm)
Ld <- Ldiff(fit.lm, 'drug')
wald(fit.lm, Ld)
Ld <- Ldiff(fit.lm, 'drug', ref = "Atypical")
wald(fit.lm, Ld)

# Which looks better: Clozapine or Typical? Remember that a higher response means 
# more severe symptoms.

#
# LME model
#

library(nlme)

fit <- lme( neg ~ drug, dd, random = ~ 1 | id )
summary(fit)
wald(fit, -1)
# Note: overall significance although individual p-values were not

# Now, Clozapine looks best

Ld <- Ldiff ( fit, "drug")   # hypothesis matrix to test differences between drugs
Ld
wald( fit, Ld )

Ld <- Ldiff ( fit,  "drug", ref = "Atypical")
wald( fit, Ld )

# Other ways


# QUESTIONS:
#       1) Can you explain why 'drug' is significant overall although
#          neither p-value is significant in the summary output. Note that
#          this illustrates the danger in dropping multiple terms on the
#          basis of individual p-values.
#
#       2) How is clozapine doing ?
#          Is it better than the others as expected?
#          What explanation can you think of for the results?
#

#
# Hausman test: test whether contextual variables have a significant effect.
#

fit2 <- update( fit, . ~ . + cvar( drug, id))
summary( fit2 )
wald( fit2, 'cvar')

#
# QUESTIONS:
#   1) What is the interpretation of the contextual variable for a
#      categorical effect:

head( cbind( dd['id'], getX( fit2) ), 18 )

#
#   2) Compare results with and without cvar's
#
#   3) Compare cvar effect with corresponding raw var: what does this suggest??
#      E.g. 'Typical' and cvar(Typical)?
#

wald( fit2, -1)
wald( fit , -1)

Ld <- Ldiff( fit2, "drug", ref = "Atypical")

wald( fit2, Ld )  # compare Clozapine with Typical

#
# QUESTIONS:
#
#  1) Would it ever be appropriate to include a contextual variable even
#        though it isn't significant?  e.g. In this case if the p-value were
#        0.06, what would be the consequence of including it or excluding it?
#
#  2) Compare the SEs for the raw variables and for the contextual variable.
#        What could explain the difference?
#
# === EXERCISE: ===
#
#  1) Estimate the between-patient differences (compositional effects)
#     in these three drugs. Note that there are at least two ways of
#     doing this.
#
#  2) Carry out a similar analysis for general symptoms: 'gen'
#

#
# Taking time into account
#
# When the range of time is compact and similar for all subjects
# and when time is not expected to have a very different effect
# as time progresses, simple models for time are generally adequate
#
# In the next session, we will see much richer functions of
# time: splines, asymptotic models, Fourier analysis, etc.
#

fit2l <- update(fit2, . ~ . + year)
summary( fit2l )
ww <-wald( fit2l )
wald( fit2l, 'cvar' )
wald( fit2l, 'drug' )
Ld <- Ldiff( fit2l, "drug", ref = "Atypical")
wald ( fit2l, Ld )    # What does this say about Clozapine ?

# Q: How do you explain the differences in the estimation of the Typical - Clozapine
#    comparison in the 4 analyses:
#
lapply(
  list("pooled" = fit.lm, "no ctx" = fit,"ctx"= fit2,"ctx+year" = fit2l),
  function( fit ) wald( fit , Ldiff( fit,'drug', ref = "Atypical"))
)
clist <- lapply(
  list("pooled" = fit.lm, "no ctx" = fit,"ctx"= fit2,"ctx+year" = fit2l),
  function( fit ) wald( fit , Ldiff( fit,'drug', ref = "Atypical"))
)
clist
do.call( rbind, lapply(clist, function(x) x[[1]][[2]][3,]))
#
# Can you think of explanations consistent with these results?
#

#
#  EXERCISE:
#     Is there evidence that there is curvature in the effect of time?
#

#
# Level 1 and Level 2 diagnostics
#

#
# Level 1
#

plot( fit2l )
plot( fit2l, id = .02)
plot( fit2l, resid(.) ~ fitted(.) | drug, id = .02)
plot( fit2l, resid(.) ~ fitted(.) | drug * Sex, id = .02)

#
# Diagnostic for heteroskedasticity
#

plot( fit2l, sqrt( abs( resid(.))) ~ fitted(.), id = .02) # exploratory version

# fancy version
plot( fit2l, sqrt( abs( resid(.))) ~ fitted(.), id = .02, sub = "scale-location plot",
      ylab = expression( sqrt(abs(" resid "))),
      panel = function(x, y, ...){
        panel.xyplot( x, y, ...)
        panel.loess(x ,y, ...)
      })
# id didn't work so we do it manually
#
if(interactive) {
  trellis.focus()            # trellis = lattice ?!? (the archaeology should be less visible)
  panel.identify(labels = dd$id)
  trellis.unfocus()
}
# no strong pattern here so we wait until later to see what to do about it.


qqnorm( fit2l, id = .05 )
qqnorm( fit2l, ~ resid(.) | drug, id = .05 )

# We note 'M47', 'F36', 'F41' as suspicious cases

xyplot( neg ~ year | id, dd, groups = drug, auto.key = T)   # not clear for lecture

show.settings()
gd( pch = 15:17, col = c('blue','green4','red'))
show.settings()

xyplot( neg ~ year | id, dd, groups = drug, auto.key = T) # look for M47, F36 and F41

#
#  Variogram: diagnostics for autocorrelation
#  New level 1 diagnostic for longitudinal data
#

vv <- Variogram( fit2l , form = ~ year | id, maxdist = .7)
vv         # variance of differences between pairs as a function of distance in time
str(vv)
plot(vv)
xyplot(variog ~ dist, vv)
# Shows that differences between pairs of residuals that are close have smaller
# variance than between those that are far apart

#
#  Level 2 diagnostics
#

ranef(fit2l)   # Level 2 residuals BLUPS for u_is
class(ranef(fit2l))
methods(class = 'ranef.lme')
?plot.ranef.lme
if(interactive) {
  plot( ranef( fit2l, aug = T ), form =  ~ sex) # ERROR
}
# note ranef( fit2l, aug = T) has a 'bug' and doesn't work if there's a matrix in data frame

# To plot BLUPS
RE <- cbind( ranef( fit2l), up( dd, ~ id))
head( RE )
xyplot( `(Intercept)` ~ age1 | Sex , RE)
qqnorm( RE[[1]])

#
# Acting on diagnostics
# Should we drop some observations
#

fit2l.d <- update( fit2l, data = subset(dd, !(id %in% c("M47","F36","F41")) ))

# or

fit2l.d <- update( fit2l, subset = !(id %in% c("M47","F36","F41")) )
summary (fit2l.d)

# Note: a problem with this approach: a cluster or observation could have high
# influence even though it does not have a large residual. So we could miss
# some influential points.
#
# There was a package 'influence.ME' that was being developed to produce
# influence diagnostics for 'lme' object but the version eventually released
# on CRAN only work for 'lmer' objects.
#
#
#
# EXERCISE:
#     Compare results with fit2l.d and fit2l. Any substantial differences?

# In the following analysis, we keep the original data but this does not
# imply that this is the best course. If some outliers prove to have
# uncorrectable measurement errors, or if they are not in scope (some other
# diagnosis) then it would be appropriate to drop them.
#

#
#  Incorporation of possible autocorrelation
#

#
#  Add a correlation structure to the R side.
#  Most common:
#      corAR1:    autoregressive of order 1 for evenly spaced data
#      corCAR1:   continuous autoregressive of order 1 for
#                 unevenly spaced data
#      corARMA:   autoregressive moving average of variable order
#      -- other classes are mainly for spatial correlation
#      -- you can write your own but it's challenging
#  See: ?corClasses for a complete list
#

# Auto-regressive process of order 1

fit2lc <- update( fit2l, correlation = corAR1( form = ~ year | id))
summary( fit2lc)

# do we need the autocorrelation?

intervals( fit2lc )   # look at CI for Phi (correlation)

# or

anova( fit2l, fit2lc ) # this test can be ok because there is no boundary at 0 for Phi
# but there is when using corCAR1.
plot( simulate( fit2l, nsim = 1000, m2 = fit2lc))  # out of luck with fancier model
wald( fit2l )
wald( fit2lc )

Ld

wald( fit2l, Ld )
wald( fit2lc, Ld )

#+ c377
# QUESTION: When would you expect results to be affected by
#        including AR in the model?
#
#    Note: AR (with positive autocorrelation) implies we expect obs.
#    close in time to be closer than expected under the assumption
#    of independence and observations that are far in time
#    to be farther.
#
#    Therefore a treatment difference between
#    adjoining times gets more weight
#    than one between distant times.
#
#    That explains why including autocorrelation can change estimated
#    differences and p-values.
#

#
#  EXERCISE:
#    Look at diagnostics for models predicting 'gen' instead of 'neg'
#
#

#
# Heteroscedasticiy revisited
#

fitg <- lme( gen ~ drug + cvar(drug,id) , dd, random = ~ 1 | id)
summary( fitg )

fitgl <- lme( gen ~ drug + cvar(drug,id) + year, dd, random = ~ 1 | id)
summary( fitgl )

wald( fitgl, Ld)

# ... many diagnostics later ....
# Note: I believe there is an error in documentation and the default
# for resid is type = 'r' for 'raw' or 'response'. We prefer the
# standardized, 'pearson' residuals here.

plot( fitgl, sqrt( abs( resid(., type = 'p'))) ~ fitted(.), id = .03)

plot( fitgl, sqrt( abs( resid(., type = 'p'))) ~ fitted(.), id = .03,
      panel = function(x, y, ...) {
        panel.xyplot( x, y, ...)
        panel.loess( x, y,...)
      })

plot( fitgl, sqrt( abs( resid(., type = 'p'))) ~ fitted(.) | Sex, id = .03,
      panel = function(x, y, ...) {
        panel.xyplot( x, y, ...)
        panel.loess( x, y,...)
      })

plot( fitgl, sqrt( abs( resid(., type = 'p'))) ~ fitted(.) | drug, id = .03,
      panel = function(x, y, ...) {
        panel.xyplot( x, y, ...)
        panel.loess( x, y,...)
      })
#
# Perhaps: increased variability with increase in predicted value
#    In practice, I wouldn't be too disturbed by this plot but
#    for pedagogical purposes, let's see what we could do to address this.
#

#
# Accounting for heteroskedasticity
#


#
#  We assume that the SD of residuals changes with the fitted value (variance covariate)
#
#  Common assumptions are that:
#     1) variance is proportional to an unknown power of the variance covariate
#         or
#     2) a unknown constant plus an unknown power of the covariate
#

# To see the full set of methods, available (you can also create your own):

?varClasses

# Here we illustrate varConstPower

fitglh <- update( fitgl, weights = varConstPower( form =  ~fitted(.)| drug) )

summary(fitglh)

anova( fitgl, fitglh )

plot( fitglh, sqrt( abs( resid(.,type = 'p'))) ~ fitted(.) | drug, id = .03,
      panel = function(x, y, ...) {
        panel.xyplot( x, y, ...)
        panel.loess( x, y,...)
      })
# note that 'pearson residuals' are plotted so that
# high variance residuals will be shrunk.
# with raw residuals we expect heteroskedasticity to look
# even worse because the high variance observations have
# less influence on the fit
# raw residuals:
plot( fitglh, sqrt( abs( resid(., type = 'r'))) ~ fitted(.) | drug, id = .03,
      panel = function(x, y, ...) {
        panel.xyplot( x, y, ...)
        panel.loess( x, y,...)
      })


plot( fitgl, sqrt( abs( resid(., type = 'r'))) ~ fitted(.) | drug, id = .03,
      panel = function(x, y, ...) {
        panel.xyplot( x, y, ...)
        panel.loess( x, y,...)
      })

wald( fitgl, Ld)
wald( fitglh, Ld)

#
# Note that the impact of accounting for heteroskedasticity is not negligible.
#
# As usual, seeking higher efficiency has an impact on what we're estimating.
# Differences between drugs in patients at lower levels of 'gen' receive
# more weight than those at higher levels. The estimate has lower variance
# and is more precise but it might estimate something slightly different if
# there are differences in drug effects at different levels of severity.
# If this is an important question to address we could reformulate the model
# to attempt to take it into account although we might not have much power
# to detect such an effect.
#

# EXERCISE:
#    Refit 'fitglh' and 'fitgl' dropping a few residual outliers.
#    Retest the difference between the two models.
#    Does accounting for heteroskedastiticy still improve the fit?
#    Might it be that heteroskedasticity was just accounting for a
#        few outliers instead of capturing a general phenomenon?
#

#  EXERCISE:
#    Enlarge the RE model. How far can you go and what is the impact
#    of doing so.

#
#  Visualizing different models
#

#
# The following code was used to generate graphs for a lecture
# It shows how the different models above fit the data and
# attemps to explain how and why they differ.
#

gd( pch = 15:17, col = c('blue','green4','red'))

xyplot( neg ~ year | Subj, dd, groups = drug )

dd$id <- dd$Subj
dd$id <- reorder( dd$id, 1000*(dd$Sex == "M") + dd$neg )

gd( pch = 15:17, col = c('blue','green4','red'))
xyplot( neg ~ year | id, dd, groups = drug,
        between = list( y = c(0,.5,0,0,0,0,0)) ,
        skip = c( rep(F,15), T, rep(F,30)),
        layout = c( 8,7),
        auto.key = list( columns = 3))

# select a 'representative' sample of 3 from each sex

sel <- c( "F4","F25","F45","M12","M40","M51")

gd( pch = 15:17, col = c('blue','green4','red'), cex = 1.2)
xyplot( neg ~ year | id, dd, groups = drug,
        between = list( y = c(0,.5,0,0,0,0,0)) ,
        skip = c( rep(F,15), T, rep(F,30)),
        layout = c( 3,2),
        auto.key = list( columns = 3),
        subset = id %in% sel)

fit.ols <- lm( neg ~ drug, dd )
summary( fit.ols )


L <- list(
  'predicted' = cbind( 1, contrasts( dd$drug)),
  'differences' = Ldiff ( fit.ols, 'drug', ref = 'Atypical'))
L
wald( fit.ols, L)
# Clozapine is worst but not significantly so

#
#  Displaying data and fitted values
#

pred <- expand.grid( year = 1:6, drug = levels( dd$drug), id = levels(dd$id))
head( pred )
some( pred )

pred$neg <- predict( fit.ols, newdata = pred )

# Combine with data for plotting

dd$what <- factor("data")
pred$what <- factor("fit")

gd( pch = 15:17, col = c('blue','green4','red'), cex = 1.2,
    lty = c(1,1,1,1,1,1,2,2,2), lwd = 2)
xyplot( neg ~ year | id, Rbind( dd, pred),  groups = what:drug,
        panel = panel.superpose.2,
        type = c('p','p','p','l','l','l'),
        subset = id %in% sel,
        between = list( y =1 ),
        auto.key = list( text = c("Atypical","Clozapine","Typical"),
                         points = T, lines = T, columns = 3))
# overplotting problem
#
# make later lines less dense:

gd( pch = 15:17, col = c('blue','green4','red'), cex = 1.2,
    lty = c(1,2,3,1,2,3,1,2,3),lwd = 2)
xyplot( neg ~ year | id, Rbind(dd, pred),  groups = what:drug,
        panel = panel.superpose.2,
        type = c('p','p','p','l','l','l'),
        subset = id %in% sel,
        between = list( y =1 ),
        auto.key = list( text = c("Atypical","Clozapine","Typical"),
                         points = T, lines = T, columns = 3))


#
# Mixed model: first version
#

fit.mix <- lme( neg ~ drug, dd, random = ~ 1 | id )
summary(fit.mix)
wald( fit.mix, L )  # same L as above because same FE model
# Clozapine is best and sig. diff from Typical

#
# Can use same pred for Popn pred and for BLUPS
#

pred$neg <- predict( fit.mix, pred, level = 0)

# Make a separate data frame for blupss

pred2 <- pred
pred2$neg <- predict( fit.mix, pred2, level = 1)
pred2$what <- factor("fit2")

# Cut and paste from last, then add 'pred2'

gd( pch = 15:17, col = c('blue','green4','red'), cex = 1.2,
    lty = c(1,1,1,2,2,2,1,1,1),lwd = 2)
xyplot( neg ~ year | id, Rbind( dd, pred, pred2),  groups = what:drug,
        panel = panel.superpose.2,
        lwd = 2,
        type = c(rep('p',3),rep('l',6)),
        subset = id %in% sel,
        between = list( y =1 ),
        auto.key = list( text = c("Atypical","Clozapine","Typical"),
                         points = T, lines = T, columns = 3))

#
# Hausman test and mixed model with contextual categorical variable
#

dd$drug.m <- cvar( dd$drug, dd$id)          # note that drug.m is matrix
dd$drug.m
head( dd, 18)
some( dd )

# QUESTION:
#  What is the interpretation of drug.m?
#  Why not three columns, one for each drug?

fit.m <- lme( neg ~ drug + drug.m , dd, random = ~ 1 | id )
summary( fit.m )

wald( fit.m , 'drug.m')   # Should we keep drug.m?
wald( fit.m , 'drug')

L
Lm <- lapply( L, function(x) cbind( x, 0, 0))
Lm
wald( fit.m , Lm )

# compare with

wald( fit.mix, L)

#
# QUESTION:
#    With drug.m in the model, the difference between Typical and Clozapine
#    is larger and more significant. Why?
#

#
#  Plotting fit with Level 2 variables (here the contextual variables)
#

#
#  If you want to show how the response depends on FE variables
#  you need to create a prediction data frame with all the FE variables
#  needed for prediction. (Level 1 or Level 2)
#
#
#  If you want to show predictions for individual clusters ( popn average
#  of BLUPS)
#           AND
#  you have Level 2 variables in your model
#  you need to construct a data frame with
#      ids and Level 1 variables
#  then you need to merge the data.frame with the matching
#  Level 2 variables.
#
#

pred <- expand.grid( year = 1:6, drug = levels( dd$drug), id = levels(dd$id))
head( pred)
dim( pred )
# merge pred with specific Level 2 variable corresponding to id

ddu <- up( dd, ~ id)
ddu

predc <- Rbind( pred, ddu)
dim( predc )
head( predc )
names( predc ) # note that drug.m matrix is preserved as a matrix

# ready to predict popn average:

predc$neg <- predict( fit.m , predc, level = 0)

# predict BLUP

predc.blup <- predc
predc.blup$neg <- predict( fit.m , predc.blup, level = 1)

# Identify data frames so they can be combined and each plot differently

dd$what <- factor("data")
predc$what <- factor("fit0")
predc.blup$what <- factor('fit1')
comb <- Rbind(dd, predc, predc.blup)
# Note: this is all cut and paste except for the first line

gd( pch = 15:17, col = c('blue','green4','red'), cex = 1.2,
    lty = c(1,1,1,2,2,2,1,1,1),lwd = 2)
xyplot( neg ~ year | id , comb,  groups = what:drug,
        panel = panel.superpose.2,
        lwd = 2,
        type = c(rep('p',3),rep('l',6)),
        subset = id %in% sel,
        between = list( y = 1 ),
        #auto.key = list( text = c("Atypical","Clozapine","Typical"),
        auto.key = list(
          points = T, lines = T, columns = 3))


#
#  Taking time into account
#

fit.my <- update( fit.m, . ~ . + year)

summary( fit.my )   # very significant drop with time

# Previous hypothesis matrix

Lm
wald( fit.my )

# We only need to add year to Lm

Lmy <- Lm

Lmy <- lapply( Lm , function( x ) cbind(x, 0) )

# let's use the average year for predicted values

Lmy[[1]][,6] <- 3.5

Lmy

#
# QUESTION: Should we do the same thing for the second matrix?
#
#

wald ( fit.my, Lmy )

#
# QUESTION: What does this say about Clozapine?
#

# We don't need new pred data.frames

predc$neg <- predict( fit.my, predc, level = 0)
predc.blup$neg <- predict( fit.my, predc.blup, level = 1)

dd$what
predc$what <- factor( 'fit0')
predc.blup$what <- factor( 'fit1')
rb <- Rbind( dd, predc, predc.blup)
# Note: this is identical to previous
xyplot( neg ~ year | id , Rbind( dd, predc, predc.blup ),  groups = what:drug,
        panel = panel.superpose.2,
        lwd = 2,
        type = c(rep('p',3),rep('l',6)),
        subset = id %in% sel,
        between = list( y =1 ),
        auto.key = list( text = c("Atypical","Clozapine","Typical"),
                         points = T, lines = T, columns = 3))


#
# Adding a possible autocorrelation
#

fit.myc <- update( fit.my, correlation = corAR1( form = ~ year | id ))

summary( fit.myc )   # very significant drop with time

wald( fit.myc, 'drug.m')   # what happened to the contextual effect? Why?

# Hypothesis matrix does not change

wald ( fit.myc, Lmy )

#
#  Adding random Drug effects
#

fit.mycr <- update( fit.myc, random = ~ 1 + dvar( drug, id) | id)

fit.mycr <- update( fit.myc, random = ~ 1 + dvar( drug, id) | id,
                    control = list( msMaxIter = 200, msVerbose = TRUE   ))

summary( fit.mycr )

summary( fit.myc )   # very significant drop with time
intervals( fit.myc ) # Beware of interpreting intervals for SDs
wald( fit.myc, 'drug.m')   # what happened to the contextual effect? Why?

# Hypothesis matrix does not change

wald ( fit.mycr, Lmy )

getVarCov( fit.mycr)
cond( getVarCov( fit.mycr) )

coef( fit.mycr )
zz <- ranef( fit.mycr )
names(zz) <- c("Int",'Cloz', "Typical")
names(zz)
ifn(interactive) {
  library( p3d )
  Init3d()
  Plot3d(Int ~ Cloz +Typical , zz)
  Id3d(pad = 2)
  Ell3d()
}
#
# some of the potential RE variance is accounted for by within-cluster
# variance.  This is analogous to having an F at or below 1 for blocks
# in a blocked randomized design.
#

fit.mycrd <- update( fit.mycr, subset = !(id %in% c("F41", "F55", "F36", "M53")))
summary(fit.mycrd)
wald( fit.mycrd, "drug")
wald( fit.mycrd, Ldiff( fit.mycrd, "drug[^\\.]", ref ="Atypical"))

# Finally: random year

fit.mycry <- update( fit.myc, random = ~ 1 + dvar( drug, id) + year | id)

fit.mycry <- update( fit.myc, random = ~ 1 + dvar( drug, id) + year| id,
                     control = list( msMaxIter = 200, msVerbose = TRUE   ))
summary(fit.mycry)

wald( fit.mycry )


#
#  Please contact Georges Monette <georges@yorku.ca>
#  with any questions or comments.
#

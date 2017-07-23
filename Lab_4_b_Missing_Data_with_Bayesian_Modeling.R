#' ---
#' title:     "Lab 4b: Missing Data with Bayesian Modeling"
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
if(.Platform$OS.type == 'windows') windowsFonts(Arial=windowsFont("TT Arial")) 
interactive <- FALSE  # do not run interactive code


library(p3d)
library(spida2)  # note: loads 'car' + others

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
windowsFonts(Arial=windowsFont("TT Arial"))


##'
##' # Weness Example
##'
#' -  X: Treatment Duration
#' -  Z: Weness 
#' -  Y: Satisfaction
#'
#' Z is hypothetically a mediator for the Treatment effect  
#'
#' Generate variables scaled as T scores
#' 
#' - Treatment is 'intensity or duration of therapy'
#'   and can be considered to have been randomized
#' - Sat is 'relationship satisfaction' the ultimate 
#'   criterion for success of therapy
#' - Weness (couple identity) is the mediator of the
#'   the effect of Treatment on Sat
#'
set.seed(243)
dd <- data.frame( Treat = rnorm( 300, 50, 10 ))
dd$Weness <- .8*(dd$Treat - 50) +  .6 * rnorm(300,0,10) + 50
dd$Sat <-   .8*(dd$Weness - 50) +  .6 * rnorm(300,0,10) + 50

dim(dd)
head(dd)
pairs( dd ) 
#'
#' - Note that pairwise plots don't reveal some relevant 
#'   aspects of the 3d structure of mediation
#'   
Init3d(cex = 1.2, family = 'serif', font = 2)
Plot3d(Sat ~ Treat + Weness, dd)
Axes3d()
Ell3d(alpha = .25, partial = 1, partial.col = 'red', partial.lwd=2)
Ell3d(alpha = .25, partial = 3, partial.col = 'green', partial.lwd=2)
Fit3d( fitm <- lm( Sat ~ Treat, dd))
summary(fitm)
Fit3d( fitfull <- lm(Sat ~ Treat + Weness, dd), col = 'pink')
summary(fitfull)
##'
##' # Missingness due to W ----
##' 
#' - Suppose missing in Y due to Weness 
#' - Therefore MCAR for Sat ~ Treat + Weness
#' - but perhaps not for Sat ~ Treat 
#' - All Weness over 50 missing

# Let's try complete CC analysis

Init3d()
dd$miss.Weness <- with(dd, Weness > 50)   # MNAR wrt to Y ~ X model
Plot3d( Sat ~ Treat + Weness | miss.Weness, dd, col = c('blue', 'gray'))

fit.full <- lm(Sat ~ Treat, dd)
Fit3d(fit.full, col = 'green')

fit.miss <- lm(Sat ~ Treat, subset(dd, !miss.Weness))
Fit3d(fit.miss)


dd$Y <- with(dd, ifelse(miss.Weness, NA, Sat))
fitY <- lm(Y ~ Treat, dd)
summary(fitY)
Fit3d(fitY)
#' 
#' Fitting the same model with Stan will yield the same result. 
#' 
#' So we'll see what we can do if we fit a model that 
#' takes missingness into accout.
#' 
#' The approach is simple:
#' 
#' - Include a missing data model for missing Ys
#' - Include an analysis model for the observed and missing Ys
#' 
cat(c('
data {
  int<lower=0>  N_obs;   // number of non-missing Ys
  vector[N_obs] Y_obs;
  vector[N_obs] Treat_obs;
  vector[N_obs] Weness_obs;
  
  int<lower=0>   N_miss;  // number of missing Y
  vector[N_miss] Treat_miss;
  vector[N_miss] Weness_miss;
}    
parameters {
  vector[N_miss] Y_miss;
  vector[3] b_imp;
  vector[2] b_model;
  real<lower=0> sigma_model;
  real<lower=0> sigma_imp;
}
model {
  b_imp ~ normal(0, 1e5);
  b_model ~ normal(0, 1e5);
  sigma_model ~ student_t(3,0,1);
  sigma_imp ~ student_t(3,0,1);
  Y_miss ~ normal(50,100);
  // Imputation model:
    
  Y_obs ~  normal(b_imp[1] + 
                    b_imp[2] * Treat_obs + 
                    b_imp[3] * Weness_obs, sigma_imp);
  Y_miss ~ normal(b_imp[1] + 
                    b_imp[2] * Treat_miss + 
                    b_imp[3] * Weness_miss, sigma_imp);
  
  // Analysis Model:
    
  Y_obs ~ normal(b_model[1] + 
                   b_model[2] * Treat_obs, sigma_model);
  Y_miss ~ normal(b_model[1] + 
                   b_model[2] * Treat_miss, sigma_model);

}'), file = 'impute_1.stan')

impute1_mod <- stan_model('impute_1.stan') 

#'
#' Data list
#'

nas <- is.na(dd$Y)

impute1_dat <- 
  with(dd, list(
    N_obs = sum(!nas),
    Y_obs = Y[!nas],
    Weness_obs = Weness[!nas],
    Treat_obs = Treat[!nas],

    N_miss = sum(nas),
    Weness_miss = Weness[nas],
    Treat_miss = Treat[nas]
    )
  )

impute1_stan <- sampling(impute1_mod, impute1_dat, chains = 1)



Fit3d( fitm.c <- lm( Sat ~ Treat, dd), col='blue', alpha= .2, lwd = 2)
Fit3d( fitm.m <- lm( Sat ~ Treat, subset(dd, miss.Weness == F)), col='blue', lwd = 2)

Fit3d( fitfull.c <- lm( Sat ~ Treat + Weness, dd), col='red', alpha= .2, lwd = 2)
Fit3d( fitfull.m <- lm( Sat ~ Treat + Weness, subset(dd, miss.Weness == F)), col='red', lwd = 2)

summary(fitm.c)
summary(fitm.m)

summary(fitfull.c)
summary(fitfull.m)


#' ---
#' title:     "R Stan: First Examples"
#' subtitle:  "ICPSR Summer Program at York University"
#' date:      "July 17 to 21, 2017"
#' author:    "Georges Monette"
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: false
#'     theme: readable
#'     highlight: tango
#'     fig.width: 14
#'     fig.height: 9
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
#' Load packages
#' 
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
# windowsFonts(Arial=windowsFont("TT Arial")) 
library(spida2)
# Install the loo package if necessary
# install.packages('loo')
library(loo)
##' 
##' # Height and Weight Example ----
##'
#'
#' Artificial data set with archetypal outliers
#' 
#' We use the subset with no outliers to start then
#' we look at things we can do with a data set with
#' an outlier.
#' 
data(hw)
head(hw)
dd <- subset(hw, Type == 0)  # no outliers
if(interactive) {
  library(p3d)
  Init3d()
  Plot3d(Health ~ Weight + Height | Outlier, hwoutliers)
}
#'
#' Fit the model using data with no outliers
#'
fit <- lm(Health ~ Weight + Height, dd)
summary(fit)
if(interactive) {
  Fit3d(fit, alpha = .5)
  fg()
}
##'
##' # First Stan model ----
##'   
#' Generic Stan model for regression with 
#' improper uniform prior on betas
#' and uniform on sigma
#'
##'
##' ## Step 0: Scale data ----
##' 
#' A major reason for posterior sampling to go poorly is
#' a posterior with a complex geometry. 
#' 
#' A first step is to scale predictors so they have similar variability. As we
#' know this prevents the confidence ellipsoids from becoming too eccentric just
#' as a result of variable scaling. They might be eccentric because of high
#' collinearity but at least rescaling reduces unnecessary eccentricity.
#'
#' Unless you run into serious problems, you shouldn't use 
#' automated methods. Just rescale to units that produce a
#' similar variability. It can also be helpful to centre 
#' variable to a meaningful value. 
#' 
##'
##'
##' ## Step 1: Write Model in Stan ---- 
##'
#' Write the model using the Stan Modeling Language
#' and save it to a file called 'first.stan'
#' Three basic blocks
#' 
#' - data block (optional)
#' - parameters block (optional)
#' - model block (required)
#' 
#' Later:
#' 
#' - transformed data
#' - transformed parameters
#' - generated quantities
#' - functions (maybe)
#' 
#' Here we're using 'cat' but we could have writen the Stan program
#' directly in the 'first.stan' file. That's considered a better
#' practice but would have been unwieldy for a workshop.
#' 
cat( c("
  data {
    int N;   // number of observations
    vector[N] weight;   // vector of weights
    vector[N] height;   // etc.
    vector[N] health;
  }
  parameters {
    real b_0;      // intercept parameter
    real b_weight; 
    real b_height;
    real<lower=0> sigma_y; // non-negative standard deviation
  }
  model {
    health ~ normal(
      b_0 + b_weight * weight + b_height * height,
      sigma_y);   // model of form y ~ normal(mean, sd) 
  }
"), file = 'first.stan')
#'
#' Notes:
#' 
#' - Every command must end with a semicolon
#' - Every variable is declared.
#' - We didn't specify any priors in the 'model' statement
#'   so the default priors are uniform, in this case improper
#' - We can specify lower and upper bounds for parameters
#' 
##'    
##' ## Step 2: Compile the Model ----
##'
#' Compile the Stan program to create an object module 
#' (Dynamic Shared Object) with C++
#' 
#' 
first.dso <- 
  stan_model('first.stan', model_name = 'First Model')
#'
#' The parser is amazingly generous and informative with
#' its error messages.
#' 
#' Compiling takes a while because it produces optimized code that
#' will be used to:
#' 
#' 1. compute the height of the bowl as the skateboard moves around
#' 2. the gradient of the bowl
#' 
#' which needs to be done very fast to minimize sampling time.
#' 
##'
##' ## Step 3: Prepare a Data List for Stan ----
##'                
#' Every variable declared in the 'data' step needs to be
#' fed to Stan through a list in R
#' 
dat <- list(
  N = nrow(dd),
  weight = dd$Weight,
  height = dd$Height,
  health = dd$Health
)
dat
##' 
##' ## Step 4: Sample From the Posterior ----
##'
#' 
#' We give 4 skateboards (chains) a random shove and let 
#' them sample using HMC with NUTS.
#' 
first.stanfit <- sampling(first.dso, dat)
#'                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
##'
##' ## Step 5: Check whether HMC worked ----
##' 
#'  These are diagnostics to check whether Stan worked, 
#'  no so much whether the model is good, although problems 
#'  with Stan are often a consequence of a poor model.
#'  
#' - Did some chains (skateboards) get stuck far from the others
#' - Is there low correlation within each chain, or do they look
#'   like slow local random walks?
#'
#' Initial diagnostics by printing the fit:
#' 
first.stanfit
#' - Rhat: Total variability / Variability within Chains
#'      - values much greater 1 show that chains are not in agreement and not exploring the same regions of parameter space.
#'      - I have seen a suggested requirement that Rhat < 1.01 for all parameter. I get 1.02 or a bit larger on problems that
#'        I think are okay. 
#' - n_eff: effective sample size taking serial correlation
#'   in chains in to account. 
#'      - With default 4 chains of
#'        2000 (1000 post-warmup), the total sample has 
#'        size 4,000. So 1,000 is pretty good. If it isn't greater than 1,000, run MCMC for
#'        more iterations, especially if you are reporting results. 
#'        [Stan's best practices page](https://github.com/stan-dev/stan/wiki/Stan-Best-Practices)
#'        recommends that if `N_eff / N < 0.001` you should be suspicious of the calculation
#'        of `N_eff`.
#'        In our example, the two predictors are stongly correlated
#'        which results in a lower `N_eff/N`.
first.stanfit
#'
#' compare with 'lm':
#' 
lm(Health ~ Weight + Height, dd) %>% summary 
first.stanfit  %>%  print(digits=4)
#' Graphical diagnostics:
traceplot(first.stanfit)   
#' Note: skew in sigma_y, as expected
pairs(first.stanfit)
#' - Lower diagonal contains draws with acceptance probability
#'   below the median. 
#' - Points leading to divergent transitions are in red
#' - Yellow points reached maximum tree depth without a U-turn
#'
##'
##' # Generic regression model with Stan ----
##'
#' Here's a a Stan model that can do OLS regression
#' given a Y vector and an X matrix
#'     
            
cat( c("
data {
  int N;   // number of observations
  int P;   // number of columns of X matrix (including intercept)
  matrix[N,P] X;   // X matrix including intercept
  vector[N] Y;  // response
}
parameters {
  vector[P] beta; // default uniform prior if nothing specied in model
  real <lower=0> sigma;
}
model {
  Y ~ normal( X * beta, sigma ); 
    // note that * is matrix mult.
    // For elementwise multiplication use .*

  // To do ridge regression, throw in  a prior
  // You can vary the variance parameter through data
  // or you can turn it into a parameter with a prior:

  //  beta[2:P] ~ normal(0, k);  
  //         and make k data for sensitivity analysis
  //         or a parameter with a prior.
  //  Note that this makes sense only if all the X's
  //  are on a comparable scale. Make sure not to include
  //  the intercept in the same prior.

}
"), file = 'ols.stan')

#' Create reg_model 'dynamic shared object module' which is compiled C++ code
#' that generates HMC samples from the posterior distribution

system.time(
ols.dso <- stan_model('ols.stan')
)
#
# Prepare the data list
# striplevels
X <- cbind(1, dd$Weight, dd$Height)
dat <- list(
  N = nrow(X), 
  P = ncol(X),
  X = X,
  Y = dd$Health)
dat
ols.fit <- sampling(ols.dso, dat)
ols.fit
#'
#' ols.dso could be used with any X matrix  
#'
#' EXECISE:
#' 
#' Specify a prior for the two slopes in the problem above.
#' Make 'k' data so you can try it with different k's
#' 
#' 
#' To visualize the results:
#' Define a function that returns the posterior mean prediction
#'
fun <- function(stanfit) {
  post <- get_posterior_mean(stanfit)
  beta <- post[,ncol(post)]  # use last column (all chains)
  function(Weight, Height) beta[1] + beta[2] * Weight + beta[3] * Height
}
fun(ols.fit)  # this is a closure
fun(ols.fit)(2,3)

if(interactive) {
  Init3d()
  Plot3d(Health ~ Weight + Height, subset(hw, Type ==3 ))
  Fit3d(fun(ols.fit), col = 'grey')
}
#
# Including Type 3 outlier
#
hw3 <- subset(hw, Type == 3)
hw3

head( Xmat3 <- model.matrix(Health ~ Weight + Height, hw3) )

hw3_list <- list(N = nrow(Xmat3), P = ncol(Xmat3), 
                 X = Xmat3, Y = hw3$Health)

system.time(
  fit3_stan <- sampling(ols.dso, hw3_list)  # same dso
)
print(fit3_stan)
pairs(fit3_stan,pars=c('beta','sigma','lp__'))
traceplot(fit3_stan)
get_posterior_mean(fit3_stan)[,5]
if(interactive) {
  Fit3d(fun(fit3_stan), col = 'magenta')
}
##'
##' # Robust fits with a heavy-tailed error distribution ----
##'
#' So far quite boring
#' - nothing new, MCMC with normal error and uniform prior
#'   give results like OLS
#' - but we can easily change the error distribution
#'
#' It's as easy as pi to use a different family of distributions
#' for error.
#' 
#' Exactly the same except for the error distribution and
#' add nu for degrees for freedom for t distribution
#' 
cat(c( 
"
data {
  int N;   // number of observations
  int P;   // number of columns of X matrix (including intercept)
  matrix[N,P] X;   // X matrix including intercept
  vector[N] Y;  // response
  int nu;   // degrees for freedom for student_t
}
parameters {
  vector[P] beta;   // default uniform prior if nothing specied in model
  real <lower=0> sigma;
}
model {
  Y ~ student_t(nu, X * beta, sigma);
}
"), file = 'robust.stan')

system.time(
  robust_model_dso <- stan_model('robust.stan')
)

fit3_stan_6 <- sampling(robust_model_dso, c(hw3_list, nu = 6))
fit3_stan_6

pairs(fit3_stan_6, pars = c('beta','sigma','lp__'))
traceplot(fit3_stan_6)

if(interactive) {
  Fit3d(fun(fit3_stan_6), col = 'purple')
}
#'
#' Let's try more kurtosis
#'
fit3_stan_3 <- sampling(robust_model_dso, c(hw3_list, nu = 3))

traceplot(fit3_stan_3)
fit3_stan_3
if(interactive) {
  Fit3d(fun(fit3_stan_3), col = 'pink')
}
##'
##' # Using proper priors ----
##' 
#' Using proper priors will often help with models that
#' don't work with improper uniform priors.
#' 
#' How 'informative' priors should be is a question
#' that is both pragmatic and philosophical.
#' 
#' See [prior choice recommendations](https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations) 
#' and [prior distributions](https://github.com/stan-dev/rstanarm/wiki/Prior-distributions)  
#'
#' Although it's easy to specify a prior with this generic regression, it usually does not
#' make sense to do so. The prior should be formulated in a way that is reasonable for the 
#' structure of the data.  See the example using the Prestige data set to illustrate a
#' way of handling a catetorical variable.
#' 
cat(c( 
"
  data {
    int N;   // number of observations
    int P;   // number of columns of X matrix (including intercept)
    matrix[N,P] X;   // X matrix including intercept
    vector[N] Y;  // response
    int nu;   // degrees for freedom for student_t
  }
  parameters {
    vector[P] beta;   // default uniform prior if nothing specied in model
    real <lower=0> sigma;
  }
  model {

  // prior distributions:

    beta ~ student_t(6,0,10); // semi-informative prior
    sigma ~ student_t(6,0,10);      // folded t

  // model:

    Y ~ student_t(nu, X * beta, sigma);
  }
  
"), file = 'proper_prior.stan')

system.time(
  proper_prior_dso <- stan_model('proper_prior.stan')
)

fit_prior_3 <- sampling(proper_prior_dso, c(hw3_list, nu = 3))

traceplot(fit3_stan_3)
fit3_stan_3
if(interactive) {
  Fit3d(fun(fit3_stan_3), col = 'cyan')
}
##'
##'
##' # Fit Indices for Bayesian models: WAIC and LOO ----
##' 
#'
#' See @fox2015applied [pp 669ff] for a review of 
#' information criteria such AIC and BIC
#' 
#' Except from [Vektari et al. (2016) Practical Bayesian model evaluation using leave-one-out cross-validation and WAIC](https://arxiv.org/abs/1507.04544)
#'
#' Leave-one-out cross-validation (LOO) and the widely 
#' applicable information criterion (WAIC) are methods 
#' for estimating pointwise out-of-sample prediction 
#' accuracy from a fitted Bayesian model using the 
#' log-likelihood evaluated at the posterior 
#' simulations of the parameter values. LOO and 
#' WAIC have various advantages over simpler 
#' estimates of predictive error such as AIC and 
#' DIC but are less used in practice because they 
#' involve additional computational steps. 
#' 
#' Here we 
#' lay out fast and stable computations for LOO and 
#' WAIC that can be performed using existing simulation 
#' draws. We introduce an efficient computation of LOO 
#' using Pareto-smoothed importance sampling (PSIS), a 
#' new procedure for regularizing importance weights. 
#' Although WAIC is asymptotically equal to LOO, 
#' we demonstrate that PSIS-LOO is more robust in the 
#' finite case with weak priors or influential observations. 
#' As a byproduct of our calculations, we also obtain 
#' approximate standard errors for estimated predictive 
#' errors and for comparing of predictive errors between 
#' two models. We implement the computations in an R 
#' package called 'loo' and demonstrate using models 
#' fit with the Bayesian inference package Stan.
#'
#' Also see [Vektari and Gelman (2014) WAIC and cross-validation in Stan](http://www.stat.columbia.edu/~gelman/research/unpublished/waic_stan.pdf)
#'
#' The 'generated quantities' block evaluates the log-likelihood
#' at each observed point for each model.
#' 
cat(c("
data {
  int N;   // number of observations
  int P;   // number of columns of X matrix (including intercept)
  matrix[N,P] X;   // X matrix including intercept
  vector[N] Y;  // response
  int nu;   // degrees for freedom for student_t
}
parameters {
  vector[P] beta;   // default uniform prior if nothing specied in model
  real <lower=0> sigma;
}
model {
  Y ~ student_t(nu, X * beta, sigma); 
}
generated quantities { 
  // compute the point-wise log likelihood 
  // at each point to compute WAIC
  vector[N] log_lik;
  for(n in 1:N) {  // index n for loop need not be declared
    log_lik[n] = student_t_lpdf(Y[n] | nu, X[n,] * beta , sigma);
  }
}
"), file = 'robust_loo.stan') 

system.time(
  robust_loo_dso <- 
    stan_model('robust_loo.stan', model_name = 'robust with LOO')
)
#'
#' See description of 'student_t_lpdf' in stan documentation.
#' (lpdf = log probability density function)
#' 
#' Fit models with different error kurtoses
#'

fitlist <- list(
  fit3_stan_3 = sampling(robust_loo_dso, c(hw3_list, nu = 3)),
  fit3_stan_6 = sampling(robust_loo_dso, c(hw3_list, nu = 6)),
  fit3_stan_100 = sampling(robust_loo_dso, c(hw3_list, nu = 100))
)

fitlist %>% 
  lapply(print, pars = c('beta','sigma')) %>% 
  invisible

fitlist  %>% 
  lapply(extract_log_lik) %>%
  lapply(loo)

fitlist  %>% 
  lapply(extract_log_lik) %>%
  lapply(loo) ->
  loolist
#' 
#' Re [Pareto k diagnostic](https://rdrr.io/cran/loo/man/pareto-k-diagnostic.html)
#' 
#' Importance sampling is likely to work less well if the marginal posterior
#' p(θ^s | y) and LOO posterior p(θ^s | y_{-i}) are much different, which is more
#' likely to happen with a non-robust model and highly influential observations.
#' 
#' A robust model may reduce the sensitivity to highly influential observations.
#' 
##'
##' ## Pairwise comparisons of LOO with SEs ----
##' 
##' See the help file and references on
?loo::compare
#' 
#' Pairwise comparisons with SEs:
#' 
#' Note that the only 'significant' comparison is
#' betweeen the two 'student_t' models   
#'
loolist[1:2] %>% compare(x = .)
loolist[c(1,3)] %>% compare(x = .)
loolist[c(2,3)] %>% compare(x = .)
##'
##' ## Identifying outliers ---- 
##'
#' A large Pareto-k parameter indicates an unusual point
#' with large weight in Pareto-Smoothed Importance Sampling (PSIS).
#' 
#' For the quasi-normal model (nu = 100):
loo3 <- loo(extract_log_lik(fitlist[[3]]))
loo3$pareto_k %>% plot(pch=16, cex =2)
#'
#' correctly identifies observation 15 as influential. See @vehtari2017practical or
#' [online preprint](http://www.stat.columbia.edu/~gelman/research/unpublished/loo_stan.pdf)
#' 
#' Also: cho2009bayesian, @zhu2011bayesian and zhu2012bayesian.
#' 
#' EXERCISE: 
#' - See what happens with the Student_t models. Does observation 15 look
#'   more 'normal' even though its residual is larger?
#'
##'
##' # Categorical Predictors ----
##'
#'
library(car)
head(Prestige)
xqplot(Prestige)
#'
#' Using complete cases
#'
Prestige %>% 
  subset(!is.na(type)) %>% 
  droplevels ->    # often good practice if dropping levels of a factor
  dd
tab(dd, ~type)
#'
#' Note that `type` has `r length(unique(dd$type))` levels.
#'
#' We will regress 'prestige' on 'type' and 'women' (percentage of women).
#' 

cat(c("

  data{
    int N; 
    int Ntype;
    int type[N]; // type will be coded as an integer from 1 to 3
    vector[N] women;
    vector[N] prestige;
  } 
  parameters{
    real m_prestige;
    real b_women;
    vector[Ntype] u_type;
    real<lower=0> sigma;
  }
  transformed parameters{
    vector[Ntype] m_type;
    m_type = m_prestige + u_type;
  }
  model{
    // uniform on m_prestige, b_women sigma
    u_type ~ normal(0,100);  // proper prior on deviations
           // -- a proper Bayesian hierarchical model for
           // type would use a hyperparameter instead of 100
           // and the hyperparameter would help determine
           // appropriate amount of pooling between types
    prestige ~ normal(
      m_prestige + 
        u_type[type] +     // note how this works using array indexing
                           // -- a key technique for hierarchical modeling
        b_women * women,
      sigma);
  }

"), file = "prestige.stan")

prestige_dso <- stan_model("prestige.stan")

#'
#' Data
#'
#' Here's a trick to make sure the levels of the integer code
#' for the categorical variable go from 1 to the number of levels.
#' 
#' The quirks of factors come in handy here because, 
#' if `x` is a categorical variable 
#' -- of any class: factor, character, logical, numeric
#' then 
#'      
#' >    `as.numeric(as.factor(x))`
#'      
#' will give you exactly what you want: 
#' integers from 1 to the number of levels of x.      
#'

dat <- 
  with(dd,
       list( N = nrow(dd),
             Ntype = length(unique(type)),
             type = as.numeric(as.factor(type)), # to ensure integers from 1 to 3
             women = women,
             prestige = prestige
       )
  )

prestige.stanfit <- sampling(prestige_dso, dat)
prestige.stanfit
wald(prestige.stanfit, diag(3), pars = 'm_type' )
Ldiff <- rbind(
  '2-1' = c(-1,1,0),
  '3-1' = c(-1,0,1),
  '3-2' = c(0,-1,1)
)
wald(prestige.stanfit, Ldiff, pars = 'm_type' )
summary(fitlm <- lm(prestige ~ type + women, dd))
wald(fitlm, Ldiff(fitlm, 'type'))
#'
#' EXERCISE:
#' 
#' Try to specify a hyperparameter for the variability between 'u_type'.
#' Experiment with priors for the hyperparameter.
#'
##'
##' # Notes ----
##'
#' - Some links: 
#'     - [Stan documentation](http://mc-stan.org/users/documentation/index.html)
#'     - [Manual: download as a pdf](https://github.com/stan-dev/stan/releases/download/v2.16.0/stan-reference-2.16.0.pdf)
#'     - [Stan tutorials](http://mc-stan.org/users/documentation/tutorials)
#'     - @stan2017forums [Stan Forums](http://discourse.mc-stan.org/)
#'         - [Old list -- now 'deprecated'](https://groups.google.com/forum/#!forum/stan-users)
#'     - [Betancourt (2017) A conceptual introduction to HMC](https://arxiv.org/pdf/1701.02434v1.pdf)
#' - Note from the documentation [@standevelopmentteam2017stan]
#'     - An example of the distributions available in the model block 
#'       for __both priors and models__. The only
#'       difference is that the 'random variable' is data for the model and a parameter for a prior.
#'         - `y ~ normal(mu, sigma);`  -- Note `sigma` is the standard deviation
#'         - `y ~ student(nu, mu, sigma)` -- Note: `sigma` is a scale parameter, not the SD
#'         - `y ~ cauchy(mu, sigma)` -- Note: same as cauchy with `nu = 0`
#'         - `y ~ exponential(beta)` -- Note: beta is the waiting time. Expected value is 1/beta
#'         - `y ~ poisson(lambda)`
#'         - `y ~ bernoulli(prob)`  -- y is 0 or 1 
#'         - `y ~ bernoulli_logit(theta)`  -- where `theta` is the logit
#'     - To use distributions in Stan, you need to understand the type declarations for the `_lpmf`
#'       (for a discrete distribution) or for the `_lpf` version of the corresponding function. For
#'       example, for the `bernouilli_logit` sampling function, search for `bernouilli_logit` in the
#'       manual [@stan2017stan] and look at declarations for `bernoulli_logit_lpmf` which are:
#'           - `real bernoulli_logit_lpmf(ints y | reals alpha)`
#'           - This means that the random variable `y` must be an integer, hence a scalar or an array, 
#'             and that the logit parameter must be a 'real', i.e. a real scalar, a vector or an array.
#'     - See section 24.3 for a description of sampling:
#'         - Simulating a skateboard with leapfrog steps:
#'             - simple models: ok to use a few big steps
#'             - complex models: use many small steps
#'             - or default NUTS: keep going until U-turn, adjust step size adaptively
#'             - with 'perfect simulation' always accept proposal because
#'               probability: sum of potential and kinetic energy
#'               always constant, but not with simulation. Trade-off
#'               between step size and probability of acceptance.
#'     - See section 25 for a very good description
#'       of data types.Notably:
#'         - ints are promoted to reals where necessary. But avoid.
#'         - bounds may be expressions but must use previously 
#'           declared variables (data + parameters). If bounds
#'           are parameter, log_Jacobian is adjusted appropriately.
#'         - vectors, row vectors and matrices are reals, arrays
#'           anything.
#'         - index starts at 1, in contrast with C++
#'         - vectors are column vectors. Dimension shown in declaration:
#'           `vector[3] u;`. Bounds apply to all terms, e.g.
#'           `vector<lower=0>[3] u;`
#'              - size expressions may use data, transformed data or 
#'                local variables, not parameters, transformed
#'                parameters or generated quantities.
#'         - Fancy constraints:
#'             - unit simplex: `simplex[5] theta;` non-neg, sum to 1,
#'               looks like a vector
#'             - ordered vector: `ordered[5] c;`
#'             - row vector: `row_vector[5] u;`
#'             - matrices: `matrix[M,N] A;` (`M`, `N` must be integers)
#'                 - assign to row: `A[1] = b` assigns vector to row
#'                 - `corr_matrix[6] B;` constrains matrix
#'                 - `cholesky_factor_corr[6] L;`
#'                 - `cov_matrix[6] S;`
#'                 - `cholesky_factor_cov[6] L;`
#'         - Assignments and constraint checks:
#'             - runtime size checks at end of blocks, not compiler
#'             - data: when read or after transformed data block
#'             - parameters: enforced by transform
#'             - transformed parameters: at end of block 
#'             - Single index to matrix refers to row 
#'               of type `row_vector`
#'         - Arrays of anything, arbitrary dimension, follow name:
#'             - e.g. `matrix[3,4] A[2,3];` a 2x3 array of 3x4 matrices
#'             - `A[1]` is a a sub-array: a 3-array of 3x4 matrices.
#'             - Arrays and subarrays can be manipulated and assigned
#'             - Array of matrices: array indices go first.
#'         - Assigment: arrays, vectors, row_vectors, matrices are 
#'           not inter-assignable unless at the scalar level with a loop or
#'           with sub-arrays/vectors of corresponding size and type.
#'         - Reserved names: `for,in,while,repeat,until,if,then,else,true,false`+ a bunch more  
#'           -- see the manual
#'         - operators: 
#'             - use apostrophe for vector or matrix transpose, e.g. `A'B`
#'             - use `*`` for matrix multiplication
#'             - use `.*`` for element-wise multiplication
#'             - vectorization of functions and operators: see manual, this is expanding
#'         - Indexing: 
#'             - indices can be integers or integer arrays or 
#'               symbols, e.g. `:` denoting all, `5:` denoting 5 up
#'               or `:6` denoting 1 to 6.
#'             - If `x = [11,12,13,14]` is a row vector and 
#'               `ii = (1,2,1,2,3)` is an integer array (note that this is not
#'               correct Stan notation to assign vectors), then
#'               `x[ii] = [11,12,11,12,13]` is a row vector.
#'         - Type signatures for Object-Oriented dispatching:
#'             - `real mean(real[])` and `real mean(vector)`
#'               are different methods of the `mean` generic function
#'         - Constants: `pi(), e(), 1.0` are real, `1` is int.       
#' - References:
#'     - @mcelreath2015statistical is considered a good introduction to Stan, highly
#'       reccommended by the developers of Stan, although
#'       it, unfortunately, avoids coding in Stan by providing a front end in an
#'       R package.
#'     - @carpenter2016stan is a recent article with an up-to-date introduction to Stan.
#'     - gelman2015stan 
#'     - @standevelopmentteam2017documentation is the on-line documentation that is a paragon of intelligible information.  
#'     - @standevelopmentteam2017stan needs to be downloaded as a pdf file. Go to
#'       @standevelopmentteam2017documentation to download the latest version.
#'     - @stan2017prior gives some recommendations for priors.
#'     - [HMC and Banana Distributions](http://nross626.math.yorku.ca/SCS_Longitudinal/files/Orbits.R.html)
#'     - @stan2016warnings [Brief Guide to Stan's Warnings](http://mc-stan.org/misc/warnings.html)
#'     - @papaspiliopoulos2007general is good article, although slightly abstract, article on the parametrization of hierarchical models.
#'
#' # References         

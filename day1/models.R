##--------------------------------------------------------------##
##          Linear and Generalized Linear Models in R           ##
##                        John Fox                              ##
##   Introduction to the R Statistical Computing Environment    ##
##                 ICPSR, York University                       ##
##                          2017                                ##
##--------------------------------------------------------------##

# Linear models in R

    # multiple regression (Prestige data)

library(car)
some(Prestige)  # 10 randomly sampled cases

prestige.mod.1 <- lm(prestige ~ education + log2(income) + women,
                     data=Prestige)  
summary(prestige.mod.1)

        # confidence intervals for coefficients

# Confidence intervals for coefficients

confint(prestige.mod.1)

    # dummy regression

Prestige$type # a factor
class(Prestige$type) 
str(Prestige$type) # structure

Prestige.2 <- na.omit(Prestige) # filter out missing data
nrow(Prestige)
nrow(Prestige.2)
levels(Prestige.2$type)
Prestige.2$type <- with(Prestige.2, 
    factor(type, levels=c("bc", "wc", "prof"))) # reorder levels
Prestige.2$type
as.numeric(Prestige.2$type)

        # generating contrasts from factors

getOption("contrasts")
contrasts(Prestige.2$type)
model.matrix(~ type, data=Prestige.2)

        # changing baseline category

contrasts(Prestige.2$type) <- contr.treatment(levels(Prestige.2$type), base=2)
contrasts(Prestige.2$type)

        # other contrast-generating functions (time permitting)

contrasts(Prestige.2$type) <- "contr.helmert"  # Helmert contrasts
contrasts(Prestige.2$type)

contrasts(Prestige.2$type) <- "contr.sum"  # "deviation" contrasts
contrasts(Prestige.2$type)

contrasts(Prestige.2$type) <- NULL  # back to default
contrasts(Prestige.2$type)

        # ordered factors (time permitting)

Prestige.2$type.ord <- ordered(Prestige.2$type, 
    levels=c("bc", "wc", "prof")) # ordered factor
Prestige.2$type.ord
round(contrasts(Prestige.2$type.ord), 3)   # orthogonal polynomial contrasts
crossprod(contrasts(Prestige.2$type.ord))  # floating-point arithmetic 
                                           # is not exact!
round(crossprod(contrasts(Prestige.2$type.ord)), 10)
zapsmall(crossprod(contrasts(Prestige.2$type.ord)), 10)

        # fitting a dummy-regression model

prestige.mod.2 <- lm(prestige ~ log2(income) + education + type, 
                     data=Prestige.2)
summary(prestige.mod.2)

scatter3d(prestige ~ log2(income) + education | type, 
          parallel=TRUE, data=Prestige)

        # adding interactions to the model

prestige.mod.3 <- update(prestige.mod.2, 
                         . ~ . + log2(income):type + education:type)     # adding interactions
summary(prestige.mod.3)
scatter3d(prestige ~ log2(income) + education | type, 
          parallel=FALSE, data=Prestige)

# equivalent specifications (* is crossing operator):
lm(prestige ~ log2(income)*type + education*type, data=Prestige.2) 
lm(prestige ~ (log2(income) + education)*type, data=Prestige.2)

    # ANOVA tables for linear models

anova(prestige.mod.3)  # sequential (type-I): usually not of interest
Anova(prestige.mod.3) # partial, obeying marginality (type-II)

    # direct computation of likelihood-ratio F-tests for nested models

anova(prestige.mod.2, prestige.mod.3) # both sets of interactions

(mod.duncan.1 <- lm(prestige ~ income + education, data=Duncan))
mod.duncan.2 <- lm(prestige ~ I(income + education), data=Duncan) # equal slopes
anova(mod.duncan.1, mod.duncan.2) # test of equal slopes

    # Wald F-tests of linear hypotheses

matchCoefs(prestige.mod.3, ":")
linearHypothesis(prestige.mod.3, matchCoefs(prestige.mod.3, ":")) # no interactions

linearHypothesis(mod.duncan.1, "income = education") # test of equal slopes

    # visualisation: effect plots

library(effects)
# library(lattice)
# trellis.par.set(strip.background=list(col="lightgrey"))
plot(allEffects(prestige.mod.3))

    # more on lm() (time permitting)

args(lm)

some(Davis)
?Davis
lm(weight ~ repwt, data=Davis, subset=sex == "F")  # observation selection 
                                                   # (women only)
lm(weight ~ repwt, data=Davis, subset=1:100)  # first 100 cases

lm(prestige ~ income + education, data=Duncan, subset=-c(6, 16))  
                                            # omit cases 6 & 16

lm(conformity ~ partner.status*fcategory,  # specifying contrasts
   contrasts=list(partner.status=contr.sum, fcategory=contr.poly),
   data=Moore)

lm(100*conformity/40 ~ partner.status*fcategory, data=Moore)
                                #  note computation of y

lm(prestige ~ I(income + education), data=Duncan)  
                # "protecting" expresssion on RHS of the model


# Generalized linear models

    # binary logit model

?Cowles

mod.cowles <- glm(volunteer ~ sex + extraversion*neuroticism, 
                  family=binomial, data=Cowles)
summary(mod.cowles)
round(exp(cbind(Estimate=coef(mod.cowles), confint(mod.cowles))), 3) # odds ratios

    # analysis of deviance for a GLM

Anova(mod.cowles)  # type-II tests

    # effects plots

plot(Effect(c("extraversion", "neuroticism"), mod.cowles))
plot(Effect(c("extraversion", "neuroticism"), mod.cowles), 
     multiline=TRUE, ci.style="bands", type="response", rug=FALSE)

    # Poisson and quasi-Poisson regression

some(Ornstein)
nrow(Ornstein)
?Ornstein
(tab <- xtabs(~interlocks, data=Ornstein))

x <- as.numeric(names(tab)) # the names are the distinct values of interlocks
plot(x, as.vector(tab), type="h", 
     xlab="Number of Interlocks", ylab="Frequency")
points(x, tab, pch=16)

mod.ornstein <- glm(interlocks ~ log2(assets) + nation + sector,
                    family=poisson, data=Ornstein)
summary(mod.ornstein)
Anova(mod.ornstein)

# quasi-Poisson model, allowing for overdispersion

mod.ornstein.q <- update(mod.ornstein, family=quasipoisson)
summary(mod.ornstein.q)
Anova(mod.ornstein, test="F")  # can use F-test with estimated dispersion

# Some regression (model) diagnostics (time permitting)

    # added-variable ("partial regression") plots

avPlots(mod.duncan.1, id.n=3, id.method="mahal")
whichNames(c("minister", "conductor"), Duncan)
mod.duncan.3 <- update(mod.duncan.1, subset = - c(6, 16))
compareCoefs(mod.duncan.1, mod.duncan.3)

    # component + residual ("partial residual") plots

prestige.mod.4 <- lm(prestige ~ income + education, data=Prestige) # original analysis
crPlots(prestige.mod.4)
crPlots(prestige.mod.4, span=3/4)

prestige.mod.5 <- lm(prestige ~ log2(income) + education, data=Prestige) # original analysis
summary(prestige.mod.5)
crPlots(prestige.mod.5, span=3/4)

    # adding partial residuals to effect plots

prestige.mod.2
plot(Effect(c("income", "type"), prestige.mod.2,
            partial.residual=TRUE), span=3/4)
plot(Effect(c("education", "type"), prestige.mod.2,
            partial.residual=TRUE), span=3/4)

prestige.mod.6 <- lm(prestige ~ type*(income + education), data=Prestige.2)
Anova(prestige.mod.6)
plot(allEffects(prestige.mod.6, partial.residuals=TRUE), span=3/4)

    # these diagnostics also work with GLMs

plot(Effect(c("extraversion", "neuroticism"), mod.cowles,
            partial.residuals=TRUE), span=3/4)

mod.ornstein.1 <- glm(interlocks ~ assets + nation + sector,
                      family=quasipoisson, data=Ornstein)
crPlot(mod.ornstein.1, "assets", span=2/3)
mod.ornstein.2 <- glm(interlocks ~ log2(assets) + nation + sector,
                      family=quasipoisson, data=Ornstein)
crPlot(mod.ornstein.2, "log2(assets)", span=2/3)
avPlot(mod.ornstein.2, "log2(assets)", id.n=2, id.method="mahal")

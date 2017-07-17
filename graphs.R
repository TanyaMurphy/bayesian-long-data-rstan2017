##--------------------------------------------------------------##
##            Basic Statistical Graphics in R                   ##
##                        John Fox                              ##
##   Introduction to the R Statistical Computing Environment    ##
##                 ICPSR, York University                       ##
##                          2017                                ##
##--------------------------------------------------------------##

# Univariate displays

    # histograms

library("car")
?UN
head(UN)
nrow(UN)
UN <- na.omit(UN)  # complete cases
nrow(UN)

with(UN, hist(gdp))
?hist
with(UN, hist(gdp, breaks="FD", col="gray"))  # change rule for bins
box()

    # density estimates

with(UN, {
    hist(gdp, freq=FALSE, breaks="FD", col="lightgray", main="") # on density scale
    lines(density(gdp, from=0), lwd=2, lty=2)         # kernel estimate
    lines(adaptiveKernel(gdp, from=0), lwd=2, lty=1)  # adaptive kernel
    rug(gdp)
    legend("topright", c("Fixed bandwidth", "Adaptive bandwidth"), 
           lty=2:1, lwd=2, inset=.02)
    box()
})

    # stem-and-leaf display

with(UN, stem(gdp))

library("aplpack")
with(UN, stem.leaf(gdp))  # better

    # boxplot

with(UN, boxplot(gdp))

Boxplot(~ gdp, data=UN)  # better ("one-sided" formula)

    # transformation to symmetry

summary(powerTransform(gdp ~ 1, data=UN))  # marginal Box-Cox transformation

symbox(~ gdp, data=UN)  # visual selection of transformation to symmetry

densityPlot(~ log10(gdp), method="adaptive", data=UN)  # one-sided formula
basicPowerAxis(0, base=10, side="above", at=10^(1:5), 
               axis.title="GDP per Capita (dollars)")

# Bivariate relationships

    # scatterplots (2 numeric variables)

with(UN, plot(gdp, infant.mortality))
plot(infant.mortality ~ gdp, data=UN)  # equivalent

        # WARNING: make sure to exit from identification mode!
with(UN, identify(gdp, infant.mortality, rownames(UN)))

scatterplot(infant.mortality ~ gdp, data=UN, # enhanced
            id.method="identify")

        # transformation to linearity and symmetry

            # marginal multivariate Box-Cox transformation
summary(powerTransform(cbind(gdp, infant.mortality) ~ 1, data=UN))  

            # showing 50 and 95% concentration ellipses,
            #  and with automatic point ID by Mahalanobis distances
scatterplot(log10(infant.mortality) ~ log10(gdp), data=UN,
            ellipse=TRUE, id.method="mahal", id.n=5)

        # jittering discrete numeric variables

?Vocab
head(Vocab)
nrow(Vocab)

scatterplot(vocabulary ~ education, data=Vocab)

scatterplot(vocabulary ~ education, data=Vocab, jitter=list(x=2, y=2),
            pch=".", span=1/3, boxplots=FALSE)


    # parallel boxplots (numeric response and factor or discrete x)

Boxplot(vocabulary ~ education, data=Vocab, id.n=0)

?Ornstein
head(Ornstein)

Boxplot(interlocks ~ nation, data=Ornstein)

        # transformation to symmetry and equal spreads

spreadLevelPlot(interlocks + 1 ~ nation, data=Ornstein) # add 1 to avoid 0s

summary(powerTransform(interlocks + 1 ~ nation, data=Ornstein))

(oldpar <- par(mar=c(5, 4, 4, 4) + 1)) # leave extra room at right
Boxplot(log2(interlocks + 1) ~ nation, data=Ornstein)
basicPowerAxis(0, base=2, start=1, axis.title="interlocks", 
               at=c(1, 5, 10, 20, 40, 80) + 1)
par(oldpar)  # restore margins (or just clear plot)

# Plotting multivariate data

    # 3D scatterplots

scatter3d(prestige ~ income + education, data=Duncan, id.n=3)
scatter3d(prestige ~ income + education, data=Duncan, id.n=3, ellipsoid=TRUE)

    # scatterplot matrices

pairs(~ income + education + prestige, data=Duncan)

scatterplotMatrix(~ prestige + income + education, data=Duncan,
                  span=0.7, id.n=3)

?Prestige
head(Prestige)

scatterplotMatrix(~ prestige + income + education, data=Prestige)

        # transformation towards multivariate normality

summary(powerTransform(cbind(prestige, income, education) ~ 1, data=Prestige))

scatterplotMatrix(~ prestige + I(income^(1/3)) + education, data=Prestige) 
                               # note use of I() in formula!

    # conditioning on a factor

Prestige$type <- factor(Prestige$type, levels=c("bc", "wc", "prof")) # reorder levels

scatterplotMatrix(~ prestige + income + education | type, data=Prestige,
                  by.groups=TRUE, span=1, ellipse=TRUE, levels=0.5)

summary(powerTransform(cbind(prestige, income, education) ~ type, data=Prestige))

scatterplotMatrix(~ prestige + log10(income) + education | type, data=Prestige,
                  by.groups=TRUE, span=1, ellipse=TRUE, levels=0.5)

# Popular graphics packages

    # lattice for trellis graphs (part of the standard R distribution)

library("lattice")

xyplot(prestige ~ income | type, data=Prestige,
       panel=function(x, y){
           panel.grid()       # grid lines
           panel.xyplot(x, y) # points
           panel.lmline(x, y, lwd=2) # LS line
           panel.loess(x, y, span=0.8, lwd=2, lty=2, col="magenta") # smooth
       }
)

    # ggplot2 ("grammar of graphics")

library("ggplot2")

Prestige2 <- na.omit(Prestige)  # removing missing data

ggplot(data=Prestige2, aes(x=income, y=prestige), show.legend=FALSE) + 
    geom_point() +
    geom_smooth(method="lm", se=FALSE, lty="dashed", col="magenta") +
    geom_smooth(method="loess", span=0.8, se=FALSE) +
    facet_grid(. ~ type) 


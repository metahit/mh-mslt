library(devtools)
# install_github("chjackson/disbayes")
options(mc.cores = parallel::detectCores()) # set if you have multi-core computer. enables Stan to run faster 



## One example disbayes run - to show how it works
## Bristol, male, IHD
## Fit two models to estimate case fatality
## (1): unsmoothed: case fatalities for different ages estimated independently of each other 
## (2): smoothed: case fatality assumed to be a smooth spline function of age.   Should give more precise estimates, but won't work if the data are weak 

library(disbayes)
dat <- dplyr::filter(disbayes_inputs, disease == "brsc", sex == "female", cityregion == "bristol")

resu <- disbayes:::disbayes(dat = dat,
                 
                 ## You can supply either estimates and denominators, or estimates with credible intervals, or numerators and denominators.  See help(disbayes)
                 inc = "inc", 
                 inc_denom = "pop", 
                 prev_num = "prevn", 
                 prev_denom = "prevdenom",
                 mort = "mort",
                 mort_denom = "pop",

                 ## You'll need to change this for different diseases:
                 ## the age below which all case fatalities are
                 ## assumed equal in the smoothed model 
                 eqage = 30, 
                 smooth = TRUE  # or FALSE if don't want smoothed estimates
                 )

## Posterior medians and 95% credible intervals for all unknowns in the model
summ <- disbayes:::summary.disbayes(resu)

## Handy tool to extract specific variables from this 
output <- disbayes:::summary.disbayes(resu, vars=c("cf","inc")) 

## Plot smoothed and unsmoothed estimates 
disbayes:::plot.disbayes(resu)


## Plot just smoothed estimates
plot(summ, variable="cf")

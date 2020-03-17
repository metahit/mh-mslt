# 1
# datadir <- paste0(relative_path_mslt, "data/city regions/Input disbayes")
# fp <- file.path(datadir,"bristol_female_ishd.rds")
dat <- readRDS(fp)
## Add incidence and mortality numerator and denominator to originally genereted dataset for disbayes inputs. 
### IF IT WORKS, CREATE A DATA SET FOR DISBAYES THAT INCLUDES ALL INPUTS (NEED TO MODIFY GENERATE INPUTS DATAFRAME)
### WE HAVE TO DO THIS DUE TO THE AGGREGATION OF LOCALITIES. 

## One example disbayes run - to show how it works
## Bristol, male, IHD
## Fit two models to estimate case fatality
## (1): unsmoothed: case fatalities for different ages estimated independently of each other 
## (2): smoothed: case fatality assumed to be a smooth spline function of age.   Should give more precise estimates, but won't work if the data are weak 


## Filter disbayes_inputs for city region, disease and sex data

dat <- dplyr::filter(disbayes_inputs, cityregion == "greatermanchester", disease == "brsc", sex == "female") 
dat2 <- dplyr::filter(disbayes_inputs_original, cityregion == "greatermanchester", disease == "brsc", sex == "female") 

library(disbayes)

resu <- disbayes(dat = dat,
                 
                 ## You can supply either estimates and denominators, or estimates with credible intervals, or numerators and denominators.  See help(disbayes)
                 # inc = "inc",
               
                 # inc_num = "num_incidence", 
                 # inc_denom = "denom_incidence", 
                 # prev_num = "prevn",
                 # prev_denom = "prevdenom",
                 # mort_num = "num_deaths", 
                 # mort_denom = "num_deaths",
                 
                 
                 
                 
                 ## Original data
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
summ <- summary(resu) 

## Handy tool to extract specific variables from this 
test_output <- as.data.frame(summary(resu, vars=c("cf","inc")))

## Plot smoothed and unsmoothed estimates 
plot(resu)



## Plot just smoothed estimates
plot(summ, variable="cf")

library(devtools)


## BZ: added this as otherwise does not dowload from the github
# 
devtools::install_github("r-lib/remotes")
# .rs.restartR()

CXX14 = "C:/Rtools/mingw_64/bin/g++.exe"

##Ali's path
Sys.getenv("R_MAKEVARS_USER")


dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) 
  dir.create(dotR)
M <- file.path(dotR, "Makevars.win")
if (!file.exists(M)) 
  file.create(M)
cat("\nCXX14FLAGS=-O3 -Wno-unused-variable -Wno-unused-function",
    "CXX14 = $(BINPREF)g++ -m$(WIN) -std=c++1y",
    "CXX11FLAGS=-O3 -Wno-unused-variable -Wno-unused-function",
    file = M, sep = "\n", append = TRUE)

##

install_github("chjackson/disbayes")
options(mc.cores = parallel::detectCores()) # set if you have multi-core computer. enables Stan to run faster 

datadir <- paste0(relative_path_mslt, "data/city regions/Input disbayes")
fp <- file.path(datadir,"bristol_female_ishd.rds")
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

dat <- dplyr::filter(disbayes_inputs, cityregion == "bristol", disease == "ishd", sex == "female")


library(disbayes)

resu <- disbayes(dat = dat,
                 
                 ## You can supply either estimates and denominators, or estimates with credible intervals, or numerators and denominators.  See help(disbayes)
                 inc = "inc",
                 # inc_num = "num_incidence_ishd",
                 inc_denom = "pop", 
                 prev_num = "prevn", 
                 prev_denom = "prevdenom",
                 mort = "mort",
                 # mort_num = "num_deaths_ishd",
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

warnings()

## Plot just smoothed estimates
plot(summ, variable="cf")

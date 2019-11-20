library(devtools)
pkgbuild::has_build_tools(debug = TRUE)
options(buildtools.check = NULL)

## BZ: added this as otherwise does not dowload from the github

devtools::install_github("r-lib/remotes")
.rs.restartR()

CXX14 = "C:/Rtools/mingw_64/bin/g++.exe"
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

datadir <- "/scratch/chris/chronic/mh-mslt/data/city regions/Input disbayes"
fp <- file.path(datadir,"bristol_male_ishd.rds")
dat <- readRDS(fp)

## One example disbayes run - to show how it works
## Bristol, male, IHD
## Fit two models to estimate case fatality
## (1): unsmoothed: case fatalities for different ages estimated independently of each other 
## (2): smoothed: case fatality assumed to be a smooth spline function of age.   Should give more precise estimates, but won't work if the data are weak 

library(disbayes)

resu <- disbayes(dat = dat,
                 
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
summ <- summary(resu) 

## Handy tool to extract specific variables from this 
summary(resu, vars=c("cf","inc")) 

## Plot smoothed and unsmoothed estimates 
plot(resu)

## Plot just smoothed estimates
plot(summ, variable="cf")

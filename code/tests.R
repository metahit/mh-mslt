## Test disbayes with one disease


library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


test_disbayes2 <- readRDS("data/city regions/bristol/dismod/input/Ishd_female.rds")

test_disbayes <- ihdlondon
#
datstan <- c(as.list(test_disbayes), nage=nrow(test_disbayes))
inits <- list(
   list(cf=rep(0.0101, datstan$nage)),
   list(cf=rep(0.0201, datstan$nage)),
   list(cf=rep(0.0056, datstan$nage)),
   list(cf=rep(0.0071, datstan$nage))
)

 gbdcf_test <- stan("disbayes-master/gbdcf-unsmoothed.stan", data=datstan, init=inits)

 gbdcf_test_summary <- summary(gbdcf_test)$summary
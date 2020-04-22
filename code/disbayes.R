# ---- Chunk 1: Disbayes ----
### Disbayes works in linux virtual machine


library(devtools)
library(disbayes)

#### Seems to be working with all data, but I am unable to summrise the data
#### Test one disease first
data <- dplyr::filter(disbayes_inputs_original, sex == "female", disease == "brsc", cityregion == "bristol")
data_new <- dplyr::filter(disbayes_inputs_original, sex == "female", disease == "brsc", cityregion == "bristol")

resu <- disbayes:::disbayes(dat = data_new,
                            
                            ## You can supply either estimates and denominators, or estimates with credible intervals, or numerators and denominators.  See help(disbayes)
                            inc = "inc",
                            inc_denom = "pop",
                            prev_num = "prevn", 
                            prev_denom = "prevdenom",
                            mort = "mort",
                            mort_denom = "pop",
                           
                            ####Dees not work 
                            # inc_num ="num_incidence", 
                            # inc_denom = "denom_incidence", 
                            # mort_num = "num_deaths", 
                            # mort_denom = "denom_deaths",
                            
                            ## You'll need to change this for different diseases:
                            ## the age below which all case fatalities are
                            ## assumed equal in the smoothed model 
                            eqage = 30, 
                            smooth = TRUE  # or FALSE if don't want smoothed estimates
)

sum <- rstan::summary(resu) # should I say resu fit here?
test_output <- as.data.frame(summary(resu, vars("cf", "inc")))

plot(resu)

### Check if the MCMC simulations have converged. The simulated chains should mix together and look like white noise. 

rstan::traceplot(resu$fit, pars=paste0("cf[", 60:65, "]"))

### Look at results with rstan function
summ <- rstan::summary(resu)
head(resu)
plot(resu) + ylab("Case Fatality") + xlab("Age") + ylim(40, 100) 



### Loop to generate disbayes outputs for: diseases, sex and area. Uses GenOutDisbayes. Assumptions and inputs for disbayes
### need changing in the function. 

index <- 1

disbayes_output_list_city_regions <- list()

for (c in c(unique(disbayes_inputs$cityregion))) {
  for (d in c(unique(disbayes_inputs$disease))){
    for (s in c(unique(disbayes_inputs$sex))){

    data <- filter(disbayes_inputs, cityregion == c , disease == d , sex == s)
    
    disbayes_output_list_city_regions[[index]] <- GenOutDisbayes(data)
   
    # names(disbayes_output_list_city_regions)[index] <- paste0(names(disbayes_input_list_city_regions[i]))
    
    # ## Add directly to dibayes input list, first 100 observations? Check with Chris
    # disbayes_output_list[[index]] <- as.data.frame(summary(gbdcf)$summary)[c(1:101, 420:519), c(6,4,8)]
    # 
    # 
    # ## add disease names
    # disbayes_output_list[[index]]$disease <- disease_short_names$sname[d]
    # 
    # ## add sex
    # disbayes_output_list[[index]]$sex <- sex_index
    # 
    # ## create sex and disease category to then join to input for disease life table dataset
    # 
    # disbayes_output_list[[index]]$sex_disease <- paste(sex_index, disease_short_names$sname[d], sep = "_")
    
    index <- index + 1
    }
  }
}



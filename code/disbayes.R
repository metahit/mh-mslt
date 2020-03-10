# ---- Chunk 1: Disbayes ----

library(disbayes)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)  

## Disbayes inputs are saved here: disbayes_inputs. These were generated in script disbayes_data_prep

data <- filter(disbayes_input_beta_agg, cityregion == "bristol", disease == "ishd", sex == "Female")


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


#### Test code

resu <- disbayes(dat = data,
                 
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

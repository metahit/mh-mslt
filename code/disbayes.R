## Run Disbayes
### Disbayes needs input for assumptions for: 1) equal case fatality (e.g. under 30). Model parameter is eqage
### Diseases to model for England city regions paper include: Ischemic heart disease, Tracheal, bronchus, and lung cancer, Chronic obstructive pulmonary disease,
### Stroke, Diabetes mellitus type 2, Breast cancer, Colon and rectum cancer, Alzheimer’s disease and other dementias
### dat below is to be selected and filtered from inputs disbayes and includes: age, sex, inc, pop, prevn, prevdenom, mort, pop, disease, city_region


### Input disbayes is generated in dataprep script

## ADD credible intervals to add to the calculations. Incidence will need intervals calculations using population data. 

dat <-dplyr::select(inputs_disbayes, age, sex, inc, pop, prevn, prevdenom, mort, pop, disease, city_region) %>% 
   dplyr::filter(sex == "female" & disease == "ishd" & city_region =="bristol")

## Create credible intervals using numbers for numerators and denominators. ADD TO DISBAYES INPUT DATA


library(disbayes)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


### Data prep to run code with num and denom
## Test code with prev and denom for one locality using disbayes_input_beta data

data <- filter(disbayes_input_beta_agg, cityregion == "bristol", disease == "adaod", sex == "Female")

## Create column for combination disease and rate type (inc, prev, cf)

data$names <- paste(data$measure, data$disease, sep = "_")

data <- data %>% pivot_wider(id_cols = c(cityregion, sex, agegr, population_number), names_from = names, values_from = c(num, denom))

data <-data[order(data$agegr),]

data$rem <- as.integer(0)
data$prevdenom <- c(100,100,500,500,500,500,500,500,500,500,500,500,500,500,500,500,200,200,100,100) / 10 # total sample size 3910, generous for London (from CJ)

## Added agegroups to derive age groups by 1

data$agegrp <- as.integer(seq(0,95, by=5))

outage <- 0:100  # assume inc/prev/mort same in each year within a five-year age group

ind <- findInterval(outage, data$agegrp)
data <- data[ind,]
data$age <- c(0:100)

data <- within(data, {
  ningrp <- rep(table(agegrp), table(agegrp))
  # popmale <- round(popmale/ningrp) ## assume population uniform between years within age group.
  pop <- round(population_number/ningrp) ## assume population uniform between years within age group.
  # ndieddismale <- round(popmale * (1 - exp(-mortmale)))
  # ndieddis <- round(pop * (1 - exp(-mort)))
  # prevnmale <- round(prevdenom * prevmale)
  prevn <- round(prevdenom * 1)
}
)
## Test code without function (add loop and summary of outcomes as in methahit_belen.r)

### Ask Chris for specific data needs if only including num and denom for incidence and prevalence

resu <- disbayes(dat = data,
                 
                 ## You can supply either estimates and denominators, or estimates with credible intervals, or numerators and denominators.  See help(disbayes)
                 # inc = "inc", 
                 inc_num = "num_incidence_adaod",
                 inc_denom = "denom_incidence_adaod", 
                 # prev_num = "prevn", 
                 #  prev_denom = "prevdenom",
                 # mort = "mort",
                 mort_num = "num_deaths_adaod",
                 mort_denom = "denom_deaths_adaod",
                 
                 
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

### Belen: add code to get what you need for the mlst model (see code in data prep) MAY MOVE TO DATA PREP

## Created a function to run all geographies simultaneously. (ADD TO SAVE IT IN A DATAFRAME TO AVOIND LOOPING OVER LIST)

index <- 1

disbayes_output_list_city_regions <- list()

for (i in 1:length(disbayes_input_list_city_regions)){
  for (j in 1:length(disbayes_input_list_city_regions[[i]])){
    
    
    
    disbayes_output_list_city_regions[[index]] <- GenOutDisbayes(disbayes_input_list_city_regions[[i]][[j]])
    
    # names(disbayes_output_list_city_regions)[index] <- paste0(names(disbayes_input_list_city_regions[i]))
    
    ## Add directly to dibayes input list, first 100 observations? Check with Chris
    disbayes_output_list[[index]] <- as.data.frame(summary(gbdcf)$summary)[c(1:101, 420:519), c(6,4,8)]
    
    
    ## add disease names
    disbayes_output_list[[index]]$disease <- disease_short_names$sname[d]
    
    ## add sex
    disbayes_output_list[[index]]$sex <- sex_index
    
    ## create sex and disease category to then join to input for disease life table dataset
    
    disbayes_output_list[[index]]$sex_disease <- paste(sex_index, disease_short_names$sname[d], sep = "_")
    
    index <- index + 1
    
  }
}


###

### Original code (DELETE)


disbayes_output_list <- list()
index <- 1

for (d in 1:nrow(disease_short_names)){
  for (sex_index in i_sex){
    
    data <- disbayes_input_list_city_regions[[1]][[1]]
    
    # disbayes_input_list[[index]]
    
    if (disease_short_names$is_not_dis[d] == 0){
      
      datstan <- c(as.list(data), nage=nrow(data))
      inits <- list(
        list(cf=rep(0.0101, datstan$nage)),
        list(cf=rep(0.0201, datstan$nage)),
        list(cf=rep(0.0056, datstan$nage)),
        list(cf=rep(0.0071, datstan$nage))
      )
      gbdcf <- stan("disbayes-master/gbdcf-unsmoothed.stan", data=datstan, init=inits)
      
      ## Extract Summary statistics
      
      ## Add directly to dibayes input list, first 100 observations? Check with Chris
      disbayes_output_list[[index]] <- as.data.frame(summary(gbdcf)$summary)[c(1:101, 420:519), c(6,4,8)]
      
      
      ## add disease names
      disbayes_output_list[[index]]$disease <- disease_short_names$sname[d]
      
      ## add sex
      disbayes_output_list[[index]]$sex <- sex_index
      
      ## create sex and disease category to then join to input for disease life table dataset
      
      disbayes_output_list[[index]]$sex_disease <- paste(sex_index, disease_short_names$sname[d], sep = "_")
      
      index <- index + 1
    }
  }
}

# View(disbayes_output_list[[14]])
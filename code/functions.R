# ---- Packages for functions ----

require(dplyr)
require(tidyverse)
require(knitr)
require(kableExtra)
require(citr)

# ---- Explanation method ---- 

# Briefly, the proportional multi-state multi cohort life table consists of a 
# general life table and a life table for each of the modelled diseases.
# The diseases are those associated to the studied risk factor/s. 
# The link between the general life table and disease life tables is via the 
# potential impact fraction (pif), also called paf (population attributable fraction)
# The pif combines exposure to the risk factor and relative risks. The pif is 
# appleid to modify incidence in the individual disease life tables, which in turn
# modify prevalence and mortality. Changes in mortality and prevalence rates
# feed bacak into the general life table to modify total mortality and disability. 
# Changes in total mortality impact on life years and changes in disability impact 
# the disability adjusment of life years. 

# Method reference: 1.	Barendregt JJ, Oortmarssen vGJ, Murray CJ, Vos T. A generic model for the assessment of disease epidemiology: the computational basis of DisMod II. Popul Health Metr. 2003;1(1):4-.
# Naming convention for functions: Function.Name

# ---- Functions ----

## To generate mslt dataframe

### SortGbdInput, RunLocDf, IsNanDataFrame & IsInfDataFrame

## To run MSLT

### RunLifeTable, RunDisease, RunNonDisease,  RunPif, Complete

## To generate outputs

### RunOutput

## To run alernative calculations

# ---- SortGbdInput ----
## Not used, but, can be used if we want to get the data for one specific location only. 
## Selects year and localities from GBD data frame dowloaded from: http://ghdx.healthdata.org/gbd-results-tool

### ADD LIST to do the city regions, and then localities within them

SortGbdInput <- function(in_data, in_year, in_locality) {
  data <- in_data[which(in_data$year== in_year & in_data$location == in_locality),]

}

# --- RunLocDf ----

## Organises GBD data per locality to tidy format with columns for variable names (e.g. age, sex, disease-cause, disease-metrics) and calculates population numbers.
## Also generates rates for localities, which we may use in the future when modelling per localities. 


## RunLocDF
RunLocDf <- function(i_data) {
  
  gbd_df <- NULL 
  
  for (ag in 1:length(unique(i_data[["age"]]))){
    for (gender in c("Male", "Female")){
      age_sex_df <- NULL
      for (dm in 1:length(disease_measures_list)){
        for (d in 1:nrow(disease_short_names)){
          dn <- disease_short_names$disease[d]
          dmeasure <- disease_measures_list[dm] %>% as.character()
          
          agroup <- unique(i_data[["age"]])[ag]
          
          idf <- dplyr::filter(i_data, sex == gender & age == agroup & measure == dmeasure & cause == dn) 
          
          if (nrow(idf) > 0){
            
            population_numbers <- dplyr::filter(idf, metric == "Number") %>% select("val", "lower", "upper")
            
            idf_rate <- dplyr::filter(idf, metric == "Rate") %>% select("val") 
            
            current_idf_rate <- idf_rate
            
            current_population_numbers <- population_numbers
            
            idf$population_number <- 0
            
            if (idf_rate$val != 0 && population_numbers$val != 0)
              idf$population_number <- (100000 * population_numbers$val) / idf_rate$val
            
            else{
              
              current_idf_rate <- idf_rate
              
              current_population_numbers <- population_numbers
              
              idf <- dplyr::filter(i_data, sex == gender & age == agroup & measure == dmeasure & val > 0) 
              
              idf <- dplyr::filter(idf, cause == unique(idf$cause)[1])
              
              idf$cause <- dn
              
              population_numbers <- dplyr::filter(idf, metric == "Number") %>% select("val", "lower", "upper")
              #, "lower", "upper")
              
              idf_rate <- dplyr::filter(idf, metric == "Rate") %>% select("val") 
              
              idf$population_number <- 0
              
              if (idf_rate$val != 0 && population_numbers$val != 0)
                idf$population_number <- (100000 * population_numbers$val) / idf_rate$val
              
            }
            
            
            idf$rate_per_1 <- round(current_idf_rate$val / 100000, 6)
            
            
            idf[[tolower(paste(dmeasure, "rate", disease_short_names$sname[d], sep = "_"))]] <- idf$rate_per_1
            
            idf[[tolower(paste(dmeasure, "med", disease_short_names$sname[d], sep = "_"))]] <- current_population_numbers$val
            idf[[tolower(paste(dmeasure, "lower95", disease_short_names$sname[d], sep = "_"))]] <- current_population_numbers$lower
            idf[[tolower(paste(dmeasure, "upper95", disease_short_names$sname[d], sep = "_"))]] <- current_population_numbers$upper

            
            
            idf <- dplyr::filter(idf, metric == "Number")
            
            if (is.null(age_sex_df)){
              
              age_sex_df <- select(idf, age, sex, population_number, location, names(idf)[ncol(idf) - 2], names(idf)[ncol(idf) - 1] , names(idf)[ncol(idf)])
              
              
              names(idf)[ncol(idf)]
              names(idf)[ncol(idf) - 1]
            }
            else{
              
              age_sex_df <- cbind(age_sex_df, select(idf, names(idf)[ncol(idf) - 2], names(idf)[ncol(idf) - 1] , names(idf)[ncol(idf)]))
              
              # browser()
            }
          }
        }
      }
      
      if (is.null(gbd_df)){
        
        gbd_df <- age_sex_df
      }
      else{
        
        age_sex_df[setdiff(names(gbd_df), names(age_sex_df))] <- 0
        gbd_df[setdiff(names(age_sex_df), names(gbd_df))] <- 0
        
        gbd_df <- rbind(gbd_df, age_sex_df)
        
      }
    }
  }
  return(gbd_df)
}

# --- GenInpDisbayes ----

GenInpDisbayes <- function(i_data) {

disbayes_input_list <- list()
index <- 1

for (d in 1:nrow(disease_short_names)){
  for (sex_index in i_sex){
    
    if (disease_short_names$is_not_dis[d] == 0){
      
      var_name <- paste0("rate_", disease_short_names$sname[d])
      
      disbayes_input_list[[index]] <- dplyr::filter(i_data, sex == sex_index) %>% select(age, sex, ends_with(var_name), population_number)
      
      ## Add column to show disease
      
      disbayes_input_list[[index]]$disease <- disease_short_names$sname[d]
      
      ## Change column names to match disbayes code (THIS WILL CHANGE AS WE CAN USE NUMBERS DIRECTLY, WITH POPULATION DATA)
      
      colnames(disbayes_input_list[[index]])[colnames(disbayes_input_list[[index]])== tolower(paste0("incidence_rate_", disease_short_names$sname[d]))] <- "inc"
      colnames(disbayes_input_list[[index]])[colnames(disbayes_input_list[[index]])== tolower(paste0("deaths_rate_", disease_short_names$sname[d]))] <- "mort"
      colnames(disbayes_input_list[[index]])[colnames(disbayes_input_list[[index]])== tolower(paste0("prevalence_rate_", disease_short_names$sname[d]))] <- "prev"
      colnames(disbayes_input_list[[index]])[colnames(disbayes_input_list[[index]])== paste0("population_number")] <- "pop"
      
      ## We assume remission is 0
      
      disbayes_input_list[[index]]$rem <- as.integer(0)
      
      ## create denominator for disbayes code
      
      disbayes_input_list[[index]]$prevdenom <- c(100,100,500,500,500,500,500,500,500,500,500,500,500,500,500,500,200,200,100,100) / 10 # total sample size 3910, generous for London (from CJ)
      
      ## Added agegroups to derive age groups by 1
      
      disbayes_input_list[[index]]$agegrp <- as.integer(seq(0,95, by=5))
      
      ## Replace 0 with small numbers for incidence, otherwise, disbayes does not work.
      
      disbayes_input_list[[index]]$inc <- ifelse(disbayes_input_list[[index]]$inc  == 0, 1e-08, disbayes_input_list[[index]]$inc)
      
      
      ## Convert 5 year data file to 100 year age intervals
      
      
      outage <- 0:100  # assume inc/prev/mort same in each year within a five-year age group
      
      ind <- findInterval(outage, disbayes_input_list[[index]]$agegrp)
      disbayes_input_list[[index]] <- disbayes_input_list[[index]][ind,]
      disbayes_input_list[[index]]$age <- outage
      
      disbayes_input_list[[index]] <- within(disbayes_input_list[[index]], {
        ningrp <- rep(table(agegrp), table(agegrp))
        # popmale <- round(popmale/ningrp) ## assume population uniform between years within age group.
        pop <- round(pop/ningrp) ## assume population uniform between years within age group.
        # ndieddismale <- round(popmale * (1 - exp(-mortmale)))
        ndieddis <- round(pop * (1 - exp(-mort)))
        # prevnmale <- round(prevdenom * prevmale)
        prevn <- round(prevdenom * prev)
      }
      )
      
      ## add sex and disease variable to match with output data frame
      
      disbayes_input_list[[index]]$sex_disease <- paste(sex_index, disease_short_names$sname[d], sep = "_")
      
      index <-  index +1
      }
    }
  }
  return(disbayes_input_list)
}


# --- GenOutDisbayes ----

### All these came up as conflicts, check with Chris. 

conflict_prefer("chisq.test", "stats")
conflict_prefer("combine", "dplyr")
conflict_prefer("dim_desc", "dplyr")
conflict_prefer("extract", "rstan")
conflict_prefer("fisher.test", "stats")
conflict_prefer("group_rows", "dplyr")
conflict_prefer("lag", "stats")
conflict_prefer("Position", "ggplot2")

### test data

# test_path <-  paste0(relative_path_mslt, "disbayes-master/gbdcf-unsmoothed.stan")
# 
# data_test <- disbayes_input_list_city_regions[[1]][[1]]
#  
# test_list_output <- GenOutDisbayes(data_test)


#### CODE for packaged disbayes

GenOutDisbayes <- function(i_data) {
  
    disbayes_output_list <- list()
    index_f <- 1
    
    for (d in 1:nrow(disease_short_names)){
      for (sex_index in i_sex){
        
        data <- i_data
        if (disease_short_names$is_not_dis[d] == 0){
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

## Posterior medians and 95% credible intervals for all unknowns in the model
disbayes_output_list[[index_f]] <- summary(resu)

disbayes_output_list[[index_f]]$area <- i_data$city_region
disbayes_output_list[[index_f]]$sex <- i_data$sex
disbayes_output_list[[index_f]]$disease <- i_data$disease


index_f <- index_f + 1
        }
      }
    }
    return(disbayes_output_list)
}


#### OLD CODE

GenOutDisbayes <- function(i_data) {


disbayes_output_list <- list()
index_f <- 1

for (d in 1:nrow(disease_short_names)){
  for (sex_index in i_sex){
    
    data <- i_data
    
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
      disbayes_output_list[[index_f]] <- as.data.frame(summary(gbdcf)$summary)[c(1:101, 420:519), c(6,4,8)]
      
      
      ## add disease names
      disbayes_output_list[[index_f]]$disease <- disease_short_names$sname[d]
      
      ## add sex
      disbayes_output_list[[index_f]]$sex <- sex_index
      
      ## create sex and disease category to then join to input for disease life table dataset
      
      disbayes_output_list[[index_f]]$sex_disease <- paste(sex_index, disease_short_names$sname[d], sep = "_")
      
      index_f <- index_f + 1
      }
    }
  }
  return(disbayes_output_list)
}


# --- IsNanDataFrame ---
## For interpolation generation, input data frame for interpolation has nan values that we replace with 0. 

IsNanDataFrame <- function(x)
  do.call(cbind, lapply(x, is.nan))

# --- IsInfDataFrame ---
## For interpolation generation, input data frame for interpolation has inf values that we replace with 0. 
IsInfDataFrame <- function(x)
  do.call(cbind, lapply(x, is.infinite))

# ---- RunLifeTable ----

## Function to generate age and sex life table for baseline and scenario.

RunLifeTable <- function(in_idata, in_sex, in_mid_age)
{
  
  # Create a life table starting data frame from input data. 
  
  lf_df <- in_idata[in_idata$age >= in_mid_age & in_idata$sex == in_sex,] 
  lf_df <- lf_df[,colnames(lf_df) %in% c('sex', 'age', 'pyld_rate', 'mx')]
  
  # Create list life table variables. First as vector and then added to the data frame at the end.
  ## We model up to 100, that is the reason for the age limit in the function
  
  # probability of dying

  qx <-  ifelse(lf_df$age < 100, 1 - exp(-1 * lf_df$mx), 1)
  
  # number of survivors year 1 simulation
  
  num_row <- nrow(lf_df)
  lx <- rep(0,num_row)
  lx[1] <- as.numeric(in_idata$population_number[in_idata$age == in_mid_age & in_idata$sex == in_sex]) 
  
  # number died in year 1 simulation
  
  dx <- rep(0,num_row)
  dx[1] <- lx[1] * qx[1]
  
  # number of survivors and who die from year 2 onwards. 
  
  for (i in 2:num_row){
    lx[i] <- lx[i - 1] - dx[i - 1]
    dx[i] <- lx[i] * qx[i]
  }
  
  # number of persons lived by cohort to age x + 1/2 (average people)
 
  Lx <- rep(0,num_row)
  
  # for years up to 99
  
  for (i in 1:(num_row-1))
    Lx[i] <- (lx[i] + lx[i + 1]) / 2
  
  # for year 100, cohort dies at 100 if anyone left
  
  Lx[num_row] <- lx[num_row] / lf_df$mx[num_row]
  
  
  # create life expectancy variable
  ex <- rep(0,num_row)
  for (i in 1:num_row){
    ex[i] <- sum(Lx[i:num_row]) / lx[i]
  }
  
  # create health adjusted life years variable 
  
  Lwx <- Lx * (1 - lf_df$pyld_rate)
  
  # create health adjusted life expectancy variable
  ewx <- rep(0,num_row)
  for (i in 1:num_row){
    ewx[i] <- sum(Lwx[i:num_row]) / lx[i]
  }
  
  lf_df$qx <- qx
  lf_df$lx <- lx
  lf_df$dx <- dx
  lf_df$Lx <- Lx
  lf_df$ex <- ex
  lf_df$Lwx <- Lwx
  lf_df$ewx <- ewx
  lf_df
}

# ---- RunDisease ----

## Function to generate age and sex disease life table for baseline and scenario.
## Remission is not modelled.

RunDisease <- function(in_idata, in_mid_age, in_sex, in_disease) 
  
{
  
  
  # create disease variable for the disease life table function 
  dw_disease <- paste("dw_adj", in_disease, sep = "_")
  incidence_disease <- paste("incidence", in_disease, sep = "_")
  case_fatality_disease <- paste("case_fatality", in_disease, sep = "_")
  
  ## add generic variable names to the source data frame (in_idata)
  in_idata$dw_disease <- in_idata[[dw_disease]]
  in_idata$incidence_disease <- in_idata[[incidence_disease]]
  in_idata$case_fatality_disease <- in_idata[[case_fatality_disease]]
  
  # Select columns for lifetable calculations
  
  ##BZ: back yo using filtering, otherwise the life tables are not run by cohort (age and sex)
  dlt_df <- dplyr::filter(in_idata, age >= in_mid_age & sex == in_sex) %>% 
    select(sex, age, dw_disease, incidence_disease, case_fatality_disease)
  
  ##BZ: Rob, line 264 does not filter by age and sex, each disease life table starts at firt age cohort (e.g. 17) and by gender. 
  
  # dlt_df <- in_idata[,colnames(in_idata) %in% c('sex', 'age', 'dw_disease', 'incidence_disease', 'case_fatality_disease')] # dplyr::select(sex, age, dw_disease, incidence_disease, case_fatality_disease)
  
  dlt_df$disease <- in_disease
  
  # create list of life table variables. Created as vectors and then added to dataframe. 
  # See see methods in: 1) Concept and original calculations: Barendregt, J. J., et al. (1998). "Coping with multiple morbidity in a life table." Math Popul Stud 7(1): 29-49. 
  # and 2) Latest version, variables below calculated from it: Barendregt, J. J., et al. (2003). "A generic model for the assessment of disease epidemiology: the computational basis of DisMod II." Popul Health Metr 1(1): 4-4.
  
  
  ### lx, qx, wx and vx are intermediate variables, 
  
  lx <- dlt_df$incidence_disease + dlt_df$case_fatality_disease
  qx <-  sqrt((dlt_df$incidence_disease - dlt_df$case_fatality_disease) * (dlt_df$incidence_disease - dlt_df$case_fatality_disease))
  wx <- exp(-1*(lx+qx)/2)
  vx <- exp(-1*(lx-qx)/2)
  
  ## Healthy (Sx), Disease (Cx) and Death (Dx), total (Tx) (control check, has to be 1000), total alive (Ax)
  ## persons years live at risk (PYx), prevalence rate (px), mortality rate (mx)
  ## Remission and mortality from other causes were replaced by zero in the formulas (as we assume no remission and independence of disease mortality with total mortlaity). 
  
  ### First create empty variables
  
  number_of_ages <- nrow(dlt_df)
  Sx <- Cx <- Dx <- Tx  <- Ax <- PYx <- px <- mx <- rep(0,number_of_ages)
  cfds <- dlt_df$case_fatality_disease
  ages <- dlt_df$age
  
  #### Starts with 1000 healthy (Sx) and total (Ax) people. 
  
  Sx[1] <- Ax[1] <- 1000
  
  ##### start with variables without calculation exceptions
  
  ##### variables without exceptions (else includes exception for year one of the simulation)  
  for (i in 2:(number_of_ages-1)){ ##!! this can go to "number_of_ages" now (?)
    if(qx[i-1] > 0){
      
      ### The following five variables are created to simplify Sx, Cx and Dx calculations, and do not form part of the disease life tables.
      vxmwx <- vx[i-1] - wx[i-1]
      SxpCx <- Sx[i-1]+Cx[i-1]
      dqx <- 2 * qx[i-1]
      qxmlx <- qx[i-1] - lx[i-1]
      qxplx <- qx[i-1] + lx[i-1]
      
      ### Healthy (Sx), Diseases (Cx) and Death from the Disease (Dx)
      
      Sx[i] <- Sx[i-1] * (2*vxmwx * cfds[i-1]  + (vx[i-1] * qxmlx + wx[i-1] * qxplx)) / dqx
      Cx[i] <- -1*(vxmwx*(2*(cfds[i-1]  * SxpCx - lx[i-1] * Sx[i-1]) - Cx[i-1] * lx[i-1]) - Cx[i-1] * qx[i-1] * (vx[i-1]+wx[i-1])) / dqx
      Dx[i] <- (vxmwx * (2 * cfds[i-1] * Cx[i-1] - lx[i-1]*SxpCx)- qx[i-1] * SxpCx*(vx[i-1]+wx[i-1]) + dqx * (SxpCx+Dx[i-1]) ) / dqx
    }else{
      Sx[i] <- Sx[i - 1] 
      Cx[i] <- Cx[i - 1]
      Dx[i] <- Dx[i - 1]
    }
  }
  

  Tx   <- Sx + Cx + Dx 
  Ax <- Sx + Cx
  
  first_indices <- 1:(number_of_ages-1)
  last_indices <- 2:number_of_ages
  PYx <- (Ax[first_indices] + Ax[last_indices])/2
  mx[first_indices] <- (Dx[last_indices] - Dx[first_indices])/PYx[first_indices]
  mx[mx<0] <- 0
  px[first_indices] <- (Cx[last_indices] + Cx[first_indices])/2/PYx[first_indices]
  
  dlt_df$Tx <- Tx
  dlt_df$mx <- mx
  dlt_df$px <- px
  dlt_df
}



# RunNonDisease

RunNonDisease <- function(in_idata, in_sex, in_mid_age, in_non_disease)
  
{
  df <- in_idata[,colnames(in_idata) %in% c('sex', 'age',  paste0("deaths_rate_", in_non_disease), paste0("ylds_rate_", in_non_disease))]
  
  ## Delete names disease from columns to simplify calculations
  
  names(df)[names(df) == paste0("deaths_rate_", in_non_disease)] <-
    paste("deaths_rate_bl")
  
  names(df)[names(df) == paste0("ylds_rate_", in_non_disease)] <-
    paste("ylds_rate_bl")
  
  
  return(df)
}



# ---- PlotOutput ----

# Function to generate graphs by age and sex, per outcome of interest. 
# 
# 
# PlotOutput <- function(in_data, in_age, in_population, in_outcomes, in_legend = "", in_disease = ""){
#   
#   # in_data <- output_df
#   # in_population <- "male"
#   # in_age <- 22
#   # in_outcomes <- c('age', 'inc_num_bl_ihd', 'inc_num_sc_ihd')
#   # in_legend <- "none"
#   # in_cols <- c('alpha', 'beta')
#   
#   data <- in_data
#   
#   if (in_population != "total")
#     data <- dplyr::filter(data, sex == in_population)
#   if (length(in_age) > 0)
#     data <- dplyr::filter(data, age_cohort == in_age)
#   if (length(in_outcomes) > 0)
#     data <- select(data, in_outcomes)
#   
#   td <- data
#   p <- ggplot(data = td, aes (x = td[[in_outcomes[[1]]]]))
#   
#   # loop
#   for (i in 2:length(in_outcomes)) {
#     # use aes_string with names of the data.frame
#     p <- p + geom_line(aes_string(y = td[[in_outcomes[i]]], color = as.factor(in_outcomes[i])), size = 0.8) +
#       
#       theme_classic() 
#     
#     
#   }
#   
#   p <- p + scale_color_discrete(name = paste(in_legend), labels = c("Baseline", "Difference", "Scenario")) +
#     theme(legend.title = element_text(size = 9))
#   
#   p <- p + xlab ('Simulation year') + ylab ('Cases') + labs (title = ifelse(length(in_disease) > 0, paste(in_age, in_population, in_disease, sep = " "), "")) +
#                                                                 
#                                                                 
#                                                                 
#                                                                 # in_disease, paste('Cohort', in_age, "years old", in_population, sep = " "))) +
#     theme(plot.title = element_text(hjust = 0.5, size = 9)) +
#     theme(legend.text = element_text(size = 9)) +
#     # theme(axis.title.x = element_text(size = 7)) +
#     xlim(in_age, 100) +
#     geom_hline(yintercept=0, linetype="dashed", color = "black")
#   
#   
#   return(p)
#   
#   last_plot()
#   
#   
# }
# install.packages("compiler")
# require(compiler)   # for byte code compilation
# PlotOutput_compiled <- cmpfun(PlotOutput)
# ggsave_compiled <- cmpfun(ggsave)

# system.time(PlotOutput_compiled())
# system.time(PlotOutput())

# ---- GenAggregate ----
# Function to aggreate outcomes by age an sex


GenAggregate <- function(in_data, in_cohorts, in_population, in_outcomes){
  
  
  # in_data <- output_df
  # in_population <- "males"
  # in_cohorts <- 10
  # in_outcomes <- c('inc_num_bl_ihd', 'inc_num_sc_ihd')
  
  age_cohort_list <- list()
  td <- in_data
  aggr <- list()
  l_age <-  min(td$age_cohort)
  for (i in 1:in_cohorts){
    if (l_age <= 100){
      ld <- dplyr::filter(td, age_cohort == l_age)
      
      if (in_population != "total")
        ld <- dplyr::filter(ld, sex == in_population)
      if (length(in_outcomes) > 0)
        ld <- select(ld, age, sex, in_outcomes)
      if (i == 1){
        aggr <- append(aggr, as.list(ld))
        aggr <- as.data.frame(aggr)
        names(aggr) <- paste(names(aggr), l_age, in_population, sep = "_" )
      }
      else {
        n_rows <-  nrow(aggr) - nrow(ld)
        ld[(nrow(ld) + 1):(nrow(ld) + n_rows),] <- NA
        names(ld) <- paste(names(ld), l_age, in_population, sep = "_" )
        aggr <- cbind(aggr, ld)
      }
      
      l_age <- l_age + 5
    }
  }
  
  for (i in 1:length(in_outcomes)){
    aggr[[paste0("total_",in_outcomes[i])]] <- select(aggr, starts_with(in_outcomes[i])) %>% rowSums(na.rm = T)
    
  }
  
  aggr
}

# ---- GridArrangSharedLegend ----
# Function to general combined labels for multiple plots in a page

GridArrangSharedLegend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right"), mainTitle = "", mainLeft = "", mainBottom = "") {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow, top = mainTitle, left = mainLeft, bottom = mainBottom)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  
  #grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
  
}


g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# ---- GetQualifiedDiseaseName ---- (Belen: check if we need this function)
# Function to get qualified names diseases

GetQualifiedDiseaseName <- function (disease){
  if (disease == 'ihd')
    return ('Ischaemic Heart Disease')
  else if (disease == 'bc')
    return ('Breast Cancer')
  else if (disease == 'dm')
    return ('Diabetes')
  else if (disease == 'cc')
    return ('Colon cancer')
  else if (disease == 'is')
    return ('Ischemic stroke')
}

# ---- PlotGBD (may need to update) ----
# Function to generate GBD graphs to compare data national to local (USED in GBD COMPARE############################

PlotGBD <- function(in_data1, in_data2, in_sex, in_cause, in_measure) {
  
  # in_data1 <- GBDEngland
  # in_data2 <- GBDGL
  # in_sex <- "male"
  # in_cause <- "all causes"
  # in_measure <- "deaths"
  
  
  data1 <- dplyr::filter(in_data1, sex == in_sex, cause == in_cause & measure == in_measure) %>% select(measure, location, sex, age, metric, cause, one_rate, age_cat)     
  
  data2 <- dplyr::filter(in_data2, sex == in_sex, cause == in_cause & measure == in_measure) %>% select(measure, location, sex, age, metric, cause, one_rate, age_cat)     
  
  
  p <- ggplot(data = data1, aes(age_cat,one_rate)) +
    geom_line(aes(color = "England"))+
    geom_line(data = data2, aes(color = "Greater London"))+
    labs(colour="Locations",x="Age",y= paste(in_cause, in_measure, sep = " "))+
    labs (title = paste("Compare", in_cause, in_measure, in_sex, sep = " "), size=14) + 
    theme_classic()
  print(p)
}


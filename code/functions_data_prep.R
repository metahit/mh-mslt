# ---- Packages for functions ----
library(devtools)
library(dplyr)
library(tidyverse)
library(citr)
library(disbayes)


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
        for (d in 1:nrow(DISEASE_SHORT_NAMES)){
          dn <- DISEASE_SHORT_NAMES$disease[d]
          dmeasure <- disease_measures_list[dm] %>% as.character()
          
          agroup <- unique(i_data[["age"]])[ag]
          
          idf <- dplyr::filter(i_data, sex == gender & age == agroup & measure == dmeasure & cause == dn) 
          
          if (nrow(idf) > 0){
            
            population_numbers <- dplyr::filter(idf, metric == "Number") %>% dplyr::select("val", "lower", "upper")
            
            idf_rate <- dplyr::filter(idf, metric == "Rate") %>% dplyr::select("val") 
            
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
              
              population_numbers <- dplyr::filter(idf, metric == "Number") %>% dplyr::select("val", "lower", "upper")
              #, "lower", "upper")
              
              idf_rate <- dplyr::filter(idf, metric == "Rate") %>% dplyr::select("val") 
              
              idf$population_number <- 0
              
              if (idf_rate$val != 0 && population_numbers$val != 0)
                idf$population_number <- (100000 * population_numbers$val) / idf_rate$val
              
            }
            
            
            idf$rate_per_1 <- round(current_idf_rate$val / 100000, 6)
            
            
            idf[[tolower(paste(dmeasure, "rate", DISEASE_SHORT_NAMES$sname[d], sep = "_"))]] <- idf$rate_per_1
            
            idf[[tolower(paste(dmeasure, "med", DISEASE_SHORT_NAMES$sname[d], sep = "_"))]] <- current_population_numbers$val
            idf[[tolower(paste(dmeasure, "lower95", DISEASE_SHORT_NAMES$sname[d], sep = "_"))]] <- current_population_numbers$lower
            idf[[tolower(paste(dmeasure, "upper95", DISEASE_SHORT_NAMES$sname[d], sep = "_"))]] <- current_population_numbers$upper

            
            
            idf <- dplyr::filter(idf, metric == "Number")
            
            if (is.null(age_sex_df)){
              
              age_sex_df <- dplyr::select(idf, age, sex, population_number, location, cityregion, names(idf)[ncol(idf) - 2], names(idf)[ncol(idf) - 1] , names(idf)[ncol(idf)])
              
              
              names(idf)[ncol(idf)]
              names(idf)[ncol(idf) - 1]
            }
            else{
              
              age_sex_df <- cbind(age_sex_df, dplyr::select(idf, names(idf)[ncol(idf) - 2], names(idf)[ncol(idf) - 1] , names(idf)[ncol(idf)]))
              
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

# --- Ci2NumDF ----

Ci2NumDF <- function(in_data) {
  
  dataframe <- dplyr::select(in_data, population_number, est, lower, upper, sex_age_cat, cityregion, indexagg)  %>%
    
    dplyr::select(a=population_number,b= est,c= lower,d=upper, e= sex_age_cat, f=cityregion, h=indexagg) %>%
    rowwise() %>%
    
    # browser()
    
    mutate(num=ifelse(b==0,0,disbayes:::ci2num(b/a,c/a,d/a)[[1]])) %>%
    mutate(denom=ifelse(b==0,0,disbayes:::ci2num(b/a,c/a,d/a)[[2]])) %>%
    mutate(population_number = a) %>%
    mutate(sex_age_cat = e) %>%
    mutate(cityregion = f) %>%
    mutate(indexagg = h) %>%
    
    dplyr::select(population_number, indexagg, num, denom) %>%
    as.data.frame()
  
}


# --- RemoveAllWs ---

RemoveAllWs<- function(string){
  return(gsub(" ", "", str_squish(string)))
}

# --- GenInpDisbayes ----


GenInpDisbayes <- function(i_data) {
  
  disbayes_input_list <- list()
  index <- 1
  
  for (d in 1:nrow(DISEASE_SHORT_NAMES)){
    for (sex_index in i_sex){
      
      ### hard coded here, these diseases do not have incidence or prevalence, so need to exclude
      if (DISEASE_SHORT_NAMES$is_not_dis[d] != 0) {}
      else {
        
        var_name <- paste0("rate_", DISEASE_SHORT_NAMES$sname[d])
        
        disbayes_input_list[[index]] <- dplyr::filter(i_data, sex == sex_index) %>% dplyr::select(age_cat, sex, ends_with(var_name), population_number)
        
        ## Add column to show disease
        
        disbayes_input_list[[index]]$disease <- paste0(DISEASE_SHORT_NAMES$sname[d])
      
        
        
        colnames(disbayes_input_list[[index]])[colnames(disbayes_input_list[[index]])== tolower(paste0("incidence_rate_", 
                                                                                                       DISEASE_SHORT_NAMES$sname[d]))] <- "inc"
        
        
        colnames(disbayes_input_list[[index]])[colnames(disbayes_input_list[[index]])== tolower(paste0("deaths_rate_", 
                                                                                                       DISEASE_SHORT_NAMES$sname[d]))] <- "mort"      
        
        
        colnames(disbayes_input_list[[index]])[colnames(disbayes_input_list[[index]])== tolower(paste0("prevalence_rate_", 
                                                                                                       DISEASE_SHORT_NAMES$sname[d]))] <- "prev"
        
        ## Drop columns with measures disease names combinations
        
        
        
        colnames(disbayes_input_list[[index]])[colnames(disbayes_input_list[[index]])== paste0("population_number")] <- "pop"
        
        ## We assume remission is 0
        
        disbayes_input_list[[index]]$rem <- as.integer(0)
        
        ## create denominator for disbayes code
        
        disbayes_input_list[[index]]$prevdenom <- c(100,100,500,500,500,500,500,500,500,500,500,500,500,500,500,500,200,200,100,100) / 10 # total sample size 3910, generous for London (from CJ)
        
        ## Added age_catgroups to derive age groups by 1
        
        disbayes_input_list[[index]]$agegrp <- as.integer(seq(0,95, by=5))
        
        ## Replace 0 with small numbers for incidence, otherwise, disbayes does not work.
        
        disbayes_input_list[[index]]$inc <- ifelse(disbayes_input_list[[index]]$inc  == 0, 1e-08, disbayes_input_list[[index]]$inc)
        
        
        
        ## Convert 5 year data file to 100 year age intervals
        
        
        outage <- 0:100  # assume inc/prev/mort same in each year within a five-year age group
        
        ind <- findInterval(outage, disbayes_input_list[[index]]$agegrp)
        disbayes_input_list[[index]] <- disbayes_input_list[[index]][ind,]
        disbayes_input_list[[index]]$age_cat <- outage
        
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
        
        disbayes_input_list[[index]]$sex_disease <- paste(sex_index, DISEASE_SHORT_NAMES$sname[d], sep = "_")
        
        disbayes_input_list[[index]] <- disbayes_input_list[[index]][ -c(4) ]
        # browser()
        # 
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
conflict_prefer("colsplit", "reshape2")
conflict_prefer("expand", "tidyr")
conflict_prefer("melt", "reshape2")
conflict_prefer("recast", "reshape2")
conflict_prefer("rename", "dplyr") 

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
  for (d in 1:nrow(DISEASE_SHORT_NAMES)){
    for (sex_index in i_sex){
      if (DISEASE_SHORT_NAMES$is_not_dis[d] == 0 || DISEASE_SHORT_NAMES$acronym[d] == 'no_pif' || DISEASE_SHORT_NAMES$acronym[d] == 'other'){
      }
      else {
        data <- i_data
        resu <- disbayes:::disbayes(dat = data,
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
        disbayes_output_list[[index_f]] <- disbayes:::summary.disbayes(resu)
        # disbayes_output_list[[index_f]]$area <- i_data$cityregion
        # disbayes_output_list[[index_f]]$sex <- i_data$sex
        # disbayes_output_list[[index_f]]$disease <- i_data$disease
        index_f <- index_f + 1
      }
    }
  }
  return(disbayes_output_list)
}

# ---- GenMSLTDF ----

## This function uses gbd sorted data and disbayes output to generate a dataframe for each of the areas (city regions).
## In principle any data can be sorted with this code (gbd_data is the result of aggregating local authority areas to city regions, but if there is nothing to aggregate works, eg. England)
## The code only generates inputs for diseases with pifs from mh-exectute. We may more diseases from new meta analysis and this script is
## problematic when there is no data. For example, marjor depressive disorders have no death data. 

GenMSLTDF <- function(i_data, d_data) { 
  
  mslt_df <- data.frame(age = rep(c(0:100), 2), sex = append(rep("male", 101), 
                                                             rep("female", 101)))
  
  ## Add age groups for cohort modelling
  
  mslt_df$age_cat [mslt_df$age == 2] <- 2
  mslt_df$age_cat [mslt_df$age == 7] <- 7
  mslt_df$age_cat [mslt_df$age == 12] <- 12
  mslt_df$age_cat [mslt_df$age == 17] <- 17
  mslt_df$age_cat [mslt_df$age == 22] <- 22
  mslt_df$age_cat [mslt_df$age == 27] <- 27
  mslt_df$age_cat [mslt_df$age == 32] <- 32
  mslt_df$age_cat [mslt_df$age == 37] <- 37
  mslt_df$age_cat [mslt_df$age == 42] <- 42
  mslt_df$age_cat [mslt_df$age == 47] <- 47
  mslt_df$age_cat [mslt_df$age == 52] <- 52
  mslt_df$age_cat [mslt_df$age == 57] <- 57
  mslt_df$age_cat [mslt_df$age == 62] <- 62
  mslt_df$age_cat [mslt_df$age == 67] <- 67
  mslt_df$age_cat [mslt_df$age == 72] <- 72
  mslt_df$age_cat [mslt_df$age == 77] <- 77
  mslt_df$age_cat [mslt_df$age == 82] <- 82
  mslt_df$age_cat [mslt_df$age == 87] <- 87
  mslt_df$age_cat [mslt_df$age == 92] <- 92
  mslt_df$age_cat [mslt_df$age == 97] <- 97
  
  ## Add population numbers (here we can choose, from GBD derived or directly from synthetic population)
  
  mslt_df$sex_age_cat <- paste(mslt_df$sex,mslt_df$age, sep = "_"  )
  
  
  ## GBD population
  
  ### HERE start a list where each area has its own data gbd data frame
  
  gbd_df <- i_data
  disease <- d_data
  
  
  ### selected data here should be gbd_data with all data, see how the code works with it 
  
  gbd_popn_df <- dplyr::select(gbd_df, population_number, sex_age_cat, area)
  
  ## Synthetic population (TO DO)
  
  # synthetic_pop <- read_csv("data/population/pop_england_2017.csv")
  
  ### Here add disbayes output data frame
  #### Add disbayes output dataframe with an area index to match GBD data frame. 
  mslt_df <- left_join(mslt_df, gbd_popn_df, by = "sex_age_cat", keep = FALSE)
  
  mslt_df$area <- gbd_popn_df$area[[1]]
  
  mslt_df$sex_age_area_cat <- paste(mslt_df$sex,mslt_df$age, mslt_df$area, sep = "_"  )
  
  
  
  # ---- chunk-6.1: Interpolate rates ---- (CHECK WITH ROB WHERE THESE RATES CALCS SHOULD GO IF WE WANT TO INCLUDE UNCERT PARAMETERS)
  
  # ---- chunk-2.5: Disability weights ---- CHANGE THIS FURTHER DOWN, DOES NOT MAKE MUCH SENSE HERE (BELONGS TO MSLT, NOT DISBAYES/DISMOD)
  
  all_ylds_df <- dplyr::select(gbd_df, starts_with("ylds (years lived with disability"))
  
  
  ## Adjust all cause ylds for included diseases and injuries (exclude all cause ). From here just med 
  
  gbd_df[["allc_ylds_adj_rate_1"]] <- (gbd_df$`ylds (years lived with disability)_med_allc`  - rowSums(dplyr::select(all_ylds_df, -`ylds (years lived with disability)_med_allc`))) / 
    gbd_df$population_number
  
  # ------------------- DWs ---------------------------#
  
  DISEASE_SHORT_NAMES <- mutate_all(DISEASE_SHORT_NAMES, funs(tolower))
  
  
  
  for (d in 1:nrow(DISEASE_SHORT_NAMES)){
    gbd_df[[paste0("dw_adj_", DISEASE_SHORT_NAMES$sname[d])]] <- 
      (gbd_df[[paste0("ylds (years lived with disability)_med_", DISEASE_SHORT_NAMES$sname[d])]] /
         gbd_df[[paste0("prevalence_med_", DISEASE_SHORT_NAMES$sname[d])]]) /
      ( 1 - gbd_df[["allc_ylds_adj_rate_1"]])
  }
  
  gbd_df[mapply(is.infinite, gbd_df)] <- 0
  gbd_df <- replace(gbd_df, is.na(gbd_df), 0)
  
  
  
  ## Data has to be interpolated from 5-year age groups to 1-year age groups.
  
  ## Create variable names.
  
  for (d in 1:nrow(DISEASE_SHORT_NAMES)){
    
    if (DISEASE_SHORT_NAMES$is_not_dis[d] == 0){
      
      var_name <- paste0("dw_adj_", DISEASE_SHORT_NAMES$sname[d])
      
      mslt_df[, var_name] <- 1
      
    }
  }
  
  ## Interpolate dw rates (and deaths and ylds?)
  
  # variables <- c('dw_adj', 'ylds (years lived with disability)_rate', 'deaths_rate')  
  
  for (d in 1:nrow(DISEASE_SHORT_NAMES)){
    for(sex_index in i_sex) {
      for (var in c('dw_adj') ){ 
        
        #### Exception hard coded for mjdd, best to change. The issue is that mjdd does not have pyld and deaths
        if (DISEASE_SHORT_NAMES$is_not_dis[d] != 0 || DISEASE_SHORT_NAMES$acronym[d] == "no_pif") {}
        else{
          
          
          var_name <- paste0(var, '_', DISEASE_SHORT_NAMES$sname[d])
          
          data <- dplyr::filter(gbd_df, sex == sex_index) %>% dplyr::select(age, sex, age_cat, starts_with(var_name))
          
          
          
          x <- data$age_cat
          y <- log(data[[var_name]])
          
          InterFunc <- stats::splinefun(x, y, method = "monoH.FC", ties = mean)
          
          interpolated <- as.data.frame(InterFunc(seq(0, 100, 1)))
          age <- seq(0, 100, by = 1)
          interpolated <- cbind(interpolated, age)
          interpolated[,1] <- exp(interpolated[,1])
          ## Add column with sex to create age_sex category to then merge with input_life table
          interpolated$sex <- paste(sex_index)
          interpolated$sex_age_cat <- paste(interpolated$sex, interpolated$age, sep = "_")
          ## Change name of column death to mx and ylds to pyld_rate to then merge
          ## with input_life table
          colnames(interpolated)[1] <- var_name
          
          interpolated[IsNanDataFrame(interpolated)] <- 0
          
          interpolated[IsInfDataFrame(interpolated)] <- 0
          
          mslt_df[mslt_df$sex_age_cat == interpolated$sex_age_cat 
                  & mslt_df$sex == sex_index, ][[var_name]] <- interpolated[[var_name]]
          
          index <- index + 1           
          #browser()
          
        }
      }
    }
  }
  
  #
  ## Interpolate all cause mortality and pylds and diseases
  
  ### Create variable names
  
  for (d in 1:nrow(DISEASE_SHORT_NAMES)){
    
    # if (DISEASE_SHORT_NAMES$is_not_dis[d] != 2){
    
    var_name1 <- paste0("deaths_rate", "_", DISEASE_SHORT_NAMES$sname[d])
    
    var_name2 <- paste0("ylds (years lived with disability)_rate", "_", DISEASE_SHORT_NAMES$sname[d])
    
    
    mslt_df[, var_name1] <- 1
    mslt_df[, var_name2] <- 1
    
    
  }
  
  
  ### Deaths
  
  for (d in 1:nrow(DISEASE_SHORT_NAMES)){
    for(sex_index in i_sex) {
      for (var in c('deaths_rate')) {
        # if (DISEASE_SHORT_NAMES$is_not_dis[d] != 2){
        
        var_name1 <- paste0(var, '_', DISEASE_SHORT_NAMES$sname[d])
        
        data <- dplyr::filter(gbd_df, sex == sex_index) %>% dplyr::select(age, sex, age_cat, starts_with(var_name1))
        
        
        if (DISEASE_SHORT_NAMES$acronym[d] == "no_pif") {}
        else{
          
          x <- data$age_cat
          y <- log(data[[var_name1]])
          
          InterFunc <- stats::splinefun(x, y, method = "monoH.FC", ties = mean)
          
          interpolated <- as.data.frame(InterFunc(seq(0, 100, 1)))
          age <- seq(0, 100, by = 1)
          interpolated <- cbind(interpolated, age)
          interpolated[,1] <- exp(interpolated[,1])
          ## Add column with sex to create age_sex category to then merge with input_life table
          interpolated$sex <- paste(sex_index)
          interpolated$sex_age_cat <- paste(interpolated$sex, interpolated$age, sep = "_")
          ## Change name of column death to mx and ylds to pyld_rate to then merge
          ## with input_life table
          colnames(interpolated)[1] <- var_name1
          
          interpolated[IsNanDataFrame(interpolated)] <- 0
          
          interpolated[IsInfDataFrame(interpolated)] <- 0
          
          mslt_df[mslt_df$sex_age_cat == interpolated$sex_age_cat
                  & mslt_df$sex == sex_index, ][[var_name1]] <- interpolated[[var_name1]]
        }
      }
    }
  }
  
  # names(gbd_df)
  # gbd_df$`ylds (years lived with disability)_rate_mtri`
  # gbd_df$deaths_rate_lwri
  
  ### YLDs
  
  for (d in 1:nrow(DISEASE_SHORT_NAMES)){
    for(sex_index in i_sex) {
      for (var in c("ylds (years lived with disability)_rate")){
        
        # if (DISEASE_SHORT_NAMES$is_not_dis[d] != 2){
        
        var_name2 <- paste0(var, '_', DISEASE_SHORT_NAMES$sname[d])
        
        data <- dplyr::filter(gbd_df, sex == sex_index) %>% dplyr::select(age, sex, age_cat, starts_with(var_name2))
        
        if (DISEASE_SHORT_NAMES$acronym[d] == "no_pif") {}
        else{
          
          # browser() Data input and x and y are fine, different for all, however, interpolated values are all the same.
          
          x <- data$age_cat
          y <- log(data[[var_name2]])
          
          interpolated <- as.data.frame(InterFunc(seq(0, 100, 1)))
          
          # browser()
          
          age <- seq(0, 100, by = 1)
          interpolated <- cbind(interpolated, age)
          interpolated[,1] <- exp(interpolated[,1])
          
          # browser()
          
          ## Add column with sex to create age_sex category to then merge with input_life table
          interpolated$sex <- paste(sex_index)
          interpolated$sex_age_cat <- paste(interpolated$sex, interpolated$age, sep = "_")
          ## Change name of column death to mx and ylds to pyld_rate to then merge
          ## with input_life table
          colnames(interpolated)[1] <- var_name2
          
          # browser()
          
          interpolated[IsNanDataFrame(interpolated)] <- 0
          
          interpolated[IsInfDataFrame(interpolated)] <- 0
          
          mslt_df[mslt_df$sex_age_cat == interpolated$sex_age_cat
                  & mslt_df$sex == sex_index, ][[var_name2]] <- interpolated[[var_name2]]
          
        }
      }
    }
  }
  
  ## drop age and area from disease to avoid generating x and y variables
  
  disease <- within(disease, rm(year, sex, area))
  
  
  mslt_df <- left_join(mslt_df, disease, by = "sex_age_area_cat", keep = FALSE)
  
  names(mslt_df)[names(mslt_df) == "deaths_rate_allc"] <- "mx"
  names(mslt_df)[names(mslt_df) == "ylds (years lived with disability)_rate_allc"] <- "pyld_rate"
  
  return(mslt_df)
  
}
# --- IsNanDataFrame ---
## For interpolation generation, input data frame for interpolation has nan values that we replace with 0. 

IsNanDataFrame <- function(x)
  do.call(cbind, lapply(x, is.nan))

# --- IsInfDataFrame ---
## For interpolation generation, input data frame for interpolation has inf values that we replace with 0. 
IsInfDataFrame <- function(x)
  do.call(cbind, lapply(x, is.infinite))

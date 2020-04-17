# ---- chunk-Introduction: Disbayes data generation generation ----

## The below code generates data to use as inputs in disbayes.


## To run this code, first the following data preparation (from dataprep.R scrip has to be generated)

## 1) gbd_city_region_data
## 2) disease_measure list 
## 3) DISEASE_SHORT_NAMES


# ---- chunk-1: Disbayes input generation ----

# ---- chunk-1.2: Generate city regions dataframes in a list ----

index <- 1

disbayes_input_list_city_regions <- list()

for (i in 1:length(gbd_city_region_data_agg)) {
  
  disbayes_input_list_city_regions[[index]] <- GenInpDisbayes(gbd_city_region_data_agg[[i]])
  # disbayes_input_list_city_regions[[index]]$cityregion <- paste(names(gbd_city_region_data_agg[i]))
  # # disbayes_input_list_city_regions[[index]]$cityregion<- paste0(names(gbd_city_region_data_agg[i]))
  
  names(disbayes_input_list_city_regions)[index] <- paste0(names(gbd_city_region_data_agg[i]))
  
  index <- index + 1
}



# ---- chunk-1.2.1: Add city regions names ----

for (i in 1:length(disbayes_input_list_city_regions)){
  for (j in 1:length(disbayes_input_list_city_regions[[i]])) {
disbayes_input_list_city_regions[[i]][[j]]$cityregion <- paste(names(disbayes_input_list_city_regions[i]))
  }
} 

# ---- chunk-1.2.2: Generate num and denom using ci2num for incidence, prevalence and mortlaity ----


disbayes_input_list_city_regions_2 <- list()
index <- 1
for (i in 1:length(gbd_city_region_data)) {
  for (dm in 1:length(disease_measures_list)){
    for (d in 1:nrow(DISEASE_SHORT_NAMES)){
      in_measure <- disease_measures_list[dm] %>% as.character() %>% tolower()
      
      ### exclude ylds for now, we are interested in disbayes inputs but later may use ylds uncertainty parameters
      
      if (DISEASE_SHORT_NAMES$is_not_dis[d] != 0 || in_measure == "ylds (years lived with disability)" ||
          DISEASE_SHORT_NAMES$disease[d] == "hypertensive heart disease" || 
          DISEASE_SHORT_NAMES$disease[d] == "major depressive disorder")
        {
      }
      else {
        
        med <- paste0(in_measure, "_med_", DISEASE_SHORT_NAMES$sname[d])
        low <- paste0(in_measure, "_lower95_", DISEASE_SHORT_NAMES$sname[d])
        upper <- paste0(in_measure, "_upper95_", DISEASE_SHORT_NAMES$sname[d])
        
        ## These data is in 5-year age groups. 
        data <- gbd_city_region_data[[i]]
        
        disbayes_input_list_city_regions_2[[index]] <- dplyr::select(data, population_number, cityregion, location, sex_age_cat, med, low, upper)
        
        disbayes_input_list_city_regions_2[[index]]$est <- disbayes_input_list_city_regions_2[[index]][[med]]/disbayes_input_list_city_regions_2[[index]][[1]]
        disbayes_input_list_city_regions_2[[index]]$lower <- disbayes_input_list_city_regions_2[[index]][[low]]/disbayes_input_list_city_regions_2[[index]][[1]]
        disbayes_input_list_city_regions_2[[index]]$upper <- disbayes_input_list_city_regions_2[[index]][[upper]]/disbayes_input_list_city_regions_2[[index]][[1]]
        disbayes_input_list_city_regions_2[[index]]$index <- paste(in_measure, DISEASE_SHORT_NAMES$sname[d], sep = "_")
        disbayes_input_list_city_regions_2[[index]]$indexagg <- paste(disbayes_input_list_city_regions_2[[index]]$index, disbayes_input_list_city_regions_2[[index]]$sex_age_cat,
                                                  disbayes_input_list_city_regions_2[[index]]$cityregion, sep = "_")
        
        ## Separate age and sex and   
        suppressWarnings(names(disbayes_input_list_city_regions_2)[index] <- paste(gbd_city_region_data[[i]]$cityregion,in_measure, DISEASE_SHORT_NAMES$sname[d], sep = '_'))
        
        index <- index + 1
        
      }
    }
  }
}


# ---- chunk-1.2.4: Generate num and denoms using Ci2NumDF ----

## trycatch is used to avoid issues with prevalence when runnind Ci2NumDF

tryCatchCi2NumDF <- function(x) tryCatch(Ci2NumDF(x), error = function(e) e)
disbayes_input_list_city_regions_3  <- lapply(disbayes_input_list_city_regions_2, tryCatchCi2NumDF)

# ---- chunk-1.2.5: Create a dataframe with all city regions data ----

## This step is to delete the data frames with errors from the list before appending the data frames in the list in a unique dataframe.
has_prevc <- c(grepl("prevalence",names(disbayes_input_list_city_regions_3)) == TRUE) %>% as.character()

index <- 1
disbayes_input_list_city_regions_3b <- list()

for (i in 1:length(disbayes_input_list_city_regions_3)) {

  if(has_prevc[[index]] == "TRUE" ) {}
  else{

disbayes_input_list_city_regions_3b[[index]] <- disbayes_input_list_city_regions_3[[i]]
  }
index <- index + 1
}

disbayes_input_list_city_regions_3b <-  list.clean(disbayes_input_list_city_regions_3b, fun = is.null, recursive = TRUE)


disbayes_input_list_city_regions_4 <- disbayes_input_list_city_regions_3b %>% lapply(as.data.frame) %>% bind_rows() %>% group_by(indexagg)

### Old, does not work, replaced by code above
# disbayes_input_list_city_regions_4 <- dplyr::ldply(disbayes_input_list_city_regions_3b, rbind) %>% group_by(indexagg)

disbayes_input_list_city_regions_5 <- disbayes_input_list_city_regions_4 %>% summarise_all(funs(sum))

disbayes_input_list_city_regions_5$indexagg <- gsub("'", '', disbayes_input_list_city_regions_5$indexagg)

disbayes_input_list_city_regions_6 <- disbayes_input_list_city_regions_5 %>%  mutate_if(is.character, RemoveAllWs)%>% 
  mutate(index = indexagg) %>% 
  separate(indexagg, c("measure", "disease", "sex", "age", "cityregion"))

## Add new variable with mid-age group

disbayes_input_list_city_regions_6$agegr <- 0
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="Under5"] <- 0
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="5to9"] <- 5
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="10to14"] <- 10
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="15to19"] <- 15
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="20to24"] <- 20
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="25to29"] <- 25
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="30to34"] <- 30
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="35to39"] <- 35
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="40to44"] <- 40
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="45to49"] <- 45
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="50to54"] <- 50
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="55to59"] <- 55
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="60to64"] <- 60
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="65to69"] <- 65
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="70to74"] <- 70
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="75to79"] <- 75
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="80to84"] <- 80
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="85to89"] <- 85
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="90to94"] <- 90
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="95plus"] <- 95

city_regions_names <- unique(disbayes_input_list_city_regions_6$cityregion)
disease_disbayes <- unique(disbayes_input_list_city_regions_6$disease)
measure_disbayes <- unique(disbayes_input_list_city_regions_6$measure)
sex_disbayes <- unique(disbayes_input_list_city_regions_6$sex)


## To wider 


disbayes_input_list_city_regions_7 <- disbayes_input_list_city_regions_6 %>% 
                                      pivot_wider(id_cols = c(agegr, sex, population_number, cityregion, measure, disease), 
                                      names_from = measure, values_from = c(num, denom))

## Create list
index <- 1
disbayes_input_list2 <- list()

for (c in city_regions_names){
    for (d in disease_disbayes){
      for (s in sex_disbayes){
        
     
      disbayes_input_list2[[index]] <- dplyr::filter(disbayes_input_list_city_regions_7, cityregion == c, disease == d, sex == s)
      
      disbayes_input_list2[[index]] <- disbayes_input_list2[[index]][order(disbayes_input_list2[[index]]$agegr),]
      
      outage <- 0:100  # assume num and denom are the same in each year within a five-year age group
      
      # 
      ind <- findInterval(outage, disbayes_input_list2[[index]]$agegr)
      disbayes_input_list2[[index]] <- disbayes_input_list2[[index]][ind,]
      disbayes_input_list2[[index]]$age <- outage
      
      
      ### It leaves NA values (I need to have all columns filled in)
     
       
      disbayes_input_list2[[index]]$index <- tolower(paste(disbayes_input_list2[[index]]$sex, 
                                                  disbayes_input_list2[[index]]$disease, 
                                                  disbayes_input_list2[[index]]$age, 
                                                  disbayes_input_list2[[index]]$cityregion, sep = "_"))
       # disbayes_input_list2[[index]] <- disbayes_input_list2[[index]] %>% pivot_wider(id_cols = c(index), names_from = measure, values_from = c(num, denom))
        index <- index + 1
          
      
    }
  }
}

# ---- chunk-1.2.6: Join dataframes with all disbayes inputs ----

## First data set rates
disbayes_inputs_df <- do.call(rbind, disbayes_input_list_city_regions)

### Some issue with disease column, which we do not need, to rbind to dataframe
disbayes_inputs_df <- lapply(disbayes_inputs_df, function(x) { x["disease"] <- NULL; x })
disbayes_inputs_df <- dplyr::bind_rows(disbayes_inputs_df)

### CHECK WHAT HAPPENEND WITH CITY REGIONS AND THATN INDEX IS THE SAME AS IN DISBAYES INPUT DATAFRAME2
disbayes_inputs_df$index <- paste(disbayes_inputs_df$sex_disease, disbayes_inputs_df$age, disbayes_inputs_df$cityregion, sep = "_")
## Second data set with num and denom
disbayes_inputs_df2 <- do.call(rbind, disbayes_input_list2)
disbayes_inputs_df2 <-   disbayes_inputs_df2[ -c(1:5,10) ]


## Final data set to process in disbayes. Filter data by city region, disease and sex. COMPARE with saved data in rds
disbayes_inputs <- disbayes_inputs_df %>%
  left_join(disbayes_inputs_df2) %>% 
  separate(sex_disease, c("drop", "disease")) 

disbayes_inputs <- disbayes_inputs[, !(colnames(disbayes_inputs ) %in% c("drop","index"))]


write_rds(disbayes_inputs, paste0(relative_path_mslt, "data/city regions/Input disbayes/disbayes_inputs", ".rds"))





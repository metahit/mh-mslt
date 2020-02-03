# To run this code, first the following data preparation (from dataprep.R scrip has to be generated)

## 1) gbd_city_region_data
## 2) disease_measure list 
## 3) disease_short_names


# ---- chunk-1: Disbayes input generation ----

# ---- chunk-1.2: Disbayes generate general data frame and rate inputs ----

## The code below generates age, sex and disease specific data frames to process with disbayes. 
## Chris Jackson generated the code for one dataset and I added a loop to do all diseases by age an sex. 

index <- 1

disbayes_input_list_city_regions <- list()

for (i in 1:length(gbd_city_region_data_agg)) {
  
  disbayes_input_list_city_regions[[index]] <- GenInpDisbayes(gbd_city_region_data_agg[[i]])
  
  names(disbayes_input_list_city_regions)[index] <- paste0(names(gbd_city_region_data_agg[i]))
  
  index <- index + 1
  
}


## I added results from ci2Num function (num and denom for incidence and mortality, there are issues with prevalence)

## Save as rds for sharing

for (i in 1:length(disbayes_input_list_city_regions)) {
  city_name <- names(disbayes_input_list_city_regions[i])
  for (j in 1:length(disbayes_input_list_city_regions[[i]])) {
    
    ## Add variable name to each of the diseases
    
    disbayes_input_list_city_regions[[i]][[j]]$city_region <- city_name
    
    
    temp_disease_name <- disbayes_input_list_city_regions[[i]][[j]][1,15]
    print(paste0(city_name, "_",temp_disease_name))
    write_rds(disbayes_input_list_city_regions[[i]][[j]],
              paste0(relative_path_mslt, "data/city regions/Input disbayes/",city_name, "_",temp_disease_name,".rds"))
  }
}

# ---- Chunk 1.4: Disbayes generate num and denom for incidence and mort

## Prepare data to process in ci2numDF (incidence, prevalence and deaths as these are disbayes inputs)


disbayes_input_list_city_regions_2 <- list()
index <- 1
for (i in 1:length(gbd_city_region_data)) {
  for (dm in 1:length(disease_measures_list)){
    for (d in 1:nrow(disease_short_names)){
      in_measure <- disease_measures_list[dm] %>% as.character() %>% tolower()
      
      if (disease_short_names$is_not_dis[d] != 0 || in_measure == "ylds (years lived with disability)" ||
          in_measure == "prevalence") {
      }
      else {
        
        med <- paste0(in_measure, "_med_", disease_short_names$sname[d])
        low <- paste0(in_measure, "_lower95_", disease_short_names$sname[d])
        upper <- paste0(in_measure, "_upper95_", disease_short_names$sname[d])
        
        
        data <- gbd_city_region_data[[i]]
        
        disbayes_input_list_city_regions_2[[index]] <- dplyr::select(data, population_number, cityregion, sex_age_cat, med, low, upper)
        
        disbayes_input_list_city_regions_2[[index]]$est <- disbayes_input_list_city_regions_2[[index]][[med]]/disbayes_input_list_city_regions_2[[index]][[1]]
        disbayes_input_list_city_regions_2[[index]]$lower <- disbayes_input_list_city_regions_2[[index]][[low]]/disbayes_input_list_city_regions_2[[index]][[1]]
        disbayes_input_list_city_regions_2[[index]]$upper <- disbayes_input_list_city_regions_2[[index]][[upper]]/disbayes_input_list_city_regions_2[[index]][[1]]
        disbayes_input_list_city_regions_2[[index]]$index <- paste(in_measure, disease_short_names$sname[d], sep = "_")
        disbayes_input_list_city_regions_2[[index]]$indexagg <- paste(disbayes_input_list_city_regions_2[[index]]$index, disbayes_input_list_city_regions_2[[index]]$sex_age_cat,
                                                  disbayes_input_list_city_regions_2[[index]]$cityregion, sep = "_")
        
        suppressWarnings(names(disbayes_input_list_city_regions_2)[index] <- paste(gbd_city_region_data[[i]]$cityregion,in_measure, disease_short_names$sname[d], sep = '_'))
        
        index <- index + 1
        
      }
    }
  }
}




### Run for each element of the list (time consuming, also RunLoc. Check virtual machine with Ian Thomas)

#### Run this excluding prevalence and YLDs

disbayes_input_list_city_regions_3 <- lapply(disbayes_input_list_city_regions_2, Ci2NumDF)


### ADD Prevalence adn YLDS numbers (est)

### Create aggregated data for city regions using index combined with sex age cat and city region

### Drop first column with id variable (character) as it gives an error (cannot sum characters)

disbayes_input_list_city_regions_4 <- plyr::ldply(disbayes_input_list_city_regions_3, rbind) %>% group_by(indexagg)

disbayes_input_list_city_regions_5 <- disbayes_input_list_city_regions_4[ -c(1) ] %>% summarise_all(funs(sum))

disbayes_input_list_city_regions_6 <- disbayes_input_list_city_regions_5 %>%  mutate_if(is.character, RemoveAllWs)%>% 
  separate(indexagg, c("measure", "disease", "sex", "age", "cityregion"))

## Add new variable with mid-age group

disbayes_input_list_city_regions_6$agegr <- 0
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="Under5"] <- 2
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="5to9"] <- 7
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="10to14"] <- 12
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="15to19"] <- 17
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="20to24"] <- 22
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="25to29"] <- 27
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="30to34"] <- 32
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="35to39"] <- 37
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="40to44"] <- 42
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="45to49"] <- 47
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="50to54"] <- 52
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="55to59"] <- 57
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="60to64"] <- 62
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="65to69"] <- 67
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="70to74"] <- 72
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="75to79"] <- 77
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="80to84"] <- 82
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="85to89"] <- 87
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="90to94"] <- 92
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="95plus"] <- 97

city_regions_names <- unique(disbayes_input_list_city_regions_6$cityregion)
disease_disbayes <- unique(disbayes_input_list_city_regions_6$disease)
measure_disbayes <- unique(disbayes_input_list_city_regions_6$measure)
sex_disbayes <- unique(disbayes_input_list_city_regions_6$sex)


## Convert list into a data frame with all diseases and areas (SEE IF WE USE THIS)

index <- 1
inputs_disbayes_list <- list()

for (i in 1:length(disbayes_input_list_city_regions)) {
  
  inputs_disbayes_list[[index]] <- plyr::ldply(disbayes_input_list_city_regions[[i]], rbind)
  
  index <- index + 1
}

inputs_disbayes <- plyr::ldply(inputs_disbayes_list, rbind)



## Create list with data needs for multistate life table processing (case fatality and incidence) (OLD Code, delete)
### NEW Code using Chris 


# ---- chunk-1: Disbayes output organisation to link to mslt dataframe (generated by Chris based on above data) ----

## add name to column outputs (column 0) NEED TO GENENERATE OUTPUTS LIKE CHRIS FROM THE DISBAYES SCRIPT

cityregions_smoothed_res <- cbind(
  mes=rownames(cityregions_smoothed_res), cityregions_smoothed_res) 


cityregions_smoothed_res <- cbind(cityregions_smoothed_res, (str_split_fixed(cityregions_smoothed_res$
                                                                               mes, fixed('['), 2)))
cityregions_smoothed_res <- cityregions_smoothed_res[ (cityregions_smoothed_res$`1` %in% c("inc", "cf", "prev")), ]
cityregions_smoothed_res$`1` <- as.character(cityregions_smoothed_res$`1`)
cityregions_smoothed_res$`2` <- as.character(cityregions_smoothed_res$`2`)
cityregions_smoothed_res$`2` <- gsub("].*", "",cityregions_smoothed_res$`2`)


## Rename 1 adn 2
names(cityregions_smoothed_res)[names(cityregions_smoothed_res) == "1"] <- "rates"
names(cityregions_smoothed_res)[names(cityregions_smoothed_res) == "2"] <- "year"
## Keep incidence, case fatality, prevalence and mortality


## Test here creating data set per area (first filter, otherwise another layer with the areas)

#### Keep them in large data frame forrmat for data prep?

## LOOP to generate data per city regions and other areas (England, etc) should start here. replace bristol with data
# 
# for (a in c(unique(cityregions_smoothed_res$area))) {

## For GBD inputs
bristol_test <- dplyr::filter(cityregions_smoothed_res, area == "bristol")
## For all other data
gbd_df <- dplyr::filter(gbd_data, area == "bristol")

## Create column for combination disease and rate type (inc, prev, cf)

bristol_test$disease_rate <- paste(bristol_test$disease, bristol_test$rates, sep = "_")

bristol_test_2 <- bristol_test %>% pivot_wider(id_cols = c(sex, cityregion, age_cat), names_from = disease_rate, values_from = c(med, lower95, upper95))




# To run this code, first the following data preparation (from dataprep.R scrip has to be generated)

## 1) gbd_city_region_data
## 2) disease_measure list 
## 3) DISEASE_SHORT_NAMES


# ---- chunk-1: Disbayes input generation ----

# ---- chunk-1.2: Disbayes generate general data frame and rate inputs ----

## The code below generates age, sex and disease specific data frames to process with disbayes. 
## Chris Jackson generated the code for one dataset and I added a loop to do all diseases by age an sex. 

index <- 1

disbayes_input_list_city_regions <- list()

for (i in 1:length(gbd_city_region_data_agg)) {
  
  disbayes_input_list_city_regions[[index]] <- GenInpDisbayes(gbd_city_region_data_agg[[i]])
  # disbayes_input_list_city_regions[[index]]$cityregion <- paste(names(gbd_city_region_data_agg[i]))
  # # disbayes_input_list_city_regions[[index]]$cityregion<- paste0(names(gbd_city_region_data_agg[i]))
  
  names(disbayes_input_list_city_regions)[index] <- paste0(names(gbd_city_region_data_agg[i]))
  
  index <- index + 1
  
}




### ADD MEASURE AND AGE GROUP TO BUILD INDEX TO MATCH DATA GENERATED FOR NUM AND DENOM which builts from gbd_city_region_data which is in 
### 5 year age intervals

for (i in 1:length(disbayes_input_list_city_regions)){
  for (j in 1:length(disbayes_input_list_city_regions[[i]])) {
disbayes_input_list_city_regions[[i]][[j]]$cityregion <- paste(names(disbayes_input_list_city_regions[i]))
  }
} 

disbayes_inputs_df <- do.call(rbind, disbayes_input_list_city_regions)
disbayes_inputs_df <- do.call(rbind, disbayes_inputs_df)
disbayes_inputs_df$index <- paste(disbayes_inputs_df$disease, disbayes_inputs_df$sex, disbayes_inputs_df$age, disbayes_inputs_df$cityregion, sep = "_")


## I added results from ci2Num function (num and denom for incidence and mortality, there are issues with prevalence)

## Save as rds for sharing (MOVE AT THE END ONCE num and denom area added ALSO SAVE COMPLETE DATA FRAME)

for (i in 1:length(disbayes_input_list_city_regions)) {
  city_name <- names(disbayes_input_list_city_regions[i])
  for (j in 1:length(disbayes_input_list_city_regions[[i]])) {
    
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
    for (d in 1:nrow(DISEASE_SHORT_NAMES)){
      in_measure <- disease_measures_list[dm] %>% as.character() %>% tolower()
      
      if (DISEASE_SHORT_NAMES$is_not_dis[d] != 0 || in_measure == "ylds (years lived with disability)")
        {
      }
      else {
        
        med <- paste0(in_measure, "_med_", DISEASE_SHORT_NAMES$sname[d])
        low <- paste0(in_measure, "_lower95_", DISEASE_SHORT_NAMES$sname[d])
        upper <- paste0(in_measure, "_upper95_", DISEASE_SHORT_NAMES$sname[d])
        
        ## These data is in 5-year age groups. 
        data <- gbd_city_region_data[[i]]
        
        disbayes_input_list_city_regions_2[[index]] <- dplyr::select(data, population_number, cityregion, sex_age_cat, med, low, upper)
        
        disbayes_input_list_city_regions_2[[index]]$est <- disbayes_input_list_city_regions_2[[index]][[med]]/disbayes_input_list_city_regions_2[[index]][[1]]
        disbayes_input_list_city_regions_2[[index]]$lower <- disbayes_input_list_city_regions_2[[index]][[low]]/disbayes_input_list_city_regions_2[[index]][[1]]
        disbayes_input_list_city_regions_2[[index]]$upper <- disbayes_input_list_city_regions_2[[index]][[upper]]/disbayes_input_list_city_regions_2[[index]][[1]]
        disbayes_input_list_city_regions_2[[index]]$index <- paste(in_measure, DISEASE_SHORT_NAMES$sname[d], sep = "_")
        disbayes_input_list_city_regions_2[[index]]$indexagg <- paste(disbayes_input_list_city_regions_2[[index]]$index, disbayes_input_list_city_regions_2[[index]]$sex_age_cat,
                                                  disbayes_input_list_city_regions_2[[index]]$cityregion, sep = "_")
        
        ### Separate age and sex and   
        suppressWarnings(names(disbayes_input_list_city_regions_2)[index] <- paste(gbd_city_region_data[[i]]$cityregion,in_measure, DISEASE_SHORT_NAMES$sname[d], sep = '_'))
        
        index <- index + 1
        
      }
    }
  }
}

### Save for to inspect problematic diseases

for (i in 1:length(disbayes_input_list_city_regions_2)) {
 
  
    ## Separate age and sex to then order the data by age and sex
    
    disbayes_input_list_city_regions_2_test <- list()
  
    disbayes_input_list_city_regions_2_test[[i]] <- disbayes_input_list_city_regions_2[[i]]

    disbayes_input_list_city_regions_2_test[[i]] <- disbayes_input_list_city_regions_2_test[[i]] %>%
     separate(sex_age_cat, c("sex", "age_cat"))
    
    ## Order the data
    disbayes_input_list_city_regions_2_test[[i]] <- disbayes_input_list_city_regions_2_test[[i]][order(disbayes_input_list_city_regions_2_test[[i]]$sex, disbayes_input_list_city_regions_2_test[[i]]$age_cat),]
    
    
    
    write_rds(disbayes_input_list_city_regions_2_test[[i]],paste0(relative_path_mslt, "data/city regions/Ci2num/",paste0(names(disbayes_input_list_city_regions_2)[i]),".rds"))
  
}



## Try catch to skip errors
tryCatchCi2NumDF <- function(x) tryCatch(Ci2NumDF(x), error = function(e) e)
disbayes_input_list_city_regions_3  <- lapply(disbayes_input_list_city_regions_2, tryCatchCi2NumDF)

### Graphs to check prevalence inputs for errors



### if condition?

### Create aggregated data for city regions using index combined with sex age cat and city region

### Drop first column with id variable (character) as it gives an error (cannot sum characters)

disbayes_input_list_city_regions_4 <- plyr::ldply(disbayes_input_list_city_regions_3, rbind) %>% group_by(indexagg)

disbayes_input_list_city_regions_5 <- disbayes_input_list_city_regions_4[ -c(1) ] %>% summarise_all(funs(sum))

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


disbayes_input_list_city_regions_7 <- disbayes_input_list_city_regions_6 %>% pivot_wider(id_cols = c(agegr, sex, population_number, cityregion, measure, disease), names_from = measure, values_from = c(num, denom))

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
     
       
      disbayes_input_list2[[index]]$index <- tolower(paste(disbayes_input_list2[[index]]$disease, 
                                                  disbayes_input_list2[[index]]$sex, 
                                                  disbayes_input_list2[[index]]$age, 
                                                  disbayes_input_list2[[index]]$cityregion, sep = "_"))
       # disbayes_input_list2[[index]] <- disbayes_input_list2[[index]] %>% pivot_wider(id_cols = c(index), names_from = measure, values_from = c(num, denom))
        index <- index + 1
          
      
    }
  }
}


disbayes_inputs_df2 <- do.call(rbind, disbayes_input_list2)
disbayes_inputs_df2 <-   disbayes_inputs_df2[ -c(1:5,10) ]


### Final data set to process in disbayes. Filter data by city region, disease and sex. COMPARE with saved data in rds
disbayes_inputs <- disbayes_inputs_df %>%
  left_join(disbayes_inputs_df2)


write_rds(disbayes_inputs, paste0(relative_path_mslt, "data/city regions/Input disbayes/disbayes_inputs", ".rds"))



# ---- chunk-2: Disbayes output organisation to link to mslt dataframe (generated by Chris based on above data) ----

## These data were generated by Chris and saved in /data/city regions/Outpu disbayes. Need to get teh data in the global environment first.

## add name to column outputs (column 0) NEED TO GENENERATE OUTPUTS LIKE CHRIS FROM THE DISBAYES SCRIPT

### create column one with outcome and year
cityregions_smoothed_res <- cbind(
  mes=rownames(cityregions_smoothed_res), cityregions_smoothed_res)

### Separate avoce in outcome and year
cityregions_smoothed_res <- cbind(cityregions_smoothed_res, (str_split_fixed(cityregions_smoothed_res$
                                                                               mes, fixed('['), 2)))

cityregions_smoothed_res <- cityregions_smoothed_res[ (cityregions_smoothed_res$`1` %in% c("inc", "cf", "prev")), ]
cityregions_smoothed_res$`1` <- as.character(cityregions_smoothed_res$`1`)
cityregions_smoothed_res$`2` <- as.character(cityregions_smoothed_res$`2`)
cityregions_smoothed_res$`2` <- gsub("].*", "",cityregions_smoothed_res$`2`)


## Rename columns
names(cityregions_smoothed_res)[names(cityregions_smoothed_res) == "1"] <- "rates"
names(cityregions_smoothed_res)[names(cityregions_smoothed_res) == "2"] <- "year"

## Rename string values inc to incidence, cf to case fatality and prev to prevalence

cityregions_smoothed_res <- cityregions_smoothed_res %>% 
  mutate(rates = str_replace(rates, "inc", "incidence"))  %>%
    mutate(rates = str_replace(rates, "cf", "case_fatality"))  %>%
      mutate(rates = str_replace(rates, "prev", "prevalence"))

## Move to columns for data for case_fatality, incidence and prevelence

cityregions_smoothed_res$disease_rate <- paste(cityregions_smoothed_res$rates, cityregions_smoothed_res$disease, sep = "_")
cityregions_smoothed_res2 <- cityregions_smoothed_res %>% pivot_wider(id_cols = c(area, gender, model, year), names_from = disease_rate, values_from = c(med, lower95, upper95))
names(cityregions_smoothed_res2) = gsub(pattern = "med_", replacement = "", x = names(cityregions_smoothed_res2))

disbayes_output <- cityregions_smoothed_res2 %>%
  dplyr::rename(sex = gender) %>% 
  mutate_if(is.factor, as.character)
disbayes_output$year <- disbayes_output$year %>% as.numeric(disbayes_output$year)
## Change year to match mslt dataframe (0 to 100 years)
disbayes_output$year[1:101] <- 0:100

disbayes_output$sex_age_area_cat <- paste(disbayes_output$sex,disbayes_output$year, disbayes_output$area, sep = "_"  )

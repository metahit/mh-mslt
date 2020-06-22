
## Create R Markdown for data preparation

# ---- chunk-intro ----
rm (list = ls())
options(scipen=999)
library(readr)
library(rlist)
library(dplyr)
library(tidyverse)
library(conflicted)
library(tidyr)
library(devtools)
library(disbayes)

## set wd

setwd("C:/Metahit/")

## Relative paths

relative_path_execute <- paste0(getwd(), '/mh-execute/')
relative_path_mslt <- paste0(getwd(),'/mh-mslt/')

##get funcions

source(paste0(relative_path_mslt,'code/DATA_PREP_functions.R'))


# ---- chunk-1: Data preparation MSLT data inputs ----

## Get look up table from mh-execute

look_up_table <- read_csv(paste0(relative_path_execute, '/inputs/mh_regions_lad_lookup.csv'))

## Dataframe with local goverment areas within each city region

local_goverment_areas <- look_up_table 

## Add non city regions names (useful for checking totals)

names_non_cr <- c("United Kingdom", "England", "East Midlands", "East of England", "Greater London", "North East England", 
                  'North West England', "South East England", "South West England", "West Midlands", "Yorkshire and the Humber", 
                  "Northern Ireland", "Scotland", "Wales")
for (i in names_non_cr){
  local_goverment_areas <- rbind(local_goverment_areas, c(i, i, i, i, i))
}
local_goverment_areas <-  local_goverment_areas %>% dplyr::filter(cityregion != "") 
names(local_goverment_areas)[names(local_goverment_areas) == "lad11nm"] <- "location"

local_goverment_areas$location <- gsub('St. Helens', 'St Helens', local_goverment_areas$location)

city_regions <- split(local_goverment_areas$location, f = local_goverment_areas$cityregion)

# ---- chunk-1.1: Get Global Burden of Disease data ----

## GBD MISSING DATA FOR NOTTINGHAM: Ashfield, Bassetlaw, Broxtowe, Gedling, Mansfield, Newark and Sherwood, Rushcliffe and City of London. 

work_folder <- "C:/Users/e95517/Dropbox/"
r_user_folder <- "C:/Users/rstudio/Dropbox/"
v_folder <- "V:/Studies/MOVED/HealthImpact/Data/Global_Burden_Disease_Metahit/"


## Change folder to work or home
# CHANGE DATA FOLDER
data_folder <- paste0(r_user_folder, "Collaborations/James Woodcock/Metahit/Data/GBD2017/")
temp_folder <- paste0(data_folder,"temp") 
result_folder <- paste0(data_folder,"final")
gbdfile_name_new <- "IHME-GBD_2017_DATA-3e0b192d-" # CHANGE NAME WHEN NEW DATA IS DOWNLOADED 
gbdfile_name_old <- "IHME-GBD_2017_DATA-ac95a757-" # CHANGE NAME WHEN NEW DATA IS DOWNLOADED 


data_extracted <- NULL
for (i in 1:5) { # LOOP NUMBER DEPENDS ON NUMBER OF ZIP FILES, HERE I JUST GOT DATA FOR ALL LOCALITIES IN ENGLAND
  file_number <- i
  
  file_select <- paste0(data_folder,gbdfile_name_new, i,".zip")
  
  unzip(file_select, exdir=temp_folder)
  
  data_read <- read_csv((paste0(temp_folder,"/", gbdfile_name_new, i, ".csv")))
  file.remove(paste0(temp_folder,"/", gbdfile_name_new, i, ".csv"))
  data_read <- subset(data_read, location_name %in% local_goverment_areas$location) # location name is easier to identify
  
  data_extracted <- rbind(data_extracted, data_read)
}

unlink(paste0(temp_folder), recursive = TRUE)

# ---- chunk-1.2: Define parameters from data ----

### Cause (e.g. diabetes, all cause)

DISEASE_SHORT_NAMES <- data.frame(disease = tolower(as.character(unique(data_extracted$cause_name))), 
                                  sname = tolower(abbreviate(unique(data_extracted$cause_name, max = 2))),
                                  stringsAsFactors = F)

DISEASE_SHORT_NAMES <- DISEASE_SHORT_NAMES %>% mutate(is_not_dis = ifelse((str_detect(disease, "injuries") |
                                                                             str_detect(disease, "All causes") |
                                                                             str_detect(disease, "Lower respiratory infections")), 
                                                                          1, 0) )

DISEASE_SHORT_NAMES[DISEASE_SHORT_NAMES$sname == "allc", "is_not_dis"] <- 2

DISEASE_SHORT_NAMES[DISEASE_SHORT_NAMES$sname == "lwri", "is_not_dis"] <- 1


### Code for major depresive disorder (no deaths) and hypertensive heart disease (no incidence)
DISEASE_SHORT_NAMES[DISEASE_SHORT_NAMES$sname == "hyhd", "is_not_dis"] <- 3

DISEASE_SHORT_NAMES[DISEASE_SHORT_NAMES$sname == "mjdd", "is_not_dis"] <- 3

### Combine with acronyms from execute-mh

## Get execute-mh diseases 

disease_names_execute <- read_csv(paste0(relative_path_execute, "inputs/dose_response/disease_outcomes_lookup.csv"))

disease_names_execute <- disease_names_execute[1:2]
disease_names_execute$disease <- tolower(disease_names_execute$GBD_name)

DISEASE_SHORT_NAMES <- left_join(DISEASE_SHORT_NAMES, disease_names_execute, by = "disease")

## Add injuries

DISEASE_SHORT_NAMES$acronym <- ifelse(str_detect(DISEASE_SHORT_NAMES$disease, "injuries"), DISEASE_SHORT_NAMES$disease, DISEASE_SHORT_NAMES$acronym)

## Only keep first word for acronyns

DISEASE_SHORT_NAMES$acronym <- word(DISEASE_SHORT_NAMES$acronym, 1)

## Add males and females only diseases

DISEASE_SHORT_NAMES$males <- ifelse(DISEASE_SHORT_NAMES$disease %in% c("breast cancer", "uterine cancer"), 0, 1)

DISEASE_SHORT_NAMES$females <- ifelse(DISEASE_SHORT_NAMES$disease %in% c("prostate cancer"), 0, 1)

DISEASE_SHORT_NAMES$sname <- gsub("'", '', DISEASE_SHORT_NAMES$sname)

## Replace NAs with blank

DISEASE_SHORT_NAMES$acronym[is.na(DISEASE_SHORT_NAMES$acronym)] <- "no_pif"


## Add column to match names from mh-execute
### Saving, but not required

write_csv(DISEASE_SHORT_NAMES, paste0(relative_path_mslt,"data/parameters/disease_names.csv"))

### Measure (e.g. deaths, prevalence)

disease_measures_list <- data.frame(measure = unique(data_extracted$measure_name)) %>%
  pull(measure) %>%
  as.character() %>%
  as.list()


# ---- chunk-1.3: Clean data ----

names(data_extracted) = gsub(pattern = "_name", replacement = "", x = names(data_extracted))

data_extracted <- select(data_extracted,-contains("id"))

data_extracted$cause <- tolower(data_extracted$cause) 

data_extracted <- left_join(local_goverment_areas, data_extracted, by = "location")


# ---- chunk-1.4: Sort data per local goverment area ----

## We first derive population and cases numbers (e.g. all cause mortality) for each locality and then aggregate at the City Region level. 

city_regions_list_loc <- split(data_extracted , f = data_extracted$cityregion)

city_regions_list <- split(data_extracted , f = data_extracted$cityregion)

city_regions_list_loc <- list()

for (i in 1:length(city_regions_list)){
  city_regions_list_loc[[i]] <- split(city_regions_list[[i]], f = city_regions_list[[i]]$location)
  
}


### This code takes about 2hs hours to run
### some help to make it more efficient. 

index <- 1
gbd_loc_data_processed <- list()

for (i in 1:length(city_regions_list_loc)) {

gbd_loc_data_processed[[index]] <- lapply(city_regions_list_loc[[i]], RunLocDf)

index <- index + 1

}



### Delete null data frames within lists

gbd_loc_data_processed <-  list.clean(gbd_loc_data_processed, fun = is.null, recursive = TRUE)


# ---- chunk-1.5: Create data frame for city region with all localities ---- 
#### Input for Ci2Num dataframe
index <- 1
gbd_city_region_data <- list()

for (i in 1:length(gbd_loc_data_processed)){
  

  gbd_city_region_data[[index]] <- bind_rows(gbd_loc_data_processed[[i]])
  
  ## Drop number columns (CHECK WHAT THIS IS DOING)
# 
#    gbd_city_region_data[[index]] <- gbd_city_region_data[[index]][ -c(1) ]
  
  ## Clean dataframes per city regions
  
  gbd_city_region_data[[index]] <- dplyr::select(gbd_city_region_data[[index]], -contains('rate')) %>% mutate_if(is.factor, as.character) 
  
  gbd_city_region_data[[index]]$sex_age_cat <- paste(gbd_city_region_data[[index]]$sex, gbd_city_region_data[[index]]$age, sep = "_")
  
  gbd_city_region_data[[index]] <- select(gbd_city_region_data[[index]], -c(age, sex))
  
  ### Add city region name here
  
 
  suppressWarnings(names(gbd_city_region_data)[index] <- paste(city_regions_list_loc[[i]][[1]]$cityregion, sep = '_'))
  
  gbd_city_region_data[[index]]$cityregion <- names(gbd_city_region_data)[index]
  
  
  index <- index + 1
  
}


#### What follows are additional steps to generate data for GenInputDisbayes

### BElen check why you are doing this step
gbd_city_region_data_2 <- list()
for (i in 1:length(gbd_city_region_data)) {
  gbd_city_region_data_2[[i]] <- gbd_city_region_data[[i]]
  #[ -c(1,3) ] 
}


### This is the input for GenInputDisbayes
gbd_city_region_data_agg <- list()
index <- 1

### Loop to generate aggregated data
for (i in 1:length(gbd_city_region_data_2)) {
  gbd_city_region_data_agg[[index]] <- gbd_city_region_data_2[[i]] %>% 
    group_by(sex_age_cat) %>%
    summarise_all(funs(if(is.numeric(.)) sum(., na.rm = TRUE) else first(.)))%>% 
    separate(sex_age_cat, c("sex", "age"), "_")
  
  
  
  ## Add numberical age categories
  
  gbd_city_region_data_agg[[index]]$age_cat <- 0
  gbd_city_region_data_agg[[index]]$age_cat [ gbd_city_region_data_agg[[index]]$age =="Under 5"] <- 2
  gbd_city_region_data_agg[[index]]$age_cat [ gbd_city_region_data_agg[[index]]$age =="5 to 9"] <- 7
  gbd_city_region_data_agg[[index]]$age_cat [ gbd_city_region_data_agg[[index]]$age =="10 to 14"] <- 12
  gbd_city_region_data_agg[[index]]$age_cat [ gbd_city_region_data_agg[[index]]$age =="15 to 19"] <- 17
  gbd_city_region_data_agg[[index]]$age_cat [ gbd_city_region_data_agg[[index]]$age =="20 to 24"] <- 22
  gbd_city_region_data_agg[[index]]$age_cat [ gbd_city_region_data_agg[[index]]$age =="25 to 29"] <- 27
  gbd_city_region_data_agg[[index]]$age_cat [ gbd_city_region_data_agg[[index]]$age =="30 to 34"] <- 32
  gbd_city_region_data_agg[[index]]$age_cat [ gbd_city_region_data_agg[[index]]$age =="35 to 39"] <- 37
  gbd_city_region_data_agg[[index]]$age_cat [ gbd_city_region_data_agg[[index]]$age =="40 to 44"] <- 42
  gbd_city_region_data_agg[[index]]$age_cat [ gbd_city_region_data_agg[[index]]$age =="45 to 49"] <- 47
  gbd_city_region_data_agg[[index]]$age_cat [ gbd_city_region_data_agg[[index]]$age =="50 to 54"] <- 52
  gbd_city_region_data_agg[[index]]$age_cat [ gbd_city_region_data_agg[[index]]$age =="55 to 59"] <- 57
  gbd_city_region_data_agg[[index]]$age_cat [ gbd_city_region_data_agg[[index]]$age =="60 to 64"] <- 62
  gbd_city_region_data_agg[[index]]$age_cat [ gbd_city_region_data_agg[[index]]$age =="65 to 69"] <- 67
  gbd_city_region_data_agg[[index]]$age_cat [ gbd_city_region_data_agg[[index]]$age =="70 to 74"] <- 72
  gbd_city_region_data_agg[[index]]$age_cat [ gbd_city_region_data_agg[[index]]$age =="75 to 79"] <- 77
  gbd_city_region_data_agg[[index]]$age_cat [ gbd_city_region_data_agg[[index]]$age =="80 to 84"] <- 82
  gbd_city_region_data_agg[[index]]$age_cat [ gbd_city_region_data_agg[[index]]$age =="85 to 89"] <- 87
  gbd_city_region_data_agg[[index]]$age_cat [ gbd_city_region_data_agg[[index]]$age =="90 to 94"] <- 92
  gbd_city_region_data_agg[[index]]$age_cat [ gbd_city_region_data_agg[[index]]$age =="95 plus"] <- 97
  
  ## Change sex variable to lower case
  
  gbd_city_region_data_agg[[index]]$sex <- tolower(gbd_city_region_data_agg[[index]]$sex)
  
  ## Create age_sex category
  
  gbd_city_region_data_agg[[index]]$sex_age_cat <- paste(gbd_city_region_data_agg[[index]]$sex,gbd_city_region_data_agg[[index]]$age_cat, sep = "_"  )
  
  ## Order data
  
  gbd_city_region_data_agg[[index]] <- gbd_city_region_data_agg[[index]][order(gbd_city_region_data_agg[[index]]$sex, gbd_city_region_data_agg[[index]]$age_cat),]
  
   
  
  suppressWarnings(names(gbd_city_region_data_agg)[index] <- paste(city_regions_list_loc[[i]][[1]]$cityregion, sep = '_'))
  
  
  ### Calculate rates per one. Needed for mslt_code (exclude major depresive disorders (no deaths) and hyoertensive heart disease (no incidence))
  for (d in 1:nrow(DISEASE_SHORT_NAMES)){
    for (dm in 1:length(disease_measures_list)){
      # dn <- DISEASE_SHORT_NAMES$disease[d]
      dmeasure <- disease_measures_list[dm] %>% as.character() %>% tolower

      if (DISEASE_SHORT_NAMES$is_not_dis[d] == 3){
      }
      else{


      var_rate <- c(paste(tolower(paste(dmeasure, "rate", DISEASE_SHORT_NAMES$sname[d], sep = "_"))))
      var_med <- c(paste(tolower(paste(dmeasure, "med", DISEASE_SHORT_NAMES$sname[d], sep = "_"))))


        gbd_city_region_data_agg[[index]][[var_rate]] <- gbd_city_region_data_agg[[index]][[var_med]] /
          gbd_city_region_data_agg[[index]]$population_number
      }
    }
  }
  
  index <- index + 1
}


# ---- chunk-1.6: Disbayes input generation ---- 

### The generations of inputs has two main sections and needs the above code to be ran first. 
### Sections main functions: GenInputDisbayes (generates inc, mort, prev rates per one and prevdenom, remission is set to 0) and Ci2NumDF which generates 
### num and denom for each of the localities for incidence, prevalence and mortality (later code sums localities into city regions)


### parameters

i_sex <- c('male', 'female')

# ---- chunk-1.6.1: GenInputDisbayes ----

### GenInputsDIsbayes: generates a data frame per city regions (and regions of England and countries in the UK) expanding 5-year estimates and values into
### one year intervals assuming that 5-yr age population is evenly distributed for each 1-yr within the interval and rates of dieseses are the same within the year
### interval. 

index <- 1

disbayes_input_list_city_regions <- list()

for (i in 1:length(gbd_city_region_data_agg)) {
  
  disbayes_input_list_city_regions[[index]] <- GenInpDisbayes(gbd_city_region_data_agg[[i]])
  
  
  names(disbayes_input_list_city_regions)[index] <- paste0(names(gbd_city_region_data_agg[i]))
  
  index <- index + 1
}


for (i in 1:length(disbayes_input_list_city_regions)){
  for (j in 1:length(disbayes_input_list_city_regions[[i]])) {
    disbayes_input_list_city_regions[[i]][[j]]$cityregion <- paste(names(disbayes_input_list_city_regions[i]))
  }
} 

# ---- chunk-1.6.3: Ci2NumDF ----

### Prepare data to process with Ci2NumDF


disbayes_input_list_city_regions_2 <- list()
index <- 1
for (i in 1:length(gbd_city_region_data)) {
  for (dm in 1:length(disease_measures_list)){
    for (d in 1:nrow(DISEASE_SHORT_NAMES)){
      in_measure <- disease_measures_list[dm] %>% as.character() %>% tolower()
      
      ### exclude ylds for now, we are interested in disbayes inputs but later may use ylds uncertainty parameters
      
      if (DISEASE_SHORT_NAMES$is_not_dis[d] != 0 || in_measure == "ylds (years lived with disability)"){
      }
      else {
        
        med <- paste0(in_measure, "_med_", DISEASE_SHORT_NAMES$sname[d])
        low <- paste0(in_measure, "_lower95_", DISEASE_SHORT_NAMES$sname[d])
        upper <- paste0(in_measure, "_upper95_", DISEASE_SHORT_NAMES$sname[d])
        
        ## These data is in 5-year age groups. 
        data <- gbd_city_region_data[[i]]
        
        disbayes_input_list_city_regions_2[[index]] <- dplyr::select(data, population_number, cityregion, location, sex_age_cat, med, low, upper)
        
        
        ##### CHRIS, ARE THE FOLLOWING CORRECT? POINT ESTIMATES AND CREDIBLE LIMITS AS RATES PER ONE?
        
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


# ---- chunk-1.6.4: Generate num and denoms using Ci2NumDF ----
### Generate num and denom using ci2num for incidence, prevalence and mortality
## trycatch is used to avoid issues with prevalence when running Ci2NumDF


tryCatchCi2NumDF <- function(x) tryCatch(Ci2NumDF(x), error = function(e) e)
disbayes_input_list_city_regions_3  <- lapply(disbayes_input_list_city_regions_2[[1]], tryCatchCi2NumDF)

 test <- Ci2NumDF(disbayes_input_list_city_regions_2[[1]])


# ---- chunk-1.6.5: Create a dataframe with all city regions data ----



#### Try to remove if not data frame

index <- 1
disbayes_input_list_city_regions_3b <- list()

for (i in 1:length(disbayes_input_list_city_regions_3)) {
  
  if(NCOL(disbayes_input_list_city_regions_3[[i]]) != as.numeric(4)) {}
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

#### Check NAs

NAs_test1 <- disbayes_input_list_city_regions_5[rowSums(is.na(disbayes_input_list_city_regions_5)) > 0,]

# disbayes_input_list_city_regions_5$indexagg <- gsub("'", '', disbayes_input_list_city_regions_5$indexagg)

disbayes_input_list_city_regions_6 <- disbayes_input_list_city_regions_5 %>% 
  mutate(index = indexagg) %>% 
  separate(indexagg, into=c("measure", "disease", "sex", "age", "cityregion"), sep = "_")
 # mutate_if(is.character)%>% 

NAs_test2 <- disbayes_input_list_city_regions_6[rowSums(is.na(disbayes_input_list_city_regions_6)) > 0,]


## Add new variable with mid-age group

disbayes_input_list_city_regions_6$agegr <- 0
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="Under 5"] <- 0
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="5 to 9"] <- 5
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="10 to 14"] <- 10
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="15 to 19"] <- 15
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="20 to 24"] <- 20
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="25 to 29"] <- 25
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="30 to 34"] <- 30
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="35 to 39"] <- 35
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="40 to 44"] <- 40
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="45 to 49"] <- 45
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="50 to 54"] <- 50
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="55 to 59"] <- 55
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="60 to 64"] <- 60
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="65 to 69"] <- 65
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="70 to 74"] <- 70
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="75 to 79"] <- 75
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="80 to 84"] <- 80
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="85 to 89"] <- 85
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="90 to 94"] <- 90
disbayes_input_list_city_regions_6$agegr [ disbayes_input_list_city_regions_6$age =="95 plus"] <- 95

city_regions_names <- unique(disbayes_input_list_city_regions_6$cityregion)
disease_disbayes <- unique(disbayes_input_list_city_regions_6$disease)
measure_disbayes <- unique(disbayes_input_list_city_regions_6$measure)
sex_disbayes <- unique(disbayes_input_list_city_regions_6$sex)


## To wider 


disbayes_input_list_city_regions_7 <- disbayes_input_list_city_regions_6 %>% 
  pivot_wider(id_cols = c(measure, disease, sex, age, cityregion, population_number, agegr), 
              names_from = measure, values_from = c(num, denom))

NAs_test3 <- disbayes_input_list_city_regions_7[rowSums(is.na(disbayes_input_list_city_regions_7)) > 0,]

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

# ---- chunk-1.6.6: Join dataframes with all disbayes inputs ----

## First data set rates
disbayes_inputs_df <- do.call(rbind, disbayes_input_list_city_regions)

### Some issue with disease column, which we do not need, to rbind to dataframe
# disbayes_inputs_df <- lapply(disbayes_inputs_df, function(x) { x["disease"] <- NULL; x })
disbayes_inputs_df <- dplyr::bind_rows(disbayes_inputs_df)

### CHECK WHAT HAPPENEND WITH CITY REGIONS AND THATN INDEX IS THE SAME AS IN DISBAYES INPUT DATAFRAME2
disbayes_inputs_df$index <- tolower(paste(disbayes_inputs_df$sex_disease, disbayes_inputs_df$age_cat, disbayes_inputs_df$cityregion, sep = "_"))

## Second list with num and denom
disbayes_inputs_df2 <- do.call(rbind, disbayes_input_list2)
disbayes_inputs_df2 <-   disbayes_inputs_df2[ -c(1:4,5,12) ]


## Final data set to process in disbayes. Filter data by city region, disease and sex. COMPARE with saved data in rds
disbayes_inputs_latest <- disbayes_inputs_df %>%
  left_join(disbayes_inputs_df2, by = "index")



disbayes_inputs_latest <- disbayes_inputs_latest[, !(colnames(disbayes_inputs_latest) %in% c("sex_index","index", "sex_disease"))]


names(disbayes_inputs_latest)[names(disbayes_inputs_latest) == "age_cat"] <- "age"


### Inspect NAs

NAs <- disbayes_inputs_latest[rowSums(is.na(disbayes_inputs_latest)) > 0,]

write_rds(disbayes_inputs_latest, paste0(relative_path_mslt, "data/city regions/Input disbayes/disbayes_inputs_new", ".rds"))





## CREATE ONE DATA FRAME WITH ALL LIST IN GBD DATA FRAME AGGREGATED, THEN, THIS INFO IS NEEDED IN COMPILING ALL DATA FOR MSLT
# 
# gbd_data <- plyr::ldply(gbd_city_region_data_agg, rbind)
# gbd_data$area <- gbd_data$.id
# 
# 
# 
# 
# 
# 
# ##### SEPARATE OUT THIS SECTIONs
# 
# 
# 
# # ---- chunk 1.6 Get Disbayes output ----
# 
# 
# ## From data generated by chris using Disbayes "mh-mslt/data/city regions/Output disbayes/cityregions_smoothed_res.rda"
# 
# ## add name to column outputs (column 0) NEED TO GENENERATE OUTPUTS LIKE CHRIS FROM THE DISBAYES SCRIPT
# 
# ### create column one with outcome and year
# cityregions_smoothed_res <- cbind(
#   mes=rownames(cityregions_smoothed_res), cityregions_smoothed_res)
# 
# ### Separate avoce in outcome and year
# cityregions_smoothed_res <- cbind(cityregions_smoothed_res, (str_split_fixed(cityregions_smoothed_res$
#                                                                                mes, fixed('['), 2)))
# 
# cityregions_smoothed_res <- cityregions_smoothed_res[ (cityregions_smoothed_res$`1` %in% c("inc", "cf", "prev")), ]
# cityregions_smoothed_res$`1` <- as.character(cityregions_smoothed_res$`1`)
# cityregions_smoothed_res$`2` <- as.character(cityregions_smoothed_res$`2`)
# cityregions_smoothed_res$`2` <- gsub("].*", "",cityregions_smoothed_res$`2`)
# 
# 
# ## Rename columns
# names(cityregions_smoothed_res)[names(cityregions_smoothed_res) == "1"] <- "rates"
# names(cityregions_smoothed_res)[names(cityregions_smoothed_res) == "2"] <- "year"
# 
# ## Rename string values inc to incidence, cf to case fatality and prev to prevalence
# 
# cityregions_smoothed_res <- cityregions_smoothed_res %>% 
#   mutate(rates = str_replace(rates, "inc", "incidence"))  %>%
#   mutate(rates = str_replace(rates, "cf", "case_fatality"))  %>%
#   mutate(rates = str_replace(rates, "prev", "prevalence"))
# 
# ## Move to columns for data for case_fatality, incidence and prevelence
# 
# cityregions_smoothed_res$disease_rate <- paste(cityregions_smoothed_res$rates, cityregions_smoothed_res$disease, sep = "_")
# cityregions_smoothed_res2 <- cityregions_smoothed_res %>% pivot_wider(id_cols = c(area, gender, model, year), names_from = disease_rate, values_from = c(med, lower95, upper95))
# names(cityregions_smoothed_res2) = gsub(pattern = "med_", replacement = "", x = names(cityregions_smoothed_res2))
# 
# disbayes_output <- cityregions_smoothed_res2 %>%
#   dplyr::rename(sex = gender) %>% 
#   mutate_if(is.factor, as.character)
# disbayes_output$year <- disbayes_output$year %>% as.numeric(disbayes_output$year)
# ## Change year to match mslt dataframe (0 to 100 years)
# disbayes_output$year[1:101] <- 0:100
# 
# disbayes_output$sex_age_area_cat <- paste(disbayes_output$sex,disbayes_output$year, disbayes_output$area, sep = "_"  )
# 
# #### Checking which variables have been smooth on disbayes
# ## Plot organised data by me
# plot(dplyr::filter(disbayes_output, area == "bristol", sex == "female") %>% dplyr::select(year, incidence_carc))
# 
# 
# 
# data_test <- cityregions_smoothed_res %>% mutate_if(is.factor, as.character) %>% 
#   dplyr::select(area, gender, disease, year, med, model, mes) %>%
#   dplyr::filter(area == "bristol", gender == "female", disease == "carc") %>%
#   dplyr::filter(str_detect(mes, "inc"))
# 
# plot(data_test$year, data_test$med)
# 
#  
#           
# # ---- chunk 1.7 ----
# 
# 
# ### Keep city regions as in mh-execute (bristol, greatermanchester, leeds, liverpool, london, northeast, nottingham, sheffield, westmidlands). 
# areas <- unique(disbayes_output$area)
# i_sex <- c('male', 'female')
# 
# mslt_df <- as.data.frame(NULL)
# 
# mslt_df_list <- list()
# 
# index <- 1
# 
# for (a in areas) {
#   
#   ### selected data here should be gbd_data with all data, see how the code works with it 
#   
#   data_1 <-  dplyr::filter(gbd_data, area == a)
#   data_2 <- dplyr::filter(disbayes_output, area == a)
# 
#   mslt_df_list[[index]] <- GenMSLTDF(data_1, data_2)
#  
#   
#   mslt_df_list[[index]]<- replace(mslt_df_list[[index]], is.na(mslt_df_list[[index]]), 0)
#   
#   
#   ### Change names to match with Rob's injury code
# 
#    names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "deaths_rate_pdri"] <- "deaths_rate_pedestrian"
#    names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "ylds (years lived with disability)_rate_pdri"] <- "ylds_rate_pedestrian"
#   
#    names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "deaths_rate_cyri"] <- "deaths_rate_cyclist"
#    names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "ylds (years lived with disability)_rate_cyri"] <- "ylds_rate_cyclist"
#   
#    names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "deaths_rate_mtri"] <- "deaths_rate_motorcyclist"
#    names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "ylds (years lived with disability)_rate_mtri"] <- "ylds_rate_motorcyclist"
#   
#    names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "deaths_rate_mvri"] <- "deaths_rate_motor"
#    names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "ylds (years lived with disability)_rate_mvri"] <- "ylds_rate_motor"
#   
#   
#    names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "deaths_rate_otri"] <- "deaths_rate_other"
#    names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "ylds (years lived with disability)_rate_otri"] <- "ylds_rate_other"
#   
#    names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "deaths_rate_lwri"] <- "deaths_rate_lri"
#    names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "ylds (years lived with disability)_rate_lwri"] <- "ylds_rate_lri"
#   
#    mslt_df_list[[index]]$sex <- as.character(mslt_df_list[[index]]$sex)
#    
#    
#    
#    
#   index <- index + 1
#   
# }
# 
# ### Save city regions data frames to mh-execute/inputes
# 
# for (i in 1:length(mslt_df_list)) {
# 
# write_csv(mslt_df_list[[i]], paste0(relative_path_execute, "inputs/mslt/", unique(mslt_df_list[[i]]$area), "_mslt", ".csv"))
#   
# }
# 
# ### Prepare city regions data for ithimr health calculations (for deaths and ylls
# 
# # ---- chunk-2: Data preparation ITHIMR data inputs ----
# 
# population_cityregions <- dplyr::select(gbd_data, sex, age, population_number, cityregion) %>%
#   mutate(index_pop = tolower(paste(sex, age, cityregion, sep = "_")))
# 
# GBD_ithim <- read_csv("C:/Users/rstudio/Dropbox/Collaborations/James Woodcock/Metahit/Data/GBD2017/ITHIM/GBD.csv") %>%
#   dplyr::filter(metric_name == "Number") %>% 
#   dplyr::select(measure_name, location_name, sex_name, age_name, cause_name, val) %>%
#   left_join(local_goverment_areas, by = c("location_name" = "location")) %>% 
#   mutate(index_add = paste(measure_name, sex_name, age_name, cause_name, cityregion, sep = "_")) %>%
#   dplyr::select(measure_name, location_name, sex_name, age_name, cause_name, val, cityregion, index_add) %>%
#   group_by(index_add) %>%
#   summarise_if(is.numeric, funs(sum)) %>% 
#   separate(index_add, c("measure_name", "sex_name", "age_name", "cause_name", "cityregion"), "_") %>% 
#   dplyr::filter(cityregion %in% areas) %>%
#   mutate(index_pop = tolower(paste(sex_name, age_name, cityregion, sep = "_"))) %>%
#   left_join(dplyr::select(population_cityregions, c(population_number, index_pop)), by = "index_pop", keep = FALSE) %>%
#   dplyr::select(!index_pop)
# 
# 
# ### Change names to match format for ITHIMR
# GBD_ithim$age_name[GBD_ithim$age_name =="95 plus"] <- '95 to 99'
# GBD_ithim$age_name[GBD_ithim$age_name =="Under 5"] <- '0 to 4'
# 
# 
# # addrow_age_95 <- dplyr::filter(GBD_ithim, age_name == "95 to 99")
# # GBD_ithim <- bind_rows(GBD_ithim, addrow_age_95)
#   
# GBD_ithim_list <- split(GBD_ithim , f = GBD_ithim$cityregion)
#   
# for (i in 1:length(GBD_ithim_list)) {
#   
#    write_csv(GBD_ithim_list[[i]], paste0(relative_path_execute, "inputs/gbd/", unique(mslt_df_list[[i]]$area), ".csv"))
#    
#  }
# 

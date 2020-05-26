
## Create R Markdown for data preparation

# ---- chunk-intro ----
rm (list = ls())
options(scipen=999)
library(readr)
library(rlist)
library(dplyr)
library(tidyverse)
library(conflicted)

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

## Get data from GBD dowloaded data for UK (all localities)
## Use code developed by Marko Tainio to extract zip files
## Created in February-March 2019 by Marko Tainio (modified by Belen Zapata June 2019 for Metahit project)
## This script extracts required Global Burden of Disease data from the zip files dowloaded from http://ghdx.healthdata.org/gbd-results-tool
## by first extracting zip-files, then reading csv file, adding required data to combined dataframe
## and finally deleiting extracted files. Resulting dataframe is then saved as csv-file.

## Defining folder where the data is stored (stored externally in my dropbox as the GBD files are large)
## CHANGE TO v-DRIVE

work_folder <- "C:/Users/e95517/Dropbox/"
home_folder <- "C:/Users/rstudio/Dropbox/"
v_folder <- "V:/Studies/MOVED/HealthImpact/Data/Global_Burden_Disease_Metahit/"
vm_folder <- "/media/sf_Dropbox/"

## Change folder to work or home
# CHANGE DATA FOLDER
data_folder <- paste0(home_folder, "Collaborations/James Woodcock/Metahit/Data/GBD2017/")
temp_folder <- paste0(data_folder,"temp") 
result_folder <- paste0(data_folder,"final")
gbdfile_name_new <- "IHME-GBD_2017_DATA-3e0b192d-" # CHANGE NAME WHEN NEW DATA IS DOWNLOADED 
gbdfile_name_old <- "IHME-GBD_2017_DATA-ac95a757-" # CHANGE NAME WHEN NEW DATA IS DOWNLOADED 
### New data: "C:\Users\e95517\Dropbox\Collaborations\James Woodcock\Metahit\Data\GBD2017\IHME-GBD_2017_DATA-3e0b192d-1.zip"
### Old data: "C:\Users\e95517\Dropbox\Collaborations\James Woodcock\Metahit\Data\GBD2017\IHME-GBD_2017_DATA-ac95a757-1.zip"


## Loop to extract zip file data for data with all diseases
##### All diseases in new meta analysis
data_extracted_new <- NULL
for (i in 1:5) { # LOOP NUMBER DEPENDS ON NUMBER OF ZIP FILES, HERE I JUST GOT DATA FOR ALL LOCALITIES IN ENGLAND
  file_number <- i
  
  file_select <- paste0(data_folder,gbdfile_name_new, i,".zip")
  
  unzip(file_select, exdir=temp_folder)
  
  data_read <- read_csv((paste0(temp_folder,"/", gbdfile_name_new, i, ".csv")))
  file.remove(paste0(temp_folder,"/", gbdfile_name_new, i, ".csv"))
  data_read <- subset(data_read, location_name %in% local_goverment_areas$location) # location name is easier to identify
  
  data_extracted_new <- rbind(data_extracted_new, data_read)
}

unlink(paste0(temp_folder), recursive = TRUE)


##### Old diseases


data_extracted <- data_extracted_new
# ---- chunk-1.2: Define parameters from data ----

## Define measure (e.g. deaths) and cause parameters (e.g. all causes, breast cancer) (this is to avoid hard coding the parameters)

## Min Length is not changing anything, how can we make it characters in the first place, rather than having to ocnvert below before running RunLocDF?

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

write_csv(DISEASE_SHORT_NAMES, paste0(relative_path_mslt,"data/parameters/disease_names.csv"))

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


### This code takes about 0.5 hour to run CHECK WITH ROB AND ALAN HOW TO MAKE FASTER


index <- 1
gbd_loc_data_processed <- list()

for (i in 1:length(city_regions_list_loc)) {

gbd_loc_data_processed[[index]] <- lapply(city_regions_list_loc[[i]], RunLocDf)

index <- index + 1

}



### Delete null data frames within lists

gbd_loc_data_processed <-  list.clean(gbd_loc_data_processed, fun = is.null, recursive = TRUE)


# ---- chunk-1.5: Create data frame for city region with all localities ---- 

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




## Create aggregated data frame (sums all numbers from localities within a city region) SEE HOW THIS IS WORKING, best to continue 
### with dataframe above and then have a combined dataset with all possible inputs for disbayes

### Rename gbd_city_region_data, as cityregionand location variables are needed in disbayes data_prep
# 
gbd_city_region_data_2 <- list()
for (i in 1:length(gbd_city_region_data)) {
  gbd_city_region_data_2[[i]] <- gbd_city_region_data[[i]]
  #[ -c(1,3) ] 
}

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

## CREATE ONE DATA FRAME WITH ALL LIST IN GBD DATA FRAME AGGREGATED, THEN, THIS INFO IS NEEDED IN COMPILING ALL DATA FOR MSLT

gbd_data <- plyr::ldply(gbd_city_region_data_agg, rbind)
gbd_data$area <- gbd_data$.id


# ---- chunk 1.6 Get Disbayes output ----


## From data generated by chris using Disbayes "mh-mslt/data/city regions/Output disbayes/cityregions_smoothed_res.rda"

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

#### Checking which variables have been smooth on disbayes
## Plot organised data by me
plot(dplyr::filter(disbayes_output, area == "bristol", sex == "female") %>% dplyr::select(year, incidence_carc))



data_test <- cityregions_smoothed_res %>% mutate_if(is.factor, as.character) %>% 
  dplyr::select(area, gender, disease, year, med, model, mes) %>%
  dplyr::filter(area == "bristol", gender == "female", disease == "carc") %>%
  dplyr::filter(str_detect(mes, "inc"))

plot(data_test$year, data_test$med)

 
          
# ---- chunk 1.7 ----


### Keep city regions as in mh-execute (bristol, greatermanchester, leeds, liverpool, london, northeast, nottingham, sheffield, westmidlands). 
areas <- unique(disbayes_output$area)
i_sex <- c('male', 'female')

mslt_df <- as.data.frame(NULL)

mslt_df_list <- list()

index <- 1

for (a in areas) {
  
  ### selected data here should be gbd_data with all data, see how the code works with it 
  
  data_1 <-  dplyr::filter(gbd_data, area == a)
  data_2 <- dplyr::filter(disbayes_output, area == a)

  mslt_df_list[[index]] <- GenMSLTDF(data_1, data_2)
 
  
  mslt_df_list[[index]]<- replace(mslt_df_list[[index]], is.na(mslt_df_list[[index]]), 0)
  
  
  ### Change names to match with Rob's injury code

   names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "deaths_rate_pdri"] <- "deaths_rate_pedestrian"
   names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "ylds (years lived with disability)_rate_pdri"] <- "ylds_rate_pedestrian"
  
   names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "deaths_rate_cyri"] <- "deaths_rate_cyclist"
   names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "ylds (years lived with disability)_rate_cyri"] <- "ylds_rate_cyclist"
  
   names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "deaths_rate_mtri"] <- "deaths_rate_motorcyclist"
   names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "ylds (years lived with disability)_rate_mtri"] <- "ylds_rate_motorcyclist"
  
   names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "deaths_rate_mvri"] <- "deaths_rate_motor"
   names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "ylds (years lived with disability)_rate_mvri"] <- "ylds_rate_motor"
  
  
   names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "deaths_rate_otri"] <- "deaths_rate_other"
   names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "ylds (years lived with disability)_rate_otri"] <- "ylds_rate_other"
  
   names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "deaths_rate_lwri"] <- "deaths_rate_lri"
   names(mslt_df_list[[index]])[names(mslt_df_list[[index]]) == "ylds (years lived with disability)_rate_lwri"] <- "ylds_rate_lri"
  
   mslt_df_list[[index]]$sex <- as.character(mslt_df_list[[index]]$sex)
   
   
   
   
  index <- index + 1
  
}

### Save city regions data frames to mh-execute/inputes

for (i in 1:length(mslt_df_list)) {

write_csv(mslt_df_list[[i]], paste0(relative_path_execute, "inputs/mslt/", unique(mslt_df_list[[i]]$area), "_mslt", ".csv"))
  
}

### Prepare city regions data for ithimr health calculations (for deaths and ylls

# ---- chunk-2: Data preparation ITHIMR data inputs ----

population_cityregions <- dplyr::select(gbd_data, sex, age, population_number, cityregion) %>%
  mutate(index_pop = tolower(paste(sex, age, cityregion, sep = "_")))

GBD_ithim <- read_csv("C:/Users/rstudio/Dropbox/Collaborations/James Woodcock/Metahit/Data/GBD2017/ITHIM/GBD.csv") %>%
  dplyr::filter(metric_name == "Number") %>% 
  dplyr::select(measure_name, location_name, sex_name, age_name, cause_name, val) %>%
  left_join(local_goverment_areas, by = c("location_name" = "location")) %>% 
  mutate(index_add = paste(measure_name, sex_name, age_name, cause_name, cityregion, sep = "_")) %>%
  dplyr::select(measure_name, location_name, sex_name, age_name, cause_name, val, cityregion, index_add) %>%
  group_by(index_add) %>%
  summarise_if(is.numeric, funs(sum)) %>% 
  separate(index_add, c("measure_name", "sex_name", "age_name", "cause_name", "cityregion"), "_") %>% 
  dplyr::filter(cityregion %in% areas) %>%
  mutate(index_pop = tolower(paste(sex_name, age_name, cityregion, sep = "_"))) %>%
  left_join(dplyr::select(population_cityregions, c(population_number, index_pop)), by = "index_pop", keep = FALSE) %>%
  dplyr::select(!index_pop)


### Change names to match format for ITHIMR
GBD_ithim$age_name[GBD_ithim$age_name =="95 plus"] <- '95 to 99'
GBD_ithim$age_name[GBD_ithim$age_name =="Under 5"] <- '0 to 4'


addrow_age_95 <- dplyr::filter(GBD_ithim, age_name == "95 to 99")
GBD_ithim <- bind_rows(GBD_ithim, addrow_age_95)
  
GBD_ithim_list <- split(GBD_ithim , f = GBD_ithim$cityregion)
  
for (i in 1:length(GBD_ithim_list)) {
  
   write_csv(GBD_ithim_list[[i]], paste0(relative_path_execute, "inputs/gbd/", unique(mslt_df_list[[i]]$area), ".csv"))
   
 }


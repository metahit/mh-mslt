
## Create R Markdown for data preparation

# ---- chunk-intro ----

## City regions, prepare a dataset for each of them by aggregating local goverment areas

# Sheffield City Region Combined Authority: Barnsley, Doncaster, Rotherham, Sheffield.
# 
# North East Combined Authority: County Durham, Gateshead, Newcastle upon Tyne, North Tyneside, Northumberland, South Tyneside, Sunderland.
# 
# Greater Manchester Combined Authority: Bolton, Bury, Manchester, Oldham, Rochdale, Salford, Stockport, Tameside, Trafford, Wigan.
# 
# Liverpool City Region Combined Authority: Halton, Knowsley, Liverpool, St. Helens, Sefton, Wirral.
# 
# West Yorkshire Combined Authority: Bradford, Calderdale, Kirklees, Leeds, Wakefield.
# 
# Bristol: Bath and North East Somerset, City of Bristol, North Somerset, South Gloucestershire.
# 
# Nottingham: Ashfield, Bassetlaw, Broxtowe, Gedling, Mansfield, Nottingham, Newark and Sherwood, Rushcliffe. (NO GBD DATA AVAILABLE)
# 
# West Midlands Combined Authority: Birmingham, Coventry, Dudley, Sandwell, Solihull, Walsall, Wolverhampton.


# ---- chunk-1: Data preparation ----

## Run mslt_code lines to get requiered packages

## Relative paths

relative_path_execute <- '../mh-execute/'
relative_path_mslt <- '../mh-mslt/'

## Get look up table from mh-execute

look_up_table <- read_csv(paste0(relative_path_execute, 'inputs/mh_regions_lad_lookup.csv'))

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

work_folder <- "C:/Users/e95517/"
home_folder <- "C:/Users/Bele/"
v_folder <- "V:/Studies/MOVED/HealthImpact/Data/Global_Burden_Disease_Metahit/"

## Change folder to work or home
# CHANGE DATA FOLDER
data_folder <- paste0(work_folder, "Dropbox/Collaborations/James Woodcock/Metahit/Data/GBD2017/")
temp_folder <- paste0(data_folder,"temp") 
result_folder <- paste0(data_folder,"final")
gbdfile_name <- "IHME-GBD_2017_DATA-ac95a757-" # CHANGE NAME WHEN NEW DATA IS DOWNLOADED 

## Loop to extract zip file data

data_extracted <- NULL
for (i in 1:4) { # LOOP NUMBER DEPENDS ON NUMBER OF ZIP FILES, HERE I JUST GOT DATA FOR ALL LOCALITIES IN ENGLAND
  file_number <- i
  
  file_select <- paste0(data_folder,gbdfile_name, i,".zip")
  
  unzip(file_select, exdir=temp_folder)
  
  data_read <- read_csv((paste0(temp_folder,"/", gbdfile_name, i, ".csv")))
  file.remove(paste0(temp_folder,"/", gbdfile_name, i, ".csv"))
  data_read <- subset(data_read, location_name %in% local_goverment_areas$location) # location name is easier to identify
  
  data_extracted <- rbind(data_extracted,data_read)
}

unlink(paste0(temp_folder), recursive = TRUE)

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

DISEASE_SHORT_NAMES$females <- 1


## Replace NAs with blank

DISEASE_SHORT_NAMES$acronym[is.na(DISEASE_SHORT_NAMES$acronym)] <- "no_pif"


## Add column to match names from mh-execute

write_csv(DISEASE_SHORT_NAMES, "data/parameters/disease_names.csv")

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
#### Drop location and city region (sum cannot summ string variables)
### Rename gbd_city_region_data, as cityregionand location variables are needed in disbayes data_prep

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
  

           
  ## Calculate rates per one. Needed for mslt_code

  for (dm in 1:length(disease_measures_list)){
     for (d in 1:nrow(DISEASE_SHORT_NAMES)){
         dn <- DISEASE_SHORT_NAMES$disease[d]
         dmeasure <- disease_measures_list[dm] %>% as.character()

         gbd_city_region_data_agg[[index]][[tolower(paste(dmeasure, "rate", DISEASE_SHORT_NAMES$sname[d], sep = "_"))]] <- gbd_city_region_data_agg[[index]][[tolower(paste(dmeasure, "med", DISEASE_SHORT_NAMES$sname[d], sep = "_"))]]/
                                                                                                                             gbd_city_region_data_agg[[index]]$population_number

              }
         }
           
  suppressWarnings(names(gbd_city_region_data_agg)[index] <- paste(city_regions_list_loc[[i]][[1]]$cityregion, sep = '_'))
  
  ## Save as rds for each city region
  
   write_rds(gbd_city_region_data_agg[[index]], paste0(relative_path_mslt, "data/city regions/GBD sorted/", unique(city_regions_list_loc[[i]][[1]]$cityregion), ".rds"))
  

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


# ---- chunk 1.7 ----

areas <- unique(disbayes_output$area)

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

### These should be a folder with the data for all localities of interest. 
# write_csv(mslt_df, ("data/mslt_df.csv"))

## Get PIFS from Rob adn expand from five year age groups to one. 

pif <- read_csv("data/pif.csv")

pif$age <- 0
pif$age [pif$age_cat =="16-19"] <- 17
pif$age [pif$age_cat =="20-24"] <- 22
pif$age [pif$age_cat =="25-29"] <- 27
pif$age [pif$age_cat =="30-34"] <- 32
pif$age [pif$age_cat =="35-39"] <- 37
pif$age [pif$age_cat =="40-44"] <- 42
pif$age [pif$age_cat =="45-49"] <- 47
pif$age [pif$age_cat =="50-54"] <- 52
pif$age [pif$age_cat =="55-59"] <- 57
pif$age [pif$age_cat =="60-64"] <- 62
pif$age [pif$age_cat =="65-69"] <- 67
pif$age [pif$age_cat =="70-74"] <- 72
pif$age [pif$age_cat =="75-79"] <- 77
pif$age [pif$age_cat =="80-84"] <- 82
pif$age [pif$age_cat =="85-89"] <- 87
pif$age [pif$age_cat =="90-94"] <- 92
pif$age [pif$age_cat =="95-120"] <- 97

## Change names to get rid of risk factors combinations in the name (BEST IF I DO NOT HAVE TO DO THIS MANUALLY)

names(pif)[names(pif) == "scen_pif_pa_ap_noise_no2_ihd"] <- "pif_ihd"
names(pif)[names(pif) == "scen_pif_pa_ap_stroke"] <- "pif_stroke"
names(pif)[names(pif) == "scen_pif_pa_colon" ] <- "pif_colon"
names(pif)[names(pif) == "scen_pif_pa_t2d"] <- "pif_t2d"
names(pif)[names(pif) == "scen_pif_pa_endo"] <- "pif_endo"
names(pif)[names(pif) == "scen_pif_pa_ap_lc"] <- "pif_lc"
names(pif)[names(pif) == "scen_pif_ap_lri"] <- "pif_lri"
names(pif)[names(pif) == "scen_pif_ap_copd"] <- "pif_copd"
names(pif)[names(pif) == "scen_pif_pa_breast"] <- "pif_breast"

names(pif)[names(pif) == "scen_cyclist_Fatal"] <- "pif_cyclist_deaths"
names(pif)[names(pif) == "scen_pedestrian_Fatal"] <- "pif_pedestrian_deaths"
names(pif)[names(pif) == "scen_cyclist_Serious"] <- "pif_cyclist_ylds"
names(pif)[names(pif) == "scen_pedestrian_Serious"] <- "pif_pedestrian_ylds"
names(pif)[names(pif) == "scen_car/taxi_Fatal"] <- "pif_motor_deaths"
names(pif)[names(pif) == "scen_motorcycle_Fatal"  ] <- "pif_motorcyclist_deaths"
names(pif)[names(pif) == "scen_car/taxi_Serious"] <- "pif_motor_ylds"
names(pif)[names(pif) == "scen_motorcycle_Serious"  ] <- "pif_motorcyclist_ylds"


## Repeat pif lri for deaths and ylds

pif$pif_lri_deaths <- pif$pif_lri
pif$pif_lri_ylds <- pif$pif_lri

## Delete pif_lri

pif <- select(pif, -c(pif_lri))


### mslt_df names are not matching pifs names, need to change this, preferably, not manually

#### MANUALLY TO CHECK THAT IT WORKS FOR ROAD INJURIES


p <- dplyr::filter(pif, sex == "male")

outage <- min(p$age):100

ind <- findInterval(outage, p$age)
pif_expanded <- p[ind,]
pif_expanded$age <- outage

p_1 <- filter(pif, sex == "female")

outage <- min(p_1$age):100

ind <- findInterval(outage, p_1$age)
pif_expanded_1 <- p_1[ind,]
pif_expanded_1$age <- outage


pif_expanded <- rbind(pif_expanded, pif_expanded_1)

write_csv(pif_expanded, "data/pif_expanded.csv")

#### Example for one city region
write_csv(mslt_df_list[[1]], "data/mslt_bristol.csv")

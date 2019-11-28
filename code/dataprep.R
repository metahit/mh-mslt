
## Create R Markdown for data preparation

# ---- chunk-intro ----

## City regions, prepare a dataset for each of them

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

## Source functions

source("code/functions.R")


## Define parameters

year <- 2017 # Only downloaded data for 2017

i_age_cohort <- c(17, 22, 27, 32, 37, 42, 47, 52, 57, 62, 67, 72, 77, 82, 87, 92, 97)

i_sex <- c("male", "female")

## Relative paths

relative_path_execute <- '../mh-execute/'
relative_path_mslt <- '../mh-mslt/'

## Get look up table from mh-execute

look_up_table <- read_csv(paste0(relative_path_execute, 'inputs/mh_regions_lad_lookup.csv'))


## Dataframe with local goverment areas within each city region


local_goverment_areas <- look_up_table 

## Add non city regions names
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

# ---- chunk-1.1: Get Global Buden of Disease data ----

## GBD MISSING DATA FOR NOTTINGHAM: Ashfield, Bassetlaw, Broxtowe, Gedling, Mansfield, Newark and Sherwood, Rushcliffe and City of London. 

## Get data from GBD dowloaded data for England (all localities)
## Use code developed by Marko Tainio to extract zip files
## Created in February-March 2019 by Marko Tainio (modified by Belen Zapata June 2019 for Metahit project)
## This script extracts required Global Burden of Disease data from the zip files dowloaded from http://ghdx.healthdata.org/gbd-results-tool
## by first extracting zip-files, then reading csv file, adding required data to combined dataframe
## and finally deleiting extracted files. Resulting dataframe is then saved as csv-file.

## Defining folder where the data is stored (stored externally in my box as the GBD files are large)
## CHANGE TO v-DRIVE

work_folder <- "C:/Users/e95517/"
home_folder <- "C:/Users/Bele/"



data_folder <- paste0(home_folder, "Dropbox/Collaborations/James Woodcock/Metahit/Data/GBD2017/")
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

disease_short_names <- data.frame(disease = tolower(as.character(unique(data_extracted$cause_name))), 
                                  sname = tolower(abbreviate(unique(data_extracted$cause_name, max = 2))),
                                  stringsAsFactors = F)

disease_short_names <- disease_short_names %>% mutate(is_not_dis = ifelse((str_detect(disease, "injuries") |
                                                                             str_detect(disease, "All causes") |
                                                                             str_detect(disease, "Lower respiratory infections")), 
                                                                          1, 0) )

disease_short_names[disease_short_names$sname == "allc", "is_not_dis"] <- 2

disease_short_names[disease_short_names$sname == "lwri", "is_not_dis"] <- 1


### Combine with acronyms from execute-mh

## Get execute-mh diseases (CHECK WITH ALI TO USE RELATIVE PATH TO READ DIRECLTY FROM MH-EXECUTE DIRECTORY, DATA PREP??)

disease_names_execute <- read_csv(paste0('../mh-execute/', "inputs/dose_response/disease_outcomes_lookup.csv"))

disease_names_execute <- disease_names_execute[1:2]
disease_names_execute$disease <- tolower(disease_names_execute$GBD_name)

disease_short_names <- left_join(disease_short_names, disease_names_execute, by = "disease")

## Add injuries

disease_short_names$acronym <- ifelse(str_detect(disease_short_names$disease, "injuries"), disease_short_names$disease, disease_short_names$acronym)

## Only keep first word for acronyns

disease_short_names$acronym <- word(disease_short_names$acronym, 1)

## Add males and females only diseases

disease_short_names$males <- ifelse(disease_short_names$disease %in% c("breast cancer", "uterine cancer"), 0, 1)

disease_short_names$females <- 1


## Replace NAs with blank

disease_short_names$acronym[is.na(disease_short_names$acronym)] <- "no_pif"


## Add column to match names from mh-execute

write_csv(disease_short_names, "data/parameters/disease_names.csv")

disease_measures_list <- data.frame(measure = unique(data_extracted$measure_name)) %>%
  pull(measure) %>%
  as.character() %>%
  as.list()

# ---- chunk-2: Dismod and Disbayes data input preparation ----

gbd_input <- data_extracted

# ---- chunk-2.1: Clean data ----

names(gbd_input) = gsub(pattern = "_name", replacement = "", x = names(gbd_input))

gbd_input <- select(gbd_input,-contains("id"))

# gbd_input <- filter(gbd_input, location %in% localities) %>% mutate_if(is.factor, as.character)

gbd_input$cause <- tolower(gbd_input$cause) 

gbd_input <- left_join(local_goverment_areas, gbd_input, by = "location")


# ---- chunk-2.2: Sort data per local goverment area ----

## We first derive populaiton and cases numbers (e.g. all cause mortality) for each locality and then aggregate at the City Region level. 

city_regions_list <- split(gbd_input , f = gbd_input$cityregion)

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
    
# ---- chunk-2.3: Create data frame for city region with all localities ---- 

index <- 1
gbd_city_region_data <- list()

for (i in 1:length(gbd_loc_data_processed)){
  

  gbd_city_region_data[[index]] <- bind_rows(gbd_loc_data_processed[[i]], .id = 'number')
  
  ## Drop number columns
  
  gbd_city_region_data[[index]] <- gbd_city_region_data[[index]][ -c(1) ]
  
  ## Clean dataframes per city regions
  
  gbd_city_region_data[[index]] <- dplyr::select(gbd_city_region_data[[index]], -contains('rate')) %>% mutate_if(is.factor, as.character) 
  
  gbd_city_region_data[[index]]$sex_age_cat <- paste(gbd_city_region_data[[index]]$sex, gbd_city_region_data[[index]]$age, sep = "_")
  
  gbd_city_region_data[[index]] <- select(gbd_city_region_data[[index]], -c(age, sex, location))
  
  suppressWarnings(names(gbd_city_region_data)[index] <- paste(city_regions_list_loc[[i]][[1]]$cityregion, sep = '_'))
  
  index <- index + 1
  
}

# View(gbd_city_region_data[[1]])

## Create aggregated data frame (sums all numbers from localities within a city region)

gbd_city_region_data_agg <- list()
index <- 1

for (i in 1:length(gbd_city_region_data)) {
  gbd_city_region_data_agg[[index]] <- gbd_city_region_data[[i]] %>% 
                                            group_by(sex_age_cat) %>%
                                            summarise_all(funs(sum))%>%
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
           
  ## Calculate rates per one
           
  for (dm in 1:length(disease_measures_list)){
    for (d in 1:nrow(disease_short_names)){
        dn <- disease_short_names$disease[d]
        dmeasure <- disease_measures_list[dm] %>% as.character()
               
        gbd_city_region_data_agg[[index]][[tolower(paste(dmeasure, "rate", disease_short_names$sname[d], sep = "_"))]] <- gbd_city_region_data_agg[[index]][[tolower(paste(dmeasure, "number", disease_short_names$sname[d], sep = "_"))]]/
                                                                                                                            gbd_city_region_data_agg[[index]]$population_number
               
             }
           }
           
  suppressWarnings(names(gbd_city_region_data_agg)[index] <- paste(city_regions_list_loc[[i]][[1]]$cityregion, sep = '_'))
  
  ## Save as rds (path)
  
   write_rds(gbd_city_region_data_agg[[index]], paste0(relative_path_mslt, "data/city regions/GBD sorted/", unique(city_regions_list_loc[[i]][[1]]$cityregion), ".rds"))
  

  index <- index + 1
}


### Check that numbers for regions () add up to England total

#### Population

##### UK
UK_population_total<- sum(gbd_city_region_data_agg[["United Kingdom"]]$population_number)
print(UK_population_total)

##### Countries

England_population_total <- sum(gbd_city_region_data_agg[["England"]]$population_number)
print(England_population_total)

Wales_population_total <- sum(gbd_city_region_data_agg[["Wales"]]$population_number)
print(Wales_population_total)

Scotland_population_total <- sum(gbd_city_region_data_agg[["Scotland"]]$population_number)
print(Scotland_population_total)

Northern_Ireland_population_total <- sum(gbd_city_region_data_agg[["Northern Ireland"]]$population_number)
print(Northern_Ireland_population_total)

total_countries <- sum(England_population_total, Wales_population_total, Scotland_population_total, Northern_Ireland_population_total)
print(total_countries)

###### Regions of England test sum

regions_England <- c("East Midlands", "East of England", "Greater London", "North East England", "North West England", "South East England", "South West England", "West Midlands", "Yorkshire and the Humber")

England_ihd_deaths <- sum(gbd_city_region_data_agg[["England"]]$deaths_number_ishd)
print(England_ihd_deaths)


total_regions_england_ihd_deaths <- list()

index <- 1

for (i in regions_England) {
  
  total_regions_england_ihd_deaths[[index]] <- sum(gbd_city_region_data_agg[[paste0(i)]]$deaths_number_ishd)
  
  index <- index + 1
}


sum_total_regions_ihd_deaths <- sum(total_regions_england_ihd_deaths[[1]], total_regions_england_ihd_deaths[[2]], total_regions_england_ihd_deaths[[3]], 
                                    total_regions_england_ihd_deaths[[4]], total_regions_england_ihd_deaths[[5]], total_regions_england_ihd_deaths[[6]],
                                    total_regions_england_ihd_deaths[[7]], total_regions_england_ihd_deaths[[8]], total_regions_england_ihd_deaths[[9]])

print(sum_total_regions_ihd_deaths)

# ---- chunk-3: Disbayes ----

## Disbayes data preparation

library(devtools)

## The code below generates age, sex and disease specific data frames to process with disbayes. 
## Chris Jackson generated the code for one dataset and I added a loop to do all diseases by age an sex. 

index <- 1

disbayes_input_list_city_regions <- list()

for (i in 1:length(gbd_city_region_data_agg)) {
  
  disbayes_input_list_city_regions[[index]] <- GenInpDisbayes(gbd_city_region_data_agg[[i]])
  
  names(disbayes_input_list_city_regions)[index] <- paste0(names(gbd_city_region_data_agg[i]))
  
  index <- index + 1
  
}

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



# test <- disbayes_input_list_city_regions[[1]][[1]]

## Run Disbayes (replaced with new code)

library(disbayes)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

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

## Create list with data needs for multistate life table processing (case fatality and incidence) (OLD Code, delete)
### NEW Code using Chris outcomes for disbayes in a table


## add name to column outputs (column 0)

cityregions_smoothed_res <- cbind(outcomes=rownames(cityregions_smoothed_res), cityregions_smoothed_res) 


cityregions_smoothed_res <- cbind(cityregions_smoothed_res, (str_split_fixed(cityregions_smoothed_res$outcomes, fixed('['), 2)))
cityregions_smoothed_res <- cityregions_smoothed_res[ (cityregions_smoothed_res$`1` %in% c("inc", "cf", "prev")), ]
cityregions_smoothed_res$`1` <- as.character(cityregions_smoothed_res$`1`)
cityregions_smoothed_res$`2` <- as.character(cityregions_smoothed_res$`2`)
cityregions_smoothed_res$`2` <- gsub("].*", "",cityregions_smoothed_res$`2`)


## Rename 1 adn 2
names(cityregions_smoothed_res)[names(cityregions_smoothed_res) == "1"] <- "rates"
names(cityregions_smoothed_res)[names(cityregions_smoothed_res) == "2"] <- "year"
## Keep incidence, case fatality, prevalence and mortality

#Extract unique values to create disease life table inputs per city regions
area <- as.character(unique(cityregions_smoothed_res$area))
disease <- as.character(unique(cityregions_smoothed_res$disease))
rates <- as.character(unique(cityregions_smoothed_res$rates))
gender <- as.character(unique(cityregions_smoothed_res$gender))


## Test here creating data set per area (first filter, otherwise another layer with the areas)

bristol_test <- dplyr::filter(cityregions_smoothed_res, area == "bristol")

## Create columns for rate and age

bristol_test$disease_rate <- paste(bristol_test$disease, bristol_test$rates)
bristol_test_1 <- spread(bristol_test, key = disease_rate, value = med)

## Create list with data needs for multistate life table processing (case fatality and incidence) (OLD Code, delete)

disease_lifetable_inputs_list <-  list()

for (i in 1:length(disbayes_output_list)){
  for (d in 1:nrow(disease_short_names)){
    
    ## Create list same lenght as outputs   
    disease_lifetable_inputs_list[[i]] <- disbayes_input_list[[i]]
    ## Add column names for incidence and case fatality disease
    disease_lifetable_inputs_list[[i]]$case_fatality  <- disbayes_output_list[[i]][1:101, 1]
    disease_lifetable_inputs_list[[i]]$prevalence  <- disbayes_output_list[[i]][102:202, 1]
    disease_lifetable_inputs_list[[i]]$incidence <- disbayes_input_list[[i]]$inc
    
    
    disease_lifetable_inputs_list[[i]][[paste("sex_age_cat", sep = "")]] <- paste(disease_lifetable_inputs_list[[i]]$sex,disease_lifetable_inputs_list[[i]]$age, sep = "_"  )
    
    # disease_lifetable_inputs_list[[i]]$disease_sex_match <- disbayes_output_list[[i]]$sex_disease
    
  }
}






## Create a data frame with all diseases case fatality and incidence to process in mslt_code disease code

# ---- chunk-4: Disease data sorting for mslt-code ----

disease_measures <- NULL
col_names <- colnames(disease_lifetable_inputs_list[[1]])
for (j in 1:length(disease_lifetable_inputs_list)){
  current_table <- disease_lifetable_inputs_list[[j]]
  colnames(current_table) <- col_names
  disease_measures <- rbind(disease_measures,
                           current_table)
}

disease_measures_age <- disease_measures %>%
  select(age,sex,sex_age_cat)

disease_measures_casefatality <- disease_measures %>%
  select(age, sex, sex_age_cat, disease, case_fatality) %>%
  mutate(disease=paste0("case_fatality_",tolower(disease))) %>%
  spread(key=disease,value=case_fatality)


disease_measures_incidence <- disease_measures %>%
  select(age, sex, sex_age_cat, disease, incidence) %>%
  mutate(disease=paste0("incidence_",tolower(disease))) %>%
  spread(key=disease,value=incidence)

disease_measures_prevalence <- disease_measures %>%
  select(age, sex, sex_age_cat, disease, prevalence) %>%
  mutate(disease=paste0("prevalence_",tolower(disease))) %>%
  spread(key=disease,value=prevalence)


disease_life_table_input <- disease_measures_age %>%
  left_join(disease_measures_casefatality) %>%
  left_join(disease_measures_incidence) %>%
  left_join(disease_measures_prevalence)

## Only keeping first 202 rows, the rest just repeat the same observations. (Include prevalence and incidence outcomes)

disease_life_table_input <- disease_life_table_input[1:202,]


## write here to inspect results and compare with dismod

write.csv(disease_life_table_input, "data/city regions/bristol/disease_input_data.csv")

# ---- chunk-5: Create mslt data frame ----

## All data requierements to be procecessed in the mslt_code
## STILL NEED TO INCLUDE TRENDS IN DISEASES
## GET POPULATION DATA FROM SYNTHETIC POPULATION

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

gbd_popn_df <- select(gbd_df, population_number, sex_age_cat)

## Synthetic population (TO DO)

synthetic_pop <- read_csv("data/population/pop_england_2017.csv")


mslt_df <- left_join(mslt_df, gbd_popn_df, by = "sex_age_cat")

# ---- chunk-6.1: Interpolate rates ----

# ---- chunk-2.5: Disability weights ---- CHANGE THIS FURTHER DOWN, DOES NOT MAKE MUCH SENSE HERE (BELONGS TO MSLT, NOT DISBAYES/DISMOD)

all_ylds_df <- dplyr::select(gbd_df, starts_with("ylds (years lived with disability)_number"))


## Adjust all cause ylds for included diseases and injuries (exclude all cause )

gbd_df[["allc_ylds_adj_rate_1"]] <- (gbd_df$`ylds (years lived with disability)_number_allc`  - rowSums(select(all_ylds_df, -`ylds (years lived with disability)_number_allc`))) / 
  gbd_df$population_number

# ------------------- DWs ---------------------------#

disease_short_names <- mutate_all(disease_short_names, funs(tolower))

for (d in 1:nrow(disease_short_names)){
  gbd_df[[paste0("dw_adj_", disease_short_names$sname[d])]] <- 
    (gbd_df[[paste0("ylds (years lived with disability)_number_", disease_short_names$sname[d])]] /
       gbd_df[[paste0("prevalence_number_", disease_short_names$sname[d])]]) /
    ( 1 - gbd_df[["allc_ylds_adj_rate_1"]])
}

gbd_df[mapply(is.infinite, gbd_df)] <- 0
gbd_df <- replace(gbd_df, is.na(gbd_df), 0)



## Data has to be interpolated from 5-year age groups to 1-year age groups.

## Create variable names.

for (d in 1:nrow(disease_short_names)){
  
  if (disease_short_names$is_not_dis[d] == 0){
  
  var_name <- paste0("dw_adj_", disease_short_names$sname[d])
  
  mslt_df[, var_name] <- 1
  
  }
}

## Interpolate dw rates 


for (d in 1:nrow(disease_short_names)){
  for(sex_index in i_sex) {
    for (var in c('dw_adj')){#, 'deaths_rate', 'ylds (years lived with disability)_rate')){
      
      if (disease_short_names$is_not_dis[d] == 0) {

        
        var_name <- paste0(var, '_', disease_short_names$sname[d])
        
        data <- dplyr::filter(gbd_df, sex == sex_index) %>% select(age, sex, age_cat, starts_with(var_name))
        

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
      }
    }
  }
}

## Interpolate all cause mortality and pylds and road injuries

### Create variable names

for (d in 1:nrow(disease_short_names)){
  
  if (disease_short_names$is_not_dis[d] != 0){
  
  var_name1 <- paste0("deaths_rate", "_", disease_short_names$sname[d])
  
  var_name2 <- paste0("ylds (years lived with disability)_rate", "_", disease_short_names$sname[d])
  
  mslt_df[, var_name1] <- 1
  mslt_df[, var_name2] <- 1
 
  } 
}

### Deaths

for (d in 1:nrow(disease_short_names)){
  for(sex_index in i_sex) {
    for (var in c('deaths_rate')) {
      if (disease_short_names$is_not_dis[d] != 0){
        
        var_name1 <- paste0(var, '_', disease_short_names$sname[d])
        
        data <- dplyr::filter(gbd_df, sex == sex_index) %>% select(age, sex, age_cat, starts_with(var_name1))

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

for (d in 1:nrow(disease_short_names)){
  for(sex_index in i_sex) {
    for (var in c("ylds (years lived with disability)_rate")){

      if (disease_short_names$is_not_dis[d] != 0){

        var_name2 <- paste0(var, '_', disease_short_names$sname[d])
        
        data <- dplyr::filter(gbd_df, sex == sex_index) %>% select(age, sex, age_cat, starts_with(var_name2))
        
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
        #}
      }
      
    }
  }
}

## Give names to mortality all cause and pYLD all cause to match general_life_table function.

names(mslt_df)[names(mslt_df) == "deaths_rate_allc"] <- "mx"
names(mslt_df)[names(mslt_df) == "ylds (years lived with disability)_rate_allc"] <- "pyld_rate"

# add incidence, case fatality and prevalence to mslt data frame 

disease_life_table_input_1 <- select(disease_life_table_input, -c(age, sex))

mslt_df <- left_join(mslt_df, disease_life_table_input_1, by = "sex_age_cat")

## ADJUST DISABILTIY WEIGHTS WITH DISBAYES GENERATED OUTCOMES

names(mslt_df)[names(mslt_df) == "deaths_rate_pdri"] <- "deaths_rate_pedestrian"
names(mslt_df)[names(mslt_df) == "ylds (years lived with disability)_rate_pdri"] <- "ylds_rate_pedestrian"

names(mslt_df)[names(mslt_df) == "deaths_rate_cyri"] <- "deaths_rate_cyclist"
names(mslt_df)[names(mslt_df) == "ylds (years lived with disability)_rate_cyri"] <- "ylds_rate_cyclist"

names(mslt_df)[names(mslt_df) == "deaths_rate_mtri"] <- "deaths_rate_motorcyclist"
names(mslt_df)[names(mslt_df) == "ylds (years lived with disability)_rate_mtri"] <- "ylds_rate_motorcyclist"

names(mslt_df)[names(mslt_df) == "deaths_rate_mvri"] <- "deaths_rate_motor"
names(mslt_df)[names(mslt_df) == "ylds (years lived with disability)_rate_mvri"] <- "ylds_rate_motor"


names(mslt_df)[names(mslt_df) == "deaths_rate_otri"] <- "deaths_rate_other"
names(mslt_df)[names(mslt_df) == "ylds (years lived with disability)_rate_otri"] <- "ylds_rate_other"

names(mslt_df)[names(mslt_df) == "deaths_rate_lwri"] <- "deaths_rate_lri"
names(mslt_df)[names(mslt_df) == "ylds (years lived with disability)_rate_lwri"] <- "ylds_rate_lri"

write_csv(mslt_df, ("data/mslt_df.csv"))

## Get PIFS from Rob adn expand from five year age groups to one. 

pif <- read_csv("data/pif.csv")

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

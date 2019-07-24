
## Create R Markdown for data preparation

# ---- chunk-intro ----

## City regions, prepare a dataset for each of them (THIS SHOULD LOOK UP IN ANNA'S LOOK UP TABLE, IN DIFFERENT REPOSITORY, HOW DO WE CONNECT?)

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
# Nottingham: Ashfield, Bassetlaw, Broxtowe, Gedling, Mansfield, Nottingham, Newark and Sherwood, Rushcliffe.
# 
# West Midlands Combined Authority: Birmingham, Coventry, Dudley, Sandwell, Solihull, Walsall, Wolverhampton.


# ---- chunk-1: Data preparation ----

## Source functions

source("code/functions.R")


## Define parameters

year <- 2017

year_trend <- 2007

i_age_cohort <- c(22, 27, 32, 37, 42, 47, 52, 57, 62, 67, 72, 77, 82, 87, 92, 97)

i_sex <- c("male", "female")

## Look up table to match local authority areas to city regions

look_up_table <- read_csv("C:/Users/Bele/Dropbox/Collaborations/James Woodcock/mh-execute/inputs/mh_regions_lad_lookup.csv")

city_regions <- data.frame(unique(look_up_table$cityregion))
city_regions <- city_regions[2:10,]

## Dataframe with local goverment areas within each city region

local_goverment_areas <-  look_up_table %>% filter(look_up_table$cityregion != "")


## Here we should have a loop to do all the processing in a loop for each of the localities (CHECK WITH ALI)
## There will be one input data frame for processing in mslt_code for each of the city regions.


## BELOW IS NOT CONNECTED TO ABOVE CODE
## The localities should be within the city region defined in the loop

localities <- c('Bristol, City of', 'Bath and North East Somerset', 'North Somerset', 'South Gloucestershire')

# ---- chunk-1.1: Get Global Buden of Disease data ----

## GBD MISSING DATA FOR NOTTINGHAM: Ashfield, Bassetlaw, Broxtowe, Gedling, Mansfield, Newark and Sherwood, Rushcliffe and City of London. 

## Get data from GBD dowloaded data for England (all localities)
## Use code developed by Marko Tainio to extract zip files
## Created in February-March 2019 by Marko Tainio (modified by Belen Zapata June 2019 for Metahit project)
## This script extracts required Global Burden of Disease data from the zip files dowloaded from http://ghdx.healthdata.org/gbd-results-tool
## by first extracting zip-files, then reading csv file, adding required data to combined dataframe
## and finally deleiting extracted files. Resulting dataframe is then saved as csv-file.

## Defining folder where the data is stored (stored externally in my dropbox as the GBD files are large)
## CHANGE TO v-DRIVE

data_folder <- "C:/Users/Bele/Dropbox/Collaborations/James Woodcock/Metahit/Data/GBD2017"
temp_folder <- paste0(data_folder,"/temp") 
result_folder <- paste0(data_folder,"/final")
gbdfile_name <- "/IHME-GBD_2017_DATA-0a504496-" # CHANGE NAME WHEN NEW DATA IS DOWNLOADED

## Next two lines defines locations that will be extracted. 
LGAs <- unlist(read_csv("data/gbd/LGAs to be extracted.csv")[,2]) # CREATE FILE FOR YOUR LOCATIONS OF INTEREST, HERE LOCALITIES IN CITY OF BRISTOL REGION

data_extracted <- NULL

for (i in 1:40) { # LOOP NUMBER DEPENDS ON NUMBER OF ZIP FILES, HERE I JUST GOT DATA FOR ALL LOCALITIES IN ENGLAND
  file_number <- i
  
  file_select <- paste0(data_folder,gbdfile_name, i,".zip")
  
  unzip(file_select, exdir=temp_folder)
  
  data_read <- read_csv((paste0(temp_folder,"/", gbdfile_name, i, ".csv")))
  file.remove(paste0(temp_folder,"/", gbdfile_name, i, ".csv"))
  data_read <- subset(data_read, location_name %in% LGAs) # location name is easier to identify
  
  data_extracted <- rbind(data_extracted,data_read)
}

unlink(paste0(temp_folder), recursive = TRUE)


# ---- chunk-1.2: Define parameters from data ----

## Define measure (e.g. deaths) and cause parameters (e.g. all causes, breast cancer) (this is to avoid hard coding the parameters)

## Min Length is not changing anything, how can we make it characters in the first place, rather than having to ocnvert below before running RunLocDF?

disease_short_names <- data.frame(disease = as.character(unique(data_extracted$cause_name)), 
                                  sname = tolower(abbreviate(unique(data_extracted$cause_name, max = 2))),
                                  stringsAsFactors = F)

disease_short_names <- disease_short_names %>% mutate(is_not_dis = ifelse((str_detect(disease, "injuries") |
                                                                             str_detect(disease, "All causes") |
                                                                             str_detect(disease, "Lower respiratory infections")), 
                                                                          1, 0) )

disease_short_names[disease_short_names$sname == "allc", "is_not_dis"] <- 2


disease_short_names <- disease_short_names %>% mutate_if(is.factor, as.character)

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

gbd_input <- filter(gbd_input, location %in% localities) %>% mutate_if(is.factor, as.character)

# ---- chunk-2.2: Sort data per local goverment area ----

## We first derive populaiton and cases numbers (e.g. all cause mortality) for each locality and then aggregate at the City Region level. 
## Loop to create a raw data set for 2017 for each of the localities to then process to get requiered data for disbayes/dismod. 

gbd_data_localities_raw <- list()
index <- 1

for (l in localities){
  for (y in year){
    
    gbd_data_localities_raw[[index]] <- SortGbdInput(in_data = gbd_input, in_year = y, in_locality = l)
    
    index <- index + 1
  }
}

## Prepare data set per locality to calculate population numbers (I used lapply, may change for loop)

gbd_loc_data_processed <- lapply(gbd_data_localities_raw, RunLocDf)

# ---- chunk-2.3: Create data frame for city region ----

## Add up all localities in a data frame for City Region: population, causes-measures rates and numbers. 

## Add up number and then calculate rates from numbers and population numbers. 

gbd_all_loc <- bind_rows(gbd_loc_data_processed, .id = 'number')

## Delete columns with rates (we recalculate them at the city region level)

gbd_all_loc <-  select(gbd_all_loc,-contains("rate"))

gbd_all_loc <- gbd_all_loc %>% mutate_if(is.factor, as.character)

## Create data frame adding up all values for city region.

## Add sex age category for matching when adding up values (unique age_sex per locality)

gbd_all_loc$sex_age_cat <- paste(gbd_all_loc$sex, gbd_all_loc$age, sep = "_")
gbd_all_loc <- select(gbd_all_loc, -c(age, sex, location, number))

## Create aggregated data frame (sums all numbers from localities within a city region)

gbd_city_region <- gbd_all_loc %>%
  group_by(sex_age_cat) %>%
  summarise_all(funs(sum))

## Create two new columns for age and seX

gbd_city_region_2017 <- gbd_city_region %>%
  separate(sex_age_cat, c("sex", "age"), "_")

# ---- chunk-2.4: Create data frame for dismod and disbayes processing ----

gbd_df <- gbd_city_region_2017

## Add numberical age categories

gbd_df$age_cat <- 0
gbd_df$age_cat [gbd_df$age =="Under 5"] <- 2
gbd_df$age_cat [gbd_df$age =="5 to 9"] <- 7
gbd_df$age_cat [gbd_df$age =="10 to 14"] <- 12
gbd_df$age_cat [gbd_df$age =="15 to 19"] <- 17
gbd_df$age_cat [gbd_df$age =="20 to 24"] <- 22
gbd_df$age_cat [gbd_df$age =="25 to 29"] <- 27
gbd_df$age_cat [gbd_df$age =="30 to 34"] <- 32
gbd_df$age_cat [gbd_df$age =="35 to 39"] <- 37
gbd_df$age_cat [gbd_df$age =="40 to 44"] <- 42
gbd_df$age_cat [gbd_df$age =="45 to 49"] <- 47
gbd_df$age_cat [gbd_df$age =="50 to 54"] <- 52
gbd_df$age_cat [gbd_df$age =="55 to 59"] <- 57
gbd_df$age_cat [gbd_df$age =="60 to 64"] <- 62
gbd_df$age_cat [gbd_df$age =="65 to 69"] <- 67
gbd_df$age_cat [gbd_df$age =="70 to 74"] <- 72
gbd_df$age_cat [gbd_df$age =="75 to 79"] <- 77
gbd_df$age_cat [gbd_df$age =="80 to 84"] <- 82
gbd_df$age_cat [gbd_df$age =="85 to 89"] <- 87
gbd_df$age_cat [gbd_df$age =="90 to 94"] <- 92
gbd_df$age_cat [gbd_df$age =="95 plus"] <- 97


## change sex to lower case

gbd_df$sex <- tolower(gbd_df$sex)

## Create age_sex category, NOT SURE WHERE THIS IS BEING USED

gbd_df$sex_age_cat <- paste(gbd_df$sex,gbd_df$age_cat, sep = "_"  )

## Order data

gbd_df <- gbd_df[order(gbd_df$sex, gbd_df$age_cat),]

## Calculate rates per one, base on aggregated numbers from localities

for (dm in 1:length(disease_measures_list)){
  for (d in 1:nrow(disease_short_names)){
    dn <- disease_short_names$disease[d]
    dmeasure <- disease_measures_list[dm] %>% as.character()

gbd_df[[tolower(paste(dmeasure, "rate", disease_short_names$sname[d], sep = "_"))]] <- gbd_df[[tolower(paste(dmeasure, "number", disease_short_names$sname[d], sep = "_"))]]/gbd_df$population_number

  }
}

## Write csv file to process in Dismod

write_csv(gbd_df, "data/city regions/bristol/dismod/input_data.csv")


# ---- chunk-2.5: Disability weights ----

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


# ---- chunk-3: Disbayes ----

View(gbd_df)

## Disbayes data preparation

library(devtools) 

## The code below generates age, sex and disease specific data frames to process with disbayes. 
## Chris Jackson generated the code for one dataset and I added a loop to do all diseases by age an sex. 

disbayes_input_list <- list()
index <- 1

for (d in 1:nrow(disease_short_names)){
  for (sex_index in i_sex){
    
    if (disease_short_names$is_not_dis[d] == 0){
    
    var_name <- paste0("rate_", disease_short_names$sname[d])

    disbayes_input_list[[index]] <- filter(gbd_df, sex == sex_index) %>% select(age, sex, ends_with(var_name), population_number)
  
    ## Add column to show disease
    
    disbayes_input_list[[index]]$disease <- disease_short_names$sname[d]

    ## Change column names to match disbayes code
    
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

# View(disbayes_input_list[[1]])

## Loop to save each data frame within disbayes_list (to check data inputs, but disbayes is run with list above)

index <- 1 

for (d in 1:nrow(disease_short_names)){
    for (sex_index in i_sex){
      
      ## replace with function for disease/injuries that we do not need to model
      
      if (disease_short_names$is_not_dis[d] == 0){
      
      ##Save to csv
      saveRDS(disbayes_input_list[[index]], paste0("data/city regions/bristol/dismod/input/", disease_short_names$sname[d], "_", sex_index, ".rds"))
      
      index <- index +1
      
     }
  }
}

## Run Disbayes

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

disbayes_output_list <- list()
index <- 1

for (d in 1:nrow(disease_short_names)){
  for (sex_index in i_sex){
    
    data <- disbayes_input_list[[index]]
      
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

## Create list with data needs for multistate life table processing (case fatality and incidence)

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

## Data has to be interpolated from 5-year age groups to 1-year age groups.

## Create variable names.

for (d in 1:nrow(disease_short_names)){
  
  if (disease_short_names$is_not_dis[d] == 0){
  
  var_name <- paste0("dw_adj_", disease_short_names$sname[d])
  
  mslt_df[, var_name] <- 1
  
  }
}

## Interpolate dw rates (NOT LOOPING THROUGH DISEASES)


for (d in 1:nrow(disease_short_names)){
  for(sex_index in i_sex) {
    for (var in c('dw_adj')){#, 'deaths_rate', 'ylds (years lived with disability)_rate')){
      
      if (disease_short_names$is_not_dis[d] == 0) {

        
        var_name <- paste0(var, '_', disease_short_names$sname[d])
        
        data <- filter(gbd_df, sex == sex_index) %>% select(age, sex, age_cat, starts_with(var_name))
        
        # browser()
        
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
        
        data <- filter(gbd_df, sex == sex_index) %>% select(age, sex, age_cat, starts_with(var_name1))

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
        
        data <- filter(gbd_df, sex == sex_index) %>% select(age, sex, age_cat, starts_with(var_name2))
        
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



write_csv(mslt_df, ("data/mslt_df.csv"))




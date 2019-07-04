
## Create R Markdown for data preparation
## ADD ROAD TRAUMA DATA (MORTALITY AND YLD RATES TO MASTER DATAFRAME)
## ADD LRI MORTALITY AND YLDS RATES TO MASTER DATAFRAME


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


## ALI TO HELP WITH BEST APPROACH TO THIS. 
## Define parameters (start with Bristol). THIS SHOULD BE MATCH WITH https://github.com/metahit/mh-execute/blob/master/inputs/mh_regions_lad_lookup.csv

## Look up table for city regions 

look_up_table <- read.csv("C:/Users/Bele/Dropbox/Collaborations/James Woodcock/mh-execute/inputs/mh_regions_lad_lookup.csv")

city_regions <- data.frame(unique(look_up_table$cityregion))
city_regions <- city_regions[2:10,]

## Dataframe with local goverment areas within each city region

local_goverment_areas <-  look_up_table %>% filter(look_up_table$cityregion != "")


## Here we should have a loop to do all the processing in a loop for each of the localities (CHECK WITH ALI)

city_region <- bristol

## BELOW IS NOT CONNECTED TO ABOVE CODE

localities <- c('Bristol, City of', 'Bath and North East Somerset', 'North Somerset', 'South Gloucestershire')

year <- 2017

i_age_cohort <- c(22, 27, 32, 37, 42, 47, 52, 57, 62, 67, 72, 77, 82, 87, 92, 97)

i_sex <- c("male", "female")

## Source functions

getwd()

source("code/functions.R")

# ---- chunk-1 ----

## GBD MISSING DATA FOR NOTTINGHAM: Ashfield, Bassetlaw, Broxtowe, Gedling, Mansfield, Newark and Sherwood, Rushcliffe and City of London. 

## Get data from GBD dowloaded data for England (all localities)
## Use code developed by Marko Tainio to extract zip files
## Created in February-March 2019 by Marko Tainio (modified by Belen Zapata June 2019 for Metahit project)
## This script extracts required Global Burden of Disease data from the zip files dowloaded from http://ghdx.healthdata.org/gbd-results-tool
## by first extracting zip-files, then reading csv file, adding required data to combined dataframe
## and finally deleiting extracted files. Resulting dataframe is then saved as csv-file.

## Defining folder where the data is stored (stored externally in my dropbox as the GBD files are large)

data_folder <- "C:/Users/Bele/Dropbox/Collaborations/James Woodcock/Metahit/Data/GBD2017"
temp_folder <- paste0(data_folder,"/temp") 
result_folder <- paste0(data_folder,"/final")
gbdfile_name <- "/IHME-GBD_2017_DATA-0a504496-" # CHANGE NAME WHEN NEW DATA IS DOWNLOADED

## Next two lines defines locations that will be extracted. 
LGAs <- unlist(read.csv("data/gbd/LGAs to be extracted.csv")[,2]) # CREATE FILE FOR YOUR LOCATIONS OF INTEREST, HERE LOCALITIES IN CITY OF BRISTOL REGION

data_extracted <- NULL

for (i in 1:40) { # LOOP NUMBER DEPENDS ON NUMBER OF ZIP FILES, HERE I JUST GOT DATA FOR ALL LOCALITIES IN ENGLAND
  file_number <- i
  
  file_select <- paste0(data_folder,gbdfile_name, i,".zip")
  
  unzip(file_select, exdir=temp_folder)
  
  data_read <- read.csv((paste0(temp_folder,"/", gbdfile_name, i, ".csv")))
  file.remove(paste0(temp_folder,"/", gbdfile_name, i, ".csv"))
  data_read <- subset(data_read, location_name %in% LGAs) # location name is easier to identify
  
  data_extracted <- rbind(data_extracted,data_read)
}

unlink(paste0(temp_folder), recursive = TRUE)



# ---- Chunk 2 ----

## Define measure (e.g. deaths) and cause parameters (e.g. all causes, breast cancer) (this is to avoid hard coding the parameters)

## Min Length is not changing anything, how can we make it characters in the first place, rather than having to ocnvert below before running RunLocDF?

disease_short_names <- data.frame(disease = unique(data_extracted$cause_name), sname = abbreviate(unique(data_extracted$cause_name, minlength = 2)))

# disease_short_names <- mutate_all(disease_short_names, funs(tolower))

disease_short_names <- disease_short_names %>% mutate_if(is.factor, as.character)

disease_measures_list <- data.frame(measure = unique(data_extracted$measure_name)) %>%
  pull(measure) %>%
  as.character() %>%
  as.list()
  
## Change all observations to lowercase

# ---- chunk-3 ----

## Data preparation for Dismod/Disbayes

### Get file dataframe sorted (start with GBD dowloaded data)

gbd_input <- data_extracted

### Rename columns to drop _name and drop _id columns

names(gbd_input) = gsub(pattern = "_name", replacement = "", x = names(gbd_input))

gbd_input <- select(gbd_input,-contains("id"))

### Only keep rows for Local Goverment Area of Interest and change factors to characters, otherwise matching parameters does not work (e.g. localities is a character)

gbd_input <- filter(gbd_input, location %in% localities) %>% mutate_if(is.factor, as.character)

### We first derive populaiton and cases numbers (e.g. all cause mortality) for each locality and then aggregate at the City Region level. 
### Loop to create a raw data set for 2017 for each of the localities to then process to get requiered data for disbayes/dismod. 

gbd_data_localities_raw <- list()
index <- 1

for (l in localities){
  for (y in year){
    
    gbd_data_localities_raw[[index]] <- SortGbdInput(in_data = gbd_input, in_year = y, in_locality = l)
    
    index <- index + 1
  }
}

## Save file as example for Ali

write.csv(gbd_data_localities_raw[[1]], "data/city regions/example_RunLocDF.csv" )

## Uncomment to check selection
# View(gbd_data_localities_raw[[1]])

### Prepare data set per locality to calculate population numbers (I used lapply, may change for loop)

gbd_loc_data_processed <- lapply(gbd_data_localities_raw, RunLocDf)

## Ali, here I am running the code for one locality with the example data, and it does not calculate population for younger ages

test_gbd_processed <- RunLocDf(read.csv("data/city regions/example_RunLocDF.csv"))


## Uncomment to check selection.

## NO POPULATION NUMBERS FOR UNDER 15, CAUSES PROBLEMS WITH DISMOD CALCULATIONS. 
# View(gbd_loc_data_processed[[4]])

## Add up all localities in a data frame for Bristol City Region: population, causes-measures rates and numbers. 

### add up number and then calculate rates from numbers and population numbers. 

gbd_Bristol_all_loc <- bind_rows(gbd_loc_data_processed, .id = 'number')

### Delete columns with rates (we recalculate them at the city region level)

gbd_Bristol_all_loc <-  select(gbd_Bristol_all_loc,-contains("rate"))

gbd_Bristol_all_loc <- gbd_Bristol_all_loc %>% mutate_if(is.factor, as.character)

### Create data frame adding up all values for Bristol

## Add sex age category for matching when adding up values (unique age_sex per locality)
gbd_Bristol_all_loc$sex_age_cat <- paste(gbd_Bristol_all_loc$sex, gbd_Bristol_all_loc$age, sep = "_")
gbd_Bristol_all_loc <- select(gbd_Bristol_all_loc, -c(age, sex, location, number))

## Create aggregated data frame (HAS A WARNING)

gbd_Bristol <- gbd_Bristol_all_loc %>%
  group_by(sex_age_cat) %>%
  summarise_all(funs(sum))

## Create two new columns for age and seX

gbd_Bristol_2017 <- gbd_Bristol %>%
  separate(sex_age_cat, c("sex", "age"), "_")


# names(gbd_Bristol_2017)

# ------------------- Dismod/Disbayes input data set ---------------------------#

gbd_df <- gbd_Bristol_2017


# ------------------- Add age-groups --------------------# 
# 22, 27, 32, 37, 42, 47, 52, 57, 62, 67, 72, 77, 82, 87, 92, 97

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


### change sex to lower case

gbd_df$sex <- tolower(gbd_df$sex)

gbd_df$sex_age_cat <- paste(gbd_df$sex,gbd_df$age_cat, sep = "_"  )
# # View(gbd_df)
# names(gbd_df)

# ------------------- Sort frame --------------------# 

gbd_df <- gbd_df[order(gbd_df$sex, gbd_df$age_cat),]

# ------------------- calculate rates per one--------------------# 

for (dm in 1:length(disease_measures_list)){
  for (d in 1:nrow(disease_short_names)){
    dn <- disease_short_names$disease[d]
    dmeasure <- disease_measures_list[dm] %>% as.character()

    ## If else will depend on final diseases
    # # Exclude hdd and incidence and deaths and mdd (not included here)
    # if((dm == "incidence" && dn == "hhd") || (dm == "deaths" && dn == "mdd" ) ){
    #   # cat("\n") # Uncomment to see list
    # }
    # else {


gbd_df[[tolower(paste(dmeasure, "rate", disease_short_names$sname[d], sep = "_"))]] <- gbd_df[[tolower(paste(dmeasure, "number", disease_short_names$sname[d], sep = "_"))]]/gbd_df$population_number

  }
}

## Uncomment to review
# names(gbd_df)
# View(gbd_df)

# ------ Write csv file to process in Dismod/Disbayes-------- # TO PROCESS

write_csv(gbd_df, "data/city regions/bristol/dismod/input_data.csv")



# ---- chunk-4 ----

## Disbayes data prep

library(devtools)

## The code below generates age, sex and disease specific data frames to process with disbayes. 
## Chris Jackson generated the code for one dataset and I added a loop to do all diseases by age an sex. 

in_data <- read.csv("data/city regions/bristol/dismod/input_data.csv")

## Check names to see that all data is available for calculations


disbayes_input_list <- list()
index <- 1

for (d in 1:nrow(disease_short_names)){
  for (sex_index in i_sex){
    
    ## this is not excluding all causes and road injuries
    
    if (disease_short_names$disease[d] == "All causes" && disease_short_names$disease[d] == "Road injuries"){
      # cat("\n") #Uncomment to see list
    }
    # 
    # if (disease_short_names$disease[d] == "Road injuries"){
    
    else {
    
    var_name <- paste0("rate_", disease_short_names$sname[d])

    disbayes_input_list[[index]] <- filter(in_data, sex == sex_index) %>% select(age, sex, ends_with(var_name), population_number)
    
    ## Add column to show disease
    
    disbayes_input_list[[index]]$disease <- disease_short_names$sname[d]

    
    ## Change column names to match disbayes code
    
    colnames(disbayes_input_list[[index]])[colnames(disbayes_input_list[[index]])== tolower(paste0("incidence_rate_", disease_short_names$sname[d]))] <- "inc"
    colnames(disbayes_input_list[[index]])[colnames(disbayes_input_list[[index]])== tolower(paste0("deaths_rate_", disease_short_names$sname[d]))] <- "mort"
    colnames(disbayes_input_list[[index]])[colnames(disbayes_input_list[[index]])== tolower(paste0("prevalence_rate_", disease_short_names$sname[d]))] <- "prev"
    colnames(disbayes_input_list[[index]])[colnames(disbayes_input_list[[index]])== paste0("population_number")] <- "pop"
    
    ## We assume remission is 0

    disbayes_input_list[[index]]$rem <- 0

    ## create denominator for disbayes code

    disbayes_input_list[[index]]$prevdenom <- c(100,100,500,500,500,500,500,500,500,500,500,500,500,500,500,500,200,200,100,100) / 10 # total sample size 3910, generous for London (from CJ)

    ## Added agegroups to derive age groups by 1

    disbayes_input_list[[index]]$agegrp <- as.integer(seq(0,95, by=5))

    ## Replace 0 with small numbers for incidence, otherwise, disbayes does not work.

    disbayes_input_list[[index]]$inc <- ifelse(disbayes_input_list[[index]]$inc  == 0, 1e-08, disbayes_input_list[[index]]$inc)


    ## Convert 5 year data file to 100 year age intervals


    outage <- 0:100  # assume inc/prev/mort same in each year within a five-year age group
    ind <- findInterval(outage, disbayes_input_list[[index]]$agegr)
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

# ## Uncoment to check 
# View(disbayes_input_list[[1]])


## Loop to save each data frame within disbayes_list (to check data inputs, but disbayes is run with list above)

index <- 1 

for (d in 1:nrow(disease_short_names)){
    for (sex_index in i_sex){
      
      ##Save to csv
      write_csv(disbayes_input_list[[index]], paste0("data/city regions/bristol/dismod/input/", disease_short_names$sname[d], "_", sex_index, ".csv"))
      
      index <- index +1
    }
}

## Run Disbayes

## We do not need the age and sex loop if working from list
## ALL CAUSE SHOULD BE EXCLUDED
## ADD disease names column, otherwise, no info on which diseases

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


disbayes_output_list <- list()
index <- 1

for (d in 1:nrow(disease_short_names)){
  for (sex_index in i_sex){
    
    # data <- disbayes_input_list[[index]]
    
        if (disease_short_names$disease[d] == "All causes"){
      # cat("\n") #Uncomment to see list
    }
    if (disease_short_names$disease[d] == "Road injuries"){
    }
    else {
      
      data <- data.frame(read.csv(paste0("data/city regions/bristol/dismod/input/", disease_short_names$sname[d], "_", sex_index, ".csv")))
    
    # Exclude all cause and road injuries
    


    ## The saved data for input may be deleted or not, perhaps useful to check errors. 
    
    # data.frame(read.csv(paste0("MSLT/data/city regions/bristol/dismod/input/", d_index, "_", sex_index, ".csv")))
    ## Why these priors? (inits?)
    ## Optiomization procresses from Dismod, can we incoporporate
    
    datstan <- c(as.list(data), nage=nrow(data))
    inits <- list(
      list(cf=rep(0.0101, datstan$nage)),
      list(cf=rep(0.0201, datstan$nage)),
      list(cf=rep(0.0056, datstan$nage)),
      list(cf=rep(0.0071, datstan$nage))
    )
    gbdcf <- stan("disbayes-master/gbdcf-unsmoothed.stan", data=datstan, init=inits)
    
    ## Extract Summary statistics
    

    ## Add directly do dibayes input list, first 100 observations?
    disbayes_output_list[[index]] <- as.data.frame(summary(gbdcf)$summary)[1:101, 1:3]
    
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

View(disbayes_output_list[[14]])

## List of complete data for disbayes, it includes incidence from input calculations based on gbd and case fatality, estimated with disbayes
## The above disbayes_output code is ommiting some diseases, may be convergence issues, to check with Chris. 
disease_lifetable_inputs_list <-  list()

for (i in 1:length(disbayes_output_list)){
   for (d in 1:nrow(disease_short_names)){
  
  ## Create list same lenght as outputs   
   disease_lifetable_inputs_list[[i]] <- disbayes_input_list[[i]]
  ## Add column names for incidence and case fatality disease
   disease_lifetable_inputs_list[[i]]$case_fatality  <- disbayes_output_list[[i]]$mean
   disease_lifetable_inputs_list[[i]]$incidence <- disbayes_input_list[[i]]$inc
  ## Add age and sex column to then create unique dataset with all diseases case fatality and incidence
   
   disease_lifetable_inputs_list[[i]][[paste("sex_age_cat", sep = "")]] <- paste(disease_lifetable_inputs_list[[i]]$sex,disease_lifetable_inputs_list[[i]]$age, sep = "_"  )

   disease_lifetable_inputs_list[[i]]$disease_sex_match <- disbayes_output_list[[i]]$sex_disease
   
 }

}

# View(disease_lifetable_inputs_list[[1]])

## Create a data frame with all diseases case fatality and incidence to process in mslt_code disease code

diseaseMeasures <- NULL
colNames <- colnames(disease_lifetable_inputs_list[[1]])
for (j in 1:length(disease_lifetable_inputs_list)){
  currentTable <- disease_lifetable_inputs_list[[j]]
  colnames(currentTable) <- colNames
  diseaseMeasures <- rbind(diseaseMeasures,
                           currentTable)
}

diseaseMeasuresAge <- diseaseMeasures %>%
  select(age,sex,sex_age_cat)

diseaseMeasuresCaseFatality <- diseaseMeasures %>%
#  select(age_sex_cat, disease, case_fatality=case_fatality_carc, incidence=incidence_carc) %>%
  select(sex_age_cat, disease, case_fatality) %>%
  mutate(disease=paste0("case_fatality_",tolower(disease))) %>%
  spread(key=disease,value=case_fatality)


diseaseMeasuresIncidence <- diseaseMeasures %>%
  #  select(age_sex_cat, disease, case_fatality=case_fatality_carc, incidence=incidence_carc) %>%
  select(sex_age_cat, disease, incidence) %>%
  mutate(disease=paste0("incidence_",tolower(disease))) %>%
  spread(key=disease,value=incidence)


disease_life_table_input <- diseaseMeasuresAge %>%
  left_join(diseaseMeasuresCaseFatality) %>%
  left_join(diseaseMeasuresIncidence)

## Only keeping first 202 rows, the rest just repeat the same observations. 

disease_life_table_input <- disease_life_table_input[1:202,]


View(disease_life_table_input)


write.csv(disease_life_table_input, "data/city regions/bristol/disease_input_data.csv")

disbayes_output_list[[13]]$sex_disease
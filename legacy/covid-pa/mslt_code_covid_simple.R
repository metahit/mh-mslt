rm (list = ls())
library(ggplot2)
library(dbplyr)
library(readr)
library(dplyr)
library(tidyr)



### Simple life table analysis for global covid related PA interventions. 
### No discouting applied to future health gains

### SET UP

setwd("C:/Metahit/")

## Relative paths

relative_path_mslt <- paste0(getwd(),'/mh-mslt/')

## get functions

source(paste0(relative_path_mslt,'covid-pa/functions.R'))

## DATA PREPARATION

data <-  read_csv(paste0(relative_path_mslt,"/covid-pa/argentina_gbd.csv"))

## get parameters from data to run RunDF

DISEASE_SHORT_NAMES <- data.frame(disease = (as.character(unique(data$cause_name))), 
                                  sname = (abbreviate(unique(data$cause_name, max = 2))),
                                  stringsAsFactors = F)


disease_measures_list <- data.frame(measure = unique(data$measure_name)) %>%
  pull(measure) %>%
  as.character() %>%
  as.list()

names(data) = gsub(pattern = "_name", replacement = "", x = names(data))

## sort data for life table 

gbd_data <- RunLocDf(data)

## generate data life table

LT_DF <- GenLTDF(gbd_data)

### TO DO: mortality trends

### MODEL COHORT PARAMETERS

i_age_cohort <- c(17, 22, 27, 32, 37, 42, 47, 52, 57, 62, 67, 72, 77, 82, 87, 92, 97)

i_sex <- c('male', 'female')

### MODEL INTERVENTION ASSUMPTIONS (TO DO)

sc_duration <- replicate(4,1) %>% append(replicate(80, 0))


### LIFE TABLE MODELS

## Create baseline life tables

general_life_table_list_bl <- list()

index <- 1

for (iage in i_age_cohort){
  for (isex in i_sex){
    # cat('age ', age, ' and sex ', sex, '\n') #Uncomment to see index
    suppressWarnings(general_life_table_list_bl[[index]] <- RunLifeTable(in_idata = LT_DF,
                                                                         in_sex = isex, in_mid_age = iage))
    
    names(general_life_table_list_bl)[index] <- paste(iage, isex, sep = '_')
    index <- index + 1
  }
}

## Create scenarios life tables

general_life_table_list_sc <- list()
index <- 1

for (iage in i_age_cohort){
  for (isex in i_sex){
    

    td2 <- LT_DF
    # td2 <- subset(td2, select = -c(mx, pyld_rate))
    td2[td2$age >= iage & td2$sex == isex,][[paste('mx')]] <- general_life_table_list_bl[[index]]$mx * (1-0.05) ### Here for now I assume a 5% reduction on mortality rates
    
    # Instead of idata, feed td to run scenarios
    general_life_table_list_sc[[index]] <- RunLifeTable(in_idata = td2, in_sex = isex, in_mid_age = iage)
    #
    names(general_life_table_list_sc)[index] <- paste(iage, isex, sep = '_')
    
    
    index <- index + 1
  }
}

### OUTPUTS

#### Create data frame with all outputs by age and sex cohort: life years and life expectancy. 
#### TO DO: ADD deaths


output_burden <- list()
l_index <- 1


l_sc_cols <- which(colnames(general_life_table_list_sc[[l_index]])%in%c('Lx', 'ex', 'dx'))
l_bl_cols <- which(colnames(general_life_table_list_bl[[l_index]])%in%c('Lx', 'ex', 'dx'))

for (iage in i_age_cohort){
  for (isex in i_sex){

     ## general_life_table_list_sc and general_life_table_list_bl (Lx)
    
    output_burden_lf_sc <- general_life_table_list_sc[[l_index]][,l_sc_cols]
    
    names(output_burden_lf_sc)[names(output_burden_lf_sc) == 'Lx'] <- paste('Lx', 'sc', sep = '_')
    names(output_burden_lf_sc)[names(output_burden_lf_sc) == 'ex'] <- paste('ex', 'sc', sep = '_')
    names(output_burden_lf_sc)[names(output_burden_lf_sc) == 'dx'] <- paste('dx', 'sc', sep = '_')  
    
    output_burden_lf_bl <- general_life_table_list_bl[[l_index]][,l_bl_cols]
    
    names(output_burden_lf_bl)[names(output_burden_lf_bl) == 'Lx'] <- paste('Lx', 'bl', sep = '_')  
    names(output_burden_lf_bl)[names(output_burden_lf_bl) == 'ex'] <- paste('ex', 'bl', sep = '_')
    names(output_burden_lf_bl)[names(output_burden_lf_bl) == 'dx'] <- paste('dx', 'bl', sep = '_')
    
    
    ### Difference in life years for each year of the cohort
    output_burden_lf_sc$Lx_diff <- general_life_table_list_sc[[l_index]]$Lx - general_life_table_list_bl[[l_index]]$Lx
    
    
    ### Difference in life expectancy, we only report for first year
    output_burden_lf_sc$ex_diff <- general_life_table_list_sc[[l_index]]$ex - general_life_table_list_bl[[l_index]]$ex
    
    ### Difference in deaths for each year of the cohort
    output_burden_lf_sc$dx_diff <- general_life_table_list_sc[[l_index]]$dx - general_life_table_list_bl[[l_index]]$dx
    
    
    output_burden_sc <- cbind(output_burden_lf_bl, output_burden_lf_sc)

    
    
    output_burden[[l_index]] <- output_burden_sc
    
    names(output_burden)[l_index] <- paste(iage, isex, sep = '_')
    
    
    l_index <- l_index + 1
    
  }
}


## Generate a data frame for all results

output_df <- plyr::ldply(output_burden, rbind)


### Create some tables

output_df <-  output_df  %>%
  separate(.id, c("Age group", "Gender"), "_", remove = FALSE) 
output_df$`Age group` <- as.numeric(output_df$`Age group`)

output_df$`Age group`[output_df$`Age group` == 17] <-"16-19"
output_df$`Age group`[output_df$`Age group` == 22] <-"20-24"
output_df$`Age group`[output_df$`Age group` == 27] <-"25-29"
output_df$`Age group`[output_df$`Age group` == 32] <-"30-34"
output_df$`Age group`[output_df$`Age group` == 37] <-"35-39"
output_df$`Age group`[output_df$`Age group` == 42] <-"40-44"
output_df$`Age group`[output_df$`Age group` == 47] <-"45-49"
output_df$`Age group`[output_df$`Age group` == 52] <-"50-54"
output_df$`Age group`[output_df$`Age group` == 57] <-"55-59"
output_df$`Age group`[output_df$`Age group` == 62] <-"60-64"
output_df$`Age group`[output_df$`Age group` == 67] <-"65-69"
output_df$`Age group`[output_df$`Age group` == 72] <-"70-74"
output_df$`Age group`[output_df$`Age group` == 77] <-"75-79"
output_df$`Age group`[output_df$`Age group` == 82] <-"80-84"
output_df$`Age group`[output_df$`Age group` == 87] <-"85-89"
output_df$`Age group`[output_df$`Age group` == 92] <-"90-94"
output_df$`Age group`[output_df$`Age group` == 97] <-"95 plus"

### Life expectancy 
### Life expectancy is for year one of simulation. For example, change in life expectancy for a female aged 16-19.

output_life_expectancy_change <- output_df[!duplicated(output_df$.id), c("Age group", "Gender", "ex_bl", "ex_sc", 
                                                                         "ex_diff")] %>%
  dplyr::rename(`Life expectancy at baseline` = ex_bl, 
                `Life expectancy scenario` = ex_sc, 
                `Difference in life expectancy` = ex_diff) %>% 
          mutate_if(is.numeric, round, digits = 3)


output_life_expectancy_change <- output_life_expectancy_change[order(output_life_expectancy_change$Gender),]


### Life years
#### Accumulated over the life of the cohort. For example, total life years change for females 16-19 over their life course

output_life_years_change <- output_df %>% 
  group_by(Gender, `Age group`) %>%
  summarise_if(is.numeric, funs(sum)) %>%
  dplyr::select(`Age group`, Gender, Lx_diff) %>%
  dplyr::rename(`Life years` = Lx_diff)  %>% 
  mutate_if(is.numeric, round)

### Deaths

output_deaths_change <- output_df %>% 
  group_by(Gender, `Age group`) %>%
  summarise_if(is.numeric, funs(sum)) %>%
  dplyr::select(`Age group`, Gender, dx_diff) %>%
 dplyr::rename(`Deaths` = dx_diff) 
# %>% 
#   mutate_if(is.numeric, round)

    
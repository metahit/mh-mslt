

# ---- chunk-intro ----

require(dplyr)
require(tidyverse)
require(knitr)
require(kableExtra)
require(citr)
require(gridExtra)
require(ggpubr)
require(grid)
require(ggplot2)
require(pillar)
require(devtools)
require(janitor)


# rm (list = ls())
options(scipen=999)

# ---- chunk-1 ----

## Functions

source("code/functions.R")

## Data 

## move name changes here

relative_path <- '../mh-mslt/'
pif_expanded <- read_csv(paste0(relative_path, "data/pif_expanded.csv"))
MSLT_DF <- read_csv(paste0(relative_path, "data/mslt_df.csv"))
DISEASE_SHORT_NAMES <<- read_csv(paste0(relative_path, "data/parameters/disease_names.csv"))

## Parameters

year <- 2017

# year_trend <- 2007 (not used for now)

i_age_cohort <- c(17, 22, 27, 32, 37, 42, 47, 52, 57, 62, 67, 72, 77, 82, 87, 92, 97)

i_sex <- c("male", "female")


# ---- chunk-2 ----

## Create baseline life tables

general_life_table_list_bl <- list()

index <- 1

for (iage in i_age_cohort){
  for (isex in i_sex){
    # cat("age ", age, " and sex ", sex, "\n") #Uncomment to see index
    suppressWarnings(general_life_table_list_bl[[index]] <- RunLifeTable(in_idata = MSLT_DF,
                                                          in_sex = isex, in_mid_age = iage))
    
    names(general_life_table_list_bl)[index] <- paste(iage, isex, sep = "_")
    index <- index + 1
  }
}


# ---- chunk-3 ----

disease_life_table_list_bl <- list()
index <- 1

for (iage in i_age_cohort){
  for (isex in i_sex){
   for (d in 1:nrow(DISEASE_SHORT_NAMES)){
      
     
      ## Exclude non-males diseases and non-chronic diseases and road injuries and disease with no pif
      if (isex == "male" && (DISEASE_SHORT_NAMES$disease[d] %in% c("breast cancer", "uterine cancer"))
          || DISEASE_SHORT_NAMES$is_not_dis[d] != 0 || DISEASE_SHORT_NAMES$acronym[d] == "no_pif" || DISEASE_SHORT_NAMES$acronym[d] == "other"){
      }
      else {
        
        # print(paste(isex, DISEASE_SHORT_NAMES$disease[d]))
      disease_life_table_list_bl[[index]] <- RunDisease(in_idata = MSLT_DF,in_mid_age = iage, in_sex = isex,  in_disease = DISEASE_SHORT_NAMES$sname[d])
      
      names(disease_life_table_list_bl)[index] <- paste(iage, isex, DISEASE_SHORT_NAMES$sname[d], sep = "_")

      index <- index + 1
             
      }
    }
  }
}


# ---- chunk-4 ----

## Create non_disease lists, these are by age and sex for road injuries and lwri baseline and scenario, including calculation of difference in rates. 
## Different calculation for scenario deaths and ylds for lri and injuries. 


non_disease_list <- list()
index <- 1


for (iage in i_age_cohort){
  for (isex in i_sex) {
    for (d in 1:nrow(DISEASE_SHORT_NAMES)){
      
      ## Exclude chronic disease and all-cause mortality and  pyld
      if (DISEASE_SHORT_NAMES$is_not_dis[d] != 1 || DISEASE_SHORT_NAMES$acronym[d] == "other" || DISEASE_SHORT_NAMES$acronym[d] == "no_pif"){
      }
      else {
        
        ## Only keep non-diseases variables in the dataframe
        td1 <- as.data.frame(filter(MSLT_DF, age >= iage & sex == isex) %>% 
          dplyr::select(age, sex, contains(DISEASE_SHORT_NAMES$acronym[DISEASE_SHORT_NAMES$sname == DISEASE_SHORT_NAMES$sname[d]])))
        
        
        pif_non_disease <- as.data.frame(filter(pif_expanded, age >= iage & sex == isex) %>% 
                                           dplyr::select(age, sex, contains(DISEASE_SHORT_NAMES$acronym[DISEASE_SHORT_NAMES$sname == DISEASE_SHORT_NAMES$sname[d]]))) 
    
        
        
        non_disease_list[[index]] <-  RunNonDisease (td1, in_sex = isex, in_mid_age = iage, in_non_disease = DISEASE_SHORT_NAMES$acronym[d])
        
        ### For LRI we modify mortality rates and ylds rates by 1-PIF. For road trauma we multiple the PIF by baseline deaths/ylds rates to generate scenario
        ### ylds and mortlality rates and then take the difference
        
        if (DISEASE_SHORT_NAMES$acronym[d] == "lri"){
       
        
        ## deaths sceanario lri
        non_disease_list[[index]][paste0("deaths_rate_sc_", DISEASE_SHORT_NAMES$acronym[d])] <-
          non_disease_list[[index]][paste0("deaths_rate_", DISEASE_SHORT_NAMES$acronym[d])] * (1 - pif_non_disease [paste0("pif_", DISEASE_SHORT_NAMES$acronym[d], "_deaths")]) 
        
        
        ## ylds scenario lri
        non_disease_list[[index]][paste0("ylds_rate_sc_", DISEASE_SHORT_NAMES$acronym[d])] <-
          non_disease_list[[index]][paste0("ylds_rate_", DISEASE_SHORT_NAMES$acronym[d])] * (1 - pif_non_disease [paste0("pif_", DISEASE_SHORT_NAMES$acronym[d], "_ylds")]) 
        
        }
        else {
          
        ## deaths sceanario injuries
        non_disease_list[[index]][paste0("deaths_rate_sc_", DISEASE_SHORT_NAMES$acronym[d])] <-
            non_disease_list[[index]][paste0("deaths_rate_", DISEASE_SHORT_NAMES$acronym[d])] * pif_non_disease [paste0("pif_", DISEASE_SHORT_NAMES$acronym[d], "_deaths")]
          
          
          ## ylds scenario injuries
          non_disease_list[[index]][paste0("ylds_rate_sc_", DISEASE_SHORT_NAMES$acronym[d])] <-
            non_disease_list[[index]][paste0("ylds_rate_", DISEASE_SHORT_NAMES$acronym[d])] * pif_non_disease [paste0("pif_", DISEASE_SHORT_NAMES$acronym[d], "_ylds")]
        }
        
        ## Difference variable
        
        ## deaths difference
        non_disease_list[[index]][paste0("diff_mort")] <- non_disease_list[[index]][paste0("deaths_rate_", DISEASE_SHORT_NAMES$acronym[d])] -
          non_disease_list[[index]][paste0("deaths_rate_sc_", DISEASE_SHORT_NAMES$acronym[d])]
        ## ylds difference
        non_disease_list[[index]][paste0("diff_ylds")] <- non_disease_list[[index]][paste0("ylds_rate_", DISEASE_SHORT_NAMES$acronym[d])] -
          non_disease_list[[index]][paste0("ylds_rate_sc_", DISEASE_SHORT_NAMES$acronym[d])]
        
        non_disease_list[[index]][IsNanDataFrame(non_disease_list[[index]])] <- 0
        
        names(non_disease_list)[index] <- paste(iage, isex, DISEASE_SHORT_NAMES$acronym[d], sep = "_")
        
        index <- index + 1
        
      }
    }
  }  
}

# ---- chunk-5 ----

## Create scenario life tables with new pifs,includes Diabetes loop. 

### Relative risks of diabetes for cardiovascular diseases

DIABETES_IHD_RR_F <<- 2.82 ## 2.35
DIABETES_STROKE_RR_F <<- 2.28 ## 1.93
DIABETES_IHD_RR_M <<- 2.16 ## 2.16
DIABETES_STROKE_RR_M <<- 1.83 ## 1.6
  

disease_life_table_list_sc <- list()
index <- 1


disease_relative_risks <- list(c(DIABETES_IHD_RR_M,DIABETES_IHD_RR_F),
                               c(DIABETES_STROKE_RR_M,DIABETES_STROKE_RR_F))
##!! diabetes must be calculated before stroke and ihd
ishd_index <- which(DISEASE_SHORT_NAMES$sname=='ishd')
strk_index <- which(DISEASE_SHORT_NAMES$sname=='strk')
dia_index <- which(DISEASE_SHORT_NAMES$sname=='dmt2')
dia_order <- c(dia_index,c(1:nrow(DISEASE_SHORT_NAMES))[-dia_index])
for (iage in i_age_cohort){
  td1_age <- MSLT_DF[MSLT_DF$age>=iage,]
  pif_disease_age <- pif_expanded[pif_expanded$age>=iage,]
  for (isex in i_sex){
    td1_age_sex <- td1_age[td1_age$sex==isex,]
    pif_disease_age_sex <- pif_disease_age[pif_disease_age$sex==isex,]
    for (d in c(1:nrow(DISEASE_SHORT_NAMES))[dia_order]){
      
      ## Exclude non-males diseases and non-chronic diseases and road injuries and disease with no pif
      if (isex == "male" && (DISEASE_SHORT_NAMES$disease[d] %in% c("breast cancer", "uterine cancer"))|| 
          DISEASE_SHORT_NAMES$is_not_dis[d] != 0 || DISEASE_SHORT_NAMES$acronym[d] == "no_pif" || DISEASE_SHORT_NAMES$acronym[d] == "other"){
        } else {
        
        
        pif_colname <- paste0('pif_',DISEASE_SHORT_NAMES$acronym[d])
       
        pif_disease <- pif_disease_age_sex[,colnames(pif_disease_age_sex) %in% c('age', 'sex', pif_colname)]
        
        # adjustment for diabetes effect on ihd and stroke
        if(d %in% c(ishd_index,strk_index)){
          # select which disease
          which_disease <- which(c(ishd_index,strk_index)==d)
          # get name for pif column
          target_disease <- c('pif_ihd','pif_stroke')[which_disease]
          # get diabetes label, just made
          dia_col <- paste0(iage,'_',isex,'_dmt2')
          # select relative risk of disease given diabetes (depends on sex, not age)
          relative_risk <- disease_relative_risks[[which_disease]][which(i_sex==isex)]
          # (store old pif)
          old_pif <- pif_disease[[target_disease]]
          # diabetes pif = - { scenario prevalence - baseline prevalence } * (RR - 1)  / { baseline prevalence * (RR - 1) + 1 }
          pif_dia <- -(disease_life_table_list_sc[[dia_col]]$px - disease_life_table_list_bl[[dia_col]]$px)*(relative_risk-1)/
            (disease_life_table_list_bl[[dia_col]]$px * (relative_risk-1) + 1)
          # modify pif for target disease: new pif =  (1 - old pif) * (1 - diabetes pif)
          pif_disease[[target_disease]] <- 1- (1-pif_disease[[target_disease]]) * (1-pif_dia)
          # print(sum(old_pif-pif_disease[[target_disease]]))
          
        }
        
        
        new_col <- td1_age_sex[[paste("incidence", DISEASE_SHORT_NAMES$sname[d], sep = "_")]] * (1 - (pif_disease[[pif_colname]]))
        
        new_col[is.na(new_col)] <- 0
        td1_age_sex[[paste("incidence", DISEASE_SHORT_NAMES$sname[d], sep = "_")]] <- new_col
        
        
        
        ## Instead of idata, feed td to run scenarios. Now all diseases are run again, with the effect of diabetes
        ## on cardiovarcular diseases taken into account. 
        
        disease_life_table_list_sc_temp <- RunDisease(in_idata = td1_age_sex, in_sex = isex,
                                                      in_mid_age = iage, in_disease = DISEASE_SHORT_NAMES$sname[d])
        
       
        
        disease_life_table_list_sc_temp$diff_inc_disease <-
          disease_life_table_list_sc_temp$incidence_disease -   disease_life_table_list_bl[[index]]$incidence_disease
        disease_life_table_list_sc_temp$diff_prev_disease <-
          disease_life_table_list_sc_temp$px  - disease_life_table_list_bl[[index]]$px
        disease_life_table_list_sc_temp$diff_mort_disease <-
          disease_life_table_list_sc_temp$mx - disease_life_table_list_bl[[index]]$mx
        disease_life_table_list_sc_temp$diff_pylds_disease <-
          (disease_life_table_list_sc_temp$px - disease_life_table_list_bl[[index]]$px) * disease_life_table_list_bl[[index]]$dw_disease
        
        disease_life_table_list_sc[[index]] <- disease_life_table_list_sc_temp
        names(disease_life_table_list_sc)[index] <- paste(iage, isex, DISEASE_SHORT_NAMES$sname[d], sep = "_")
        
        index <- index + 1
      }
    }
  }
}
## Uncommnet to check scenario life tables
# View(disease_life_table_list_sc[[3]])

# ---- chunk-8 ----

## Generate total change in mortality rate to recalculate scenario general life tables

### Vector in common by all calculations for change in mx and pylds.

index <- 1
age_sex_cols <- which(colnames(disease_life_table_list_sc[[index]])%in%c('age', 'sex'))

### Sum mortality rate change scenarios (mx_sc_total)

#### Diseases
mx_sc_total_disease <- list()
l_index <- 1
index <- 1
for (iage in i_age_cohort){
  for (isex in i_sex){
    mortality_sum <- NULL

    create_new <- T

    ## Sum all diseases mortality rates
    
    for (d in 1:nrow(DISEASE_SHORT_NAMES)) {
      if (isex == "male" && (DISEASE_SHORT_NAMES$disease[d] %in% c("breast cancer", "uterine cancer"))
          || DISEASE_SHORT_NAMES$is_not_dis[d] != 0 || DISEASE_SHORT_NAMES$acronym[d] == "no_pif" || DISEASE_SHORT_NAMES$acronym[d] == "other"){
      }
      else {
        
        # print(paste(isex, DISEASE_SHORT_NAMES$disease[d]))
        
        if (create_new){
          mortality_sum <- disease_life_table_list_sc[[index]][,age_sex_cols]
          mortality_sum$total <- 0
          create_new <- F
          mortality_sum$total <- mortality_sum$total +
            (disease_life_table_list_sc[[index]]$diff_mort_disease)
        }else{
          mortality_sum$total <- mortality_sum$total +
            (disease_life_table_list_sc[[index]]$diff_mort_disease)
        }
        
        # cat(age, " - ", sex," - ",  disease," - ",  index, " - ", l_index,  "\n")
        index <- index + 1
      }
    }
    mx_sc_total_disease[[l_index]] <- mortality_sum 
    names(mx_sc_total_disease)[l_index] <- paste(iage, isex)
    
    l_index <- l_index + 1
      
    }
  }


#### NonDiseases (road injuries and lri)
mx_sc_total_non_disease <- list()
l_index <- 1
index <- 1
for (iage in i_age_cohort){
  for (isex in i_sex){
    mortality_sum <- NULL
    mortality_sum1 <- NULL
    create_new <- T
    
 ## Sum all non_diseases mortality rates
    
    for (d in 1:nrow(DISEASE_SHORT_NAMES)) {
      if (DISEASE_SHORT_NAMES$is_not_dis[d] != 1 || DISEASE_SHORT_NAMES$acronym[d] == "no_pif" || DISEASE_SHORT_NAMES$acronym[d] == "other"){
      }
      else {
        
        # print(paste(isex, DISEASE_SHORT_NAMES$disease[d]))
        
        if (create_new){
          mortality_sum <- non_disease_list[[index]][,age_sex_cols]
          mortality_sum$total <- 0
          create_new <- F
          mortality_sum$total <- mortality_sum$total +
            (non_disease_list[[index]]$diff_mort)
        }else{
          mortality_sum$total <- mortality_sum$total +
            (non_disease_list[[index]]$diff_mort)
        }
        
        # cat(age, " - ", sex," - ",  disease," - ",  index, " - ", l_index,  "\n")
        index <- index + 1
      }
    }
    mx_sc_total_non_disease[[l_index]] <- mortality_sum
    names(mx_sc_total_non_disease)[l_index] <- paste(iage, isex)
    
    l_index <- l_index + 1
    
  }
}



## YLDs change
### Diseases

pylds_sc_total_disease <- list()
l_index <- 1
index <- 1
for (iage in i_age_cohort){
  for (isex in i_sex){
    pylds_sum <- NULL
    create_new <- T
    
    for (d in 1:nrow(DISEASE_SHORT_NAMES)) {
      if (isex == "male" && (DISEASE_SHORT_NAMES$disease[d] %in% c("breast cancer", "uterine cancer"))
          || DISEASE_SHORT_NAMES$is_not_dis[d] != 0 || DISEASE_SHORT_NAMES$acronym[d] == "no_pif" || DISEASE_SHORT_NAMES$acronym[d] == "other"){
      }
      else {

        if (create_new){
          
          pylds_sum <- disease_life_table_list_sc[[index]][,age_sex_cols]
          pylds_sum$total <- 0
          create_new <- F
          pylds_sum$total <- pylds_sum$total +
            (disease_life_table_list_sc[[index]]$diff_pylds_disease)
        }else{
          pylds_sum$total <- pylds_sum$total +
            (disease_life_table_list_sc[[index]]$diff_pylds_disease)
        }
        
        # cat(age, " - ", sex," - ",  disease," - ",  index, " - ", l_index,  "\n")
        index <- index + 1
      }
      
    }
    pylds_sc_total_disease[[l_index]] <- pylds_sum
    names(pylds_sc_total_disease)[l_index] <- paste(iage, isex)
    l_index <- l_index + 1
  }
}

### Non-disease

pylds_sc_total_non_disease <- list()
l_index <- 1
index <- 1
for (iage in i_age_cohort){
  for (isex in i_sex){
    pylds_sum <- NULL
    create_new <- T
    
    for (d in 1:nrow(DISEASE_SHORT_NAMES)) {
      if (isex == "male" && (DISEASE_SHORT_NAMES$disease[d] %in% c("breast cancer", "uterine cancer"))
          || DISEASE_SHORT_NAMES$is_not_dis[d] != 1 || DISEASE_SHORT_NAMES$acronym[d] == "no_pif" || DISEASE_SHORT_NAMES$acronym[d] == "other"){
      }
      else {
        
        # print(paste(isex, DISEASE_SHORT_NAMES$disease[d]))
        
        if (create_new){
          pylds_sum <- non_disease_list[[index]][,age_sex_cols]
          pylds_sum$total <- 0
          create_new <- F
          pylds_sum$total <- pylds_sum$total +
            (non_disease_list[[index]]$diff_ylds)
        }else{
          pylds_sum$total <- pylds_sum$total +
            (non_disease_list[[index]]$diff_ylds)
        }
        
        # cat(age, " - ", sex," - ",  disease," - ",  index, " - ", l_index,  "\n")
        index <- index + 1
      }
      
    }
    pylds_sc_total_non_disease[[l_index]] <- pylds_sum
    names(pylds_sc_total_non_disease)[l_index] <- paste(iage, isex)
    l_index <- l_index + 1
  }
}

# ---- chunk-9 ----

## Calculate general life tables with modified mortality and pylds total
## Original mortality rate is modified by the mx_sc_total (total change in mortality from diseases)
## Original pyld rate is modified by the change in each disease pylds

general_life_table_list_sc <- list()
index <- 1

for (iage in i_age_cohort){
  for (isex in i_sex){
    
    
    # cat("age ", age, " and sex ", sex, "\n")
    # modify idata's mortality and pyld total for the said scenario
    td2 <- MSLT_DF
    # td2 <- subset(td2, select = -c(mx, pyld_rate))
    td2[td2$age >= iage & td2$sex == isex,][[paste("mx")]] <- general_life_table_list_bl[[index]]$mx + mx_sc_total_disease[[index]]$total + mx_sc_total_non_disease[[index]]$total
    td2[td2$age >= iage & td2$sex == isex,][[paste("pyld_rate")]] <- general_life_table_list_bl[[index]]$pyld_rate + pylds_sc_total_disease[[index]]$total + pylds_sc_total_non_disease[[index]]$total
    
    # Instead of idata, feed td to run scenarios
    general_life_table_list_sc[[index]] <- RunLifeTable(in_idata = td2, in_sex = isex, in_mid_age = iage)
    #
    
    names(general_life_table_list_sc)[index] <- paste(iage, isex, sep = "_")
    
    
    index <- index + 1
  }
}

# ---- chunk-10 ----

## In the following list "output_life_table", 34 data frames are nested per age and sex cohort

output_burden <- list()
l_index <- 1
index <- 1
index_n <- 1

## Vectors diseases
sc_cols <- which(colnames(disease_life_table_list_sc[[index]])%in%c('age', 'sex', 'incidence_disease', 'mx', 'px'))
bl_cols <- which(colnames(disease_life_table_list_bl[[index]])%in%c('incidence_disease', 'mx', 'px'))

## Vectors general life table
l_sc_cols <- which(colnames(general_life_table_list_sc[[l_index]])%in%c('Lx', 'Lwx'))
l_bl_cols <- which(colnames(general_life_table_list_bl[[l_index]])%in%c('Lx', 'Lwx'))


for (iage in i_age_cohort){
  for (isex in i_sex){
    
    # Males do not have breast cancer, that is why we need the if/else.
    # We create a TRUE/FALSE variable for the loop to move into the next disease
    
    create_new <- T
    for (d in 1:nrow(DISEASE_SHORT_NAMES)) {
      if (isex == "male" && (DISEASE_SHORT_NAMES$disease[d] %in% c("breast cancer", "uterine cancer"))
          || DISEASE_SHORT_NAMES$is_not_dis[d] != 0 || DISEASE_SHORT_NAMES$acronym[d] == "no_pif" || DISEASE_SHORT_NAMES$acronym[d] == "other"){
      }
      else {
        
        if (create_new){
          
          ## Burden sceanrio rates per one
          output_burden_sc <- disease_life_table_list_sc[[index]][,sc_cols]
          names(output_burden_sc)[names(output_burden_sc) == 'incidence_disease'] <-
            paste('incidence_disease', DISEASE_SHORT_NAMES$sname[d], "sc", sep = "_")
          names(output_burden_sc)[names(output_burden_sc) == 'mx'] <-
            paste('mx', DISEASE_SHORT_NAMES$sname[d], "sc", sep = "_")
          names(output_burden_sc)[names(output_burden_sc) == 'px'] <-
            paste('px', DISEASE_SHORT_NAMES$sname[d], "sc", sep = "_")
         
          ## Burden baseline rates per one
          output_burden_bl <- disease_life_table_list_bl[[index]][,bl_cols]
          names(output_burden_bl)[names(output_burden_bl) == 'incidence_disease'] <-
            paste('incidence_disease', DISEASE_SHORT_NAMES$sname[d], "bl", sep = "_")
          names(output_burden_bl)[names(output_burden_bl) == 'mx'] <-
            paste('mx', DISEASE_SHORT_NAMES$sname[d], "bl", sep = "_")
          names(output_burden_bl)[names(output_burden_bl) == 'px'] <-
            paste('px', DISEASE_SHORT_NAMES$sname[d], "bl", sep = "_")
          
          ## New list to add calculations for changes in burden of disease (incidence and mortality numbers)
          
          output_burden_change <- list()
          
          output_burden_change$inc_num_bl <- disease_life_table_list_bl[[index]]$incidence_disease *
            (1 - disease_life_table_list_bl[[index]]$px) * general_life_table_list_bl[[l_index]]$Lx
          output_burden_change$inc_num_sc <- disease_life_table_list_sc[[index]]$incidence_disease *
            (1 - disease_life_table_list_sc[[index]]$px) * general_life_table_list_sc[[l_index]]$Lx
          output_burden_change$inc_num_diff <- (disease_life_table_list_sc[[index]]$incidence_disease *
                                                  (1 - disease_life_table_list_sc[[index]]$px) * general_life_table_list_sc[[l_index]]$Lx) - (disease_life_table_list_bl[[index]]$incidence_disease * (1 - disease_life_table_list_bl[[index]]$px)
                                                                                                                                              * general_life_table_list_bl[[l_index]]$Lx)
          
          output_burden_change$mx_num_bl <- disease_life_table_list_bl[[index]]$mx * general_life_table_list_bl[[l_index]]$Lx
          output_burden_change$mx_num_sc <- disease_life_table_list_sc[[index]]$mx * general_life_table_list_sc[[l_index]]$Lx
          output_burden_change$mx_num_diff <- (disease_life_table_list_sc[[index]]$mx * general_life_table_list_sc[[l_index]]$Lx) - (disease_life_table_list_bl[[index]]$mx * general_life_table_list_bl[[l_index]]$Lx)
          
          names(output_burden_change)[names(output_burden_change) == 'inc_num_bl'] <-
            paste('inc_num_bl', DISEASE_SHORT_NAMES$sname[d], sep = "_")
          names(output_burden_change)[names(output_burden_change) == 'inc_num_sc'] <-
            paste('inc_num_sc', DISEASE_SHORT_NAMES$sname[d], sep = "_")
          names(output_burden_change)[names(output_burden_change) == 'inc_num_diff'] <-
            paste('inc_num_diff', DISEASE_SHORT_NAMES$sname[d], sep = "_")
          names(output_burden_change)[names(output_burden_change) == 'mx_num_bl'] <-
            paste('mx_num_bl', DISEASE_SHORT_NAMES$sname[d], sep = "_")
          names(output_burden_change)[names(output_burden_change) == 'mx_num_sc'] <-
            paste('mx_num_sc', DISEASE_SHORT_NAMES$sname[d], sep = "_")
          names(output_burden_change)[names(output_burden_change) == 'mx_num_diff'] <-
            paste('mx_num_diff', DISEASE_SHORT_NAMES$sname[d], sep = "_")
          
          ## Bind all lists
          
          output_burden_sc <- cbind(output_burden_sc, output_burden_bl)
          output_burden_sc <- cbind(output_burden_sc, output_burden_change)
          
          create_new <- F
          
          ## Here the calculations above are repeated, here is where the F is telling to move into the next disease
          
        }else{
          
         
          td3 <- disease_life_table_list_sc[[index]][,colnames(disease_life_table_list_sc[[index]])%in%c('incidence_disease', 'mx', 'px')]
          names(td3)[names(td3) == 'incidence_disease'] <-
            paste('incidence_disease', DISEASE_SHORT_NAMES$sname[d], "sc", sep = "_")
          names(td3)[names(td3) == 'mx'] <-
            paste('mx', DISEASE_SHORT_NAMES$sname[d], "sc", sep = "_")
          names(td3)[names(td3) == 'px'] <-
            paste('px', DISEASE_SHORT_NAMES$sname[d], "sc", sep = "_")
          
          
          td4 <- disease_life_table_list_bl[[index]][,colnames(disease_life_table_list_bl[[index]])%in%c('incidence_disease', 'mx', 'px')]
          names(td4)[names(td4) == 'incidence_disease'] <-
            paste('incidence_disease', DISEASE_SHORT_NAMES$sname[d], "bl", sep = "_")
          names(td4)[names(td4) == 'mx'] <-
            paste('mx', DISEASE_SHORT_NAMES$sname[d], "bl", sep = "_")
          names(td4)[names(td4) == 'px'] <-
            paste('px', DISEASE_SHORT_NAMES$sname[d], "bl", sep = "_")
          
          output_burden_change2 <- list()
          
          output_burden_change2$inc_num_bl <- disease_life_table_list_bl[[index]]$incidence_disease * (1 - disease_life_table_list_bl[[index]]$px) * general_life_table_list_bl[[l_index]]$Lx
          output_burden_change2$inc_num_sc <- disease_life_table_list_sc[[index]]$incidence_disease * (1 - disease_life_table_list_sc[[index]]$px) * general_life_table_list_sc[[l_index]]$Lx
          output_burden_change2$inc_num_diff <- (disease_life_table_list_sc[[index]]$incidence_disease * (1 - disease_life_table_list_sc[[index]]$px) * general_life_table_list_sc[[l_index]]$Lx) - (disease_life_table_list_bl[[index]]$incidence_disease * (1 - disease_life_table_list_bl[[index]]$px) * general_life_table_list_bl[[l_index]]$Lx)
          
          output_burden_change2$mx_num_bl <- disease_life_table_list_bl[[index]]$mx * general_life_table_list_bl[[l_index]]$Lx
          output_burden_change2$mx_num_sc <- disease_life_table_list_sc[[index]]$mx * general_life_table_list_sc[[l_index]]$Lx
          output_burden_change2$mx_num_diff <- (disease_life_table_list_sc[[index]]$mx * general_life_table_list_sc[[l_index]]$Lx) - (disease_life_table_list_bl[[index]]$mx * general_life_table_list_bl[[l_index]]$Lx)
          
          names(output_burden_change2)[names(output_burden_change2) == 'inc_num_bl'] <-
            paste('inc_num_bl', DISEASE_SHORT_NAMES$sname[d], sep = "_")
          names(output_burden_change2)[names(output_burden_change2) == 'inc_num_sc'] <-
            paste('inc_num_sc', DISEASE_SHORT_NAMES$sname[d], sep = "_")
          names(output_burden_change2)[names(output_burden_change2) == 'inc_num_diff'] <-
            paste('inc_num_diff', DISEASE_SHORT_NAMES$sname[d], sep = "_")
          names(output_burden_change2)[names(output_burden_change2) == 'mx_num_bl'] <-
            paste('mx_num_bl', DISEASE_SHORT_NAMES$sname[d], sep = "_")
          names(output_burden_change2)[names(output_burden_change2) == 'mx_num_sc'] <-
            paste('mx_num_sc', DISEASE_SHORT_NAMES$sname[d], sep = "_")
          names(output_burden_change2)[names(output_burden_change2) == 'mx_num_diff'] <-
            paste('mx_num_diff', DISEASE_SHORT_NAMES$sname[d], sep = "_")
          
          
          ## Bind all lists
          
          output_burden_sc <- cbind(output_burden_sc, td3)
          output_burden_sc <- cbind(output_burden_sc, td4)
          output_burden_sc$age_cohort <- iage
          output_burden_sc <- cbind(output_burden_sc, output_burden_change2)
          
        }
        
        # cat(iage, " - ", isex," - ",  disease," - ",  index, " - ", l_index,  "\n")
        index <- index + 1
      }
      
    }
    
    #### Add non_diseases (injuires and lwri)
    
    create_new <- T
    for (d in 1:nrow(DISEASE_SHORT_NAMES)) {
      if (DISEASE_SHORT_NAMES$is_not_dis[d] != 1 || DISEASE_SHORT_NAMES$acronym[d] == "no_pif" || DISEASE_SHORT_NAMES$acronym[d] == "other"){
      }
      else {
        if (create_new){
          
          td5 <- non_disease_list[[index_n]]
          
          
          ## Calculate numbers
          
          td5$mx_num_diff <- non_disease_list[[index_n]]$diff_mort * general_life_table_list_bl[[l_index]]$Lx
          
          td5$ylds_num_diff <- non_disease_list[[index_n]]$diff_ylds * general_life_table_list_bl[[l_index]]$Lx
          
          
          ### Change names rates to match each non_disease
          
          names(td5)[names(td5) == "diff_ylds"] <- paste("ylds_rate_diff_", DISEASE_SHORT_NAMES$acronym[d], sep = "_")
          
          names(td5)[names(td5) == "diff_mort"] <- paste("deaths_rate_diff_", DISEASE_SHORT_NAMES$acronym[d], sep = "_")
          
          ## Change names numbers to match non_disease
          
          names(td5)[names(td5) == "mx_num_diff"] <- paste("mx_num_diff_", DISEASE_SHORT_NAMES$acronym[d], sep = "_")
          
          names(td5)[names(td5) == "ylds_num_diff"] <- paste("ylds_num_diff_", DISEASE_SHORT_NAMES$acronym[d], sep = "_")
          
          
          create_new <- F
          
          ## Here the calculations above are repeated, here is where the F is telling to move into the next disease
          
        }else{
          
          td6 <- non_disease_list[[index_n]]
          
          ## Calculate numbers
          
          td6$mx_num_diff <- non_disease_list[[index_n]]$diff_mort * general_life_table_list_bl[[l_index]]$Lx
          
          td6$ylds_num_diff <- non_disease_list[[index_n]]$diff_ylds * general_life_table_list_bl[[l_index]]$Lx
          
          
          ### Change names rates to match each non_disease
          
          names(td6)[names(td6) == "diff_ylds"] <- paste("ylds_rate_diff_", DISEASE_SHORT_NAMES$acronym[d], sep = "_")
          
          names(td6)[names(td6) == "diff_mort"] <- paste("deaths_rate_diff_", DISEASE_SHORT_NAMES$acronym[d], sep = "_")
          
          ## Change names numbers to match non_disease
          
          names(td6)[names(td6) == "mx_num_diff"] <- paste("mx_num_diff_", DISEASE_SHORT_NAMES$acronym[d], sep = "_")
          
          names(td6)[names(td6) == "ylds_num_diff"] <- paste("ylds_num_diff_", DISEASE_SHORT_NAMES$acronym[d], sep = "_")
          
          
          ## Bind all lists
          
          output_burden_sc <- cbind(output_burden_sc, td5)
          
          output_burden_sc <- cbind(output_burden_sc, td6)
          
        }
        index_n <- index_n + 1
      }
    }
    
    
    ## general_life_table_list_sc and general_life_table_list_bl (Lx)
    
    output_burden_lf_sc <- dplyr::select(general_life_table_list_sc[[l_index]], c('Lx', 'Lwx'))
    names(output_burden_lf_sc)[names(output_burden_lf_sc) == 'Lx'] <- paste('Lx', "sc", sep = "_")
    names(output_burden_lf_sc)[names(output_burden_lf_sc) == 'Lwx'] <- paste('Lwx', "sc", sep = "_")
    
    output_burden_lf_bl <- dplyr::select(general_life_table_list_bl[[l_index]], c('Lx', 'Lwx'))
    names(output_burden_lf_bl)[names(output_burden_lf_bl) == 'Lx'] <- paste('Lx', "bl", sep = "_")
    names(output_burden_lf_bl)[names(output_burden_lf_bl) == 'Lwx'] <- paste('Lwx', "bl", sep = "_")
    
    
    output_burden_lf_sc$Lx_diff <- general_life_table_list_bl[[l_index]]$Lx - general_life_table_list_sc[[l_index]]$Lx
    output_burden_lf_sc$Lwx_diff <- general_life_table_list_bl[[l_index]]$Lwx - general_life_table_list_sc[[l_index]]$Lwx
    
    output_burden_sc <- cbind(output_burden_sc, output_burden_lf_sc)
    output_burden_sc <- cbind(output_burden_sc, output_burden_lf_bl)
    
    
    output_burden[[l_index]] <- output_burden_sc
    
    names(output_burden)[l_index] <- paste(iage, isex, sep = "_")
    
    
    l_index <- l_index + 1
    
  }
}

# ---- chunk-11 ----



# ---- chunk-13 ---- 

## Generate a data frame for all results and create function to get outcomes.

output_df <- plyr::ldply(output_burden, rbind)


# ---- chunk- 14 ----  

output_dir = "output/"

## Graphs for diseases by age and sex, disease incidence and mortality numbers diseases. 

## Generate number of plots diseases males and females (non-diseases do separetly)

nplots_males <- filter(DISEASE_SHORT_NAMES, males == 1 &(!acronym %in% c('no_pif' , 'other') & is_not_dis == 0)) %>% nrow()

nplots_females <- filter(DISEASE_SHORT_NAMES, females == 1 &(!acronym %in% c('no_pif' , 'other') & is_not_dis == 0)) %>% nrow()


iage <- 17
isex <- "male"

i_outcome <- c("mx", "inc")
p_list_male <- list()
p_list_female <- list()
male_index <- 1
female_index <- 1
for (iage in i_age_cohort){
  for (isex in i_sex) {
    for (outcome in i_outcome) {
      for (d in 1:nrow(DISEASE_SHORT_NAMES)) {
        
        
        if (isex == "male" && (DISEASE_SHORT_NAMES$disease[d] %in% c("breast cancer", "uterine cancer"))
            || DISEASE_SHORT_NAMES$acronym[d] == "no_pif" || DISEASE_SHORT_NAMES$acronym[d] == "other" || DISEASE_SHORT_NAMES$is_not_dis[d] !=0){
        }
        else{
          
          p_index  <- PlotOutput(in_data = output_df, in_age = iage, in_population = isex, in_outcomes = c("age", paste(outcome, "num", "bl", DISEASE_SHORT_NAMES$sname[d], sep = "_"), paste(outcome, "num", "sc", DISEASE_SHORT_NAMES$sname[d], sep = "_"), paste(outcome, "num", "diff", DISEASE_SHORT_NAMES$sname[d], sep = "_")), in_legend = "", in_disease = DISEASE_SHORT_NAMES$disease[d])
          
          if (isex == "male"){
            
            p_list_male[[male_index]] <- p_index
            
            
            if (male_index %% nplots_males == 0 && male_index > 0){
              
              for (np in 1:nplots_males){
                if (np != nplots_males)
                  assign(paste0("local_plot_object", np), p_list_male[[male_index - (nplots_males - np)]] + theme(legend.position="none", axis.title.x = element_blank(),  axis.title.y = element_blank()))
                else
                  assign(paste0("local_plot_object", np), p_index + theme(legend.position="none", axis.title.x = element_blank(),  axis.title.y = element_blank()))
                  
              

            # jpeg(filename = paste0(output_dir, paste(iage, isex, outcome, sep="_"), ".jpeg"))
            jpeg("test.jpeg")
              # 
            
            
            GridArrangSharedLegend (local_plot_object, ncol = 3, nrow = 2, mainTitle = paste(ifelse(outcome == 'mx', 'Deaths', 'Incidence'), isex, iage),
            mainLeft = 'Cases', mainBottom = 'Age')
            dev.off()

            }
            
            ## This saves plots for each disease by age and sex
              
              
           # ggsave(p_index, file=paste(output_dir, DISEASE_SHORT_NAMES$sname[d], "_", isex, iage, ".tiff", sep=""), width = 14, height = 10, units = "cm")
           #  
            
            ## Add plot by age and sex and all diseases
            
            
            
            male_index <- male_index + 1
            
            }
          }
          
          if (isex == "female"){

            p_list_female[[female_index]] <- p_index
            
            if (female_index %% nplots_females == 0 && female_index > 0){
              
              for (np in 1:nplots_females){
                if (np != nplots_females)
                  assign(paste0("local_plot_object", np), p_list_female[[female_index - (nplots_females - np)]] + theme(legend.position="none", axis.title.x = element_blank(),  axis.title.y = element_blank()))
                else
                  assign(paste0("local_plot_object", np), p_index + theme(legend.position="none", axis.title.x = element_blank(),  axis.title.y = element_blank()))
          
              }
            }
          }
        }
      }
    }
  }
}





## Loop to include graphs in the document(CHECK WHAT IS THE USE OF THIS CODE)

# graphs_doc <- list()
# index <- 1
# for (age in i.age.cohort) {
#   for (sex in i.sex)  {
#     for (outcome in i_outcome) {
# 
#       graphs_doc [[index]] <- c(paste(output_dir, "/",age,"_",sex,"_", outcome,".jpeg", sep = ""))
#       knitr::include_graphics(graphs_doc [[index]])
# 
#       index <- index + 1
#     }
#   }
# }






# ---- chunk-15 ----

aggregate_frame_males <- list()
aggregate_frame_females <- list()

index <- 1

for (outcome in i_outcome) {
  for (disease in i_disease) {
    
    aggregate_frame_males[[index]] <- GenAggregate(in_data = output_df, in_cohorts = 16, in_population = "male", in_outcomes = c(paste(outcome, "num", "bl", disease, sep = "_"), paste(outcome, "num", "sc", disease, sep = "_"), paste(outcome, "num", "diff", disease, sep = "_")))
    
    aggregate_frame_females[[index]] <- GenAggregate(in_data = output_df, in_cohorts = 16, in_population = "female", in_outcomes = c(paste(outcome, "num", "bl", disease, sep = "_"), paste(outcome, "num", "sc", disease, sep = "_"), paste(outcome, "num", "diff", disease, sep = "_")))
    
    # Remove non-numeric columns starting with age and sex
    aggregate_frame_males[[index]] <- aggregate_frame_males[[index]] %>% dplyr::select(-starts_with("age"), -starts_with("sex"))
    
    aggregate_frame_females[[index]] <- aggregate_frame_females[[index]] %>% dplyr::select(-starts_with("age"), -starts_with("sex"))
    
    index <- index + 1
  }
}


## Loop for life years (Lx) and health adjusted life years (Lwx) to then add to total aggregated data.

## RUN ANOTHER LOOP FOR LX AND LWX

i_outcome2 <- c("Lx", "Lwx")

aggregate_frame_males2 <- list()
aggregate_frame_females2 <- list()

for (i in i_outcome2){
  
  aggregate_frame_males2[[i]] <- GenAggregate(in_data = output_df, in_cohorts = 16, in_population = "male", in_outcomes = c(paste(i, "bl", sep = "_"), paste(i, "sc", sep = "_"), paste(i, "diff",sep = "_")))
  
  aggregate_frame_females2[[i]] <- GenAggregate(in_data = output_df, in_cohorts = 16, in_population = "female", in_outcomes = c(paste(i, "bl", sep = "_"), paste(i,  "sc", sep = "_"), paste(i, "diff",sep = "_")))
  
  
  aggregate_frame_males2[[i]] <- aggregate_frame_males2[[i]] %>% dplyr::select(-starts_with("age"), -starts_with("sex"))
  
  aggregate_frame_females2[[i]] <- aggregate_frame_females2[[i]] %>% dplyr::select(-starts_with("age"), -starts_with("sex"))
  
}

## Uncomment to check data frames (list correspond to a disease and outcome combination, for example,
## incidence breast cancer)

# View(aggregate_frame_females[[2]])

## Transform list to data frame (this is not working after the first disease)

aggregate_frame_females <- do.call(cbind, aggregate_frame_females)
aggregate_frame_males <- do.call(cbind, aggregate_frame_males)
aggregate_frame_females2 <- do.call(cbind, aggregate_frame_females2)
aggregate_frame_males2 <- do.call(cbind, aggregate_frame_males2)

View(aggregate_frame_females2)

## Drop ending of variables names _males/_females to add up all outcomes in next step.

names(aggregate_frame_females) = gsub(pattern = "_female", replacement = "", x = names(aggregate_frame_females))

names(aggregate_frame_males) = gsub(pattern = "_male", replacement = "", x = names(aggregate_frame_males))

names(aggregate_frame_females2) = gsub(pattern = "_female", replacement = "", x = names(aggregate_frame_females2))

names(aggregate_frame_males2) = gsub(pattern = "_male", replacement = "", x = names(aggregate_frame_males2))


## Uncomment to check that the code is dropping the male/female ending.
# View(aggregate_frame_males)

## Create a copy of aggregate_frame_females.
total_aggr1 <- aggregate_frame_females
## Add aggregate_frame_males values to it
for (i in 1:ncol(aggregate_frame_females)){
  total_aggr1[i] <- total_aggr1[i] + aggregate_frame_males[i]
}

## Add data frames 2 with life years (check adds Lx and Lwx at the beginning of the variable name)
total_aggr2 <- aggregate_frame_females2
## Add aggregate_frame_males values to it
for (i in 1:ncol(aggregate_frame_females2)){
  total_aggr2[i] <- total_aggr2[i] + aggregate_frame_males2[i]
}

## Combine data frames

total_aggr <- cbind.data.frame(total_aggr1, total_aggr2)

total_aggr$sim_year <- seq.int(nrow(total_aggr))


## Uncomment to see total data frame
View(total_aggr)

## Test that total_aggr is adding males and females dataframes
#
# (aggregate_frame_females2$Lx.Lx_bl_22 + aggregate_frame_males2$Lx.Lx_bl_22) -total_aggr$Lx.Lx_bl_22




# ---- chunk-16 ----


####This plot has to be customised to in_outcomes, here, only totals shown, but specifications are up to the user. ADD LOOP for all outcomes over time and total TABLE.

####[] is used here to indicate the number of simulation years into the future.
####Disease outcomes has to be changed to the outcome of interest

#### Test code with loops for aggregated outcomes diseases burden. NOT WORKING.

### Compare with loops for age and sex cohort outcomes.

p_aggr_list <- list()
index <- 1

for (outcome in i_outcome) {
  for (disease in i_disease) {
    # outcome <- i_outcome[1]
    # disease <- i_disease[1]
    
    p_aggr_list_index <- ggplot(total_aggr[1:79,], aes(x = total_aggr[["sim_year"]])) +
      
      geom_line(mapping = aes(y = total_aggr[[paste("total", outcome, "num_bl", disease, sep = "_")]], colour = paste("total", outcome, "num_bl", disease, sep = "_"))) +
      theme_classic() +
      geom_hline(yintercept=0, linetype="dashed", color = "black") +
      geom_line(mapping = aes(y = total_aggr[[paste("total", outcome, "num_sc", disease, sep = "_")]], colour = paste("total", outcome, "num_sc", disease, sep = "_"))) +
      geom_line(mapping = aes(y = total_aggr[[paste("total", outcome, "num_diff", disease, sep = "_")]], colour = paste("total", outcome, "num_diff", disease, sep = "_"))) +
      xlab ("Simulation years") + ylab ("Cases") + labs (title = paste(disease, outcome)) +
      theme(plot.title = element_text(hjust = 0.5, size = 12)) +
      scale_color_discrete(name = paste(""), labels = c("Baseline", "Difference", "Scenario")) +
      theme(plot.title = element_text(hjust = 0.5))
    p_aggr_list[[index]] <- p_aggr_list_index
    index <- index + 1
    
    
  }
}

index <- 1

interpolation_index <- 1
for (outcome in i_outcome) {
  for (disease in i_disease) {
    file_name = paste("output/graphs", "Aggregated Outcomes", outcome, disease, ".jpeg", sep=" ")
    jpeg(file_name)
    print(p_aggr_list[[index]])
    index <- index + 1
    dev.off()
  }
}


p_aggregated <- do.call(marrangeGrob, list(grobs=p_aggr_list, nrow = 2, ncol = 2))
p_aggregated







##### To do

# Add remission if we decide to use it for cancers: wont be added
# Use disbayes to test dismod alternative: compare couple of disease processed in dismod and disbayes
# move all parameters to the top of the code (e.g. disease life table generation)

# Prevalence estimates in the GBD assume remission after 10 years. If we do not assume remission, then we need to: 1) use only incidence and mortlaity
# for cancers estimates in Dismod; 2) Deflate DW by the ratio of Dismod estimated prevalence and GBD prevalence. 
# From GBD appendix: "Prevalence for all cancers is estimated for a maximum of ten years after incidence, as in GBD 2013-2016. Prevalence beyond the
# ten yera period is only estimated for permanent sequelae resulting from procedures. 
# GBD reference for data: James, S. L., et al. (2018). "Global, regional, and national incidence, prevalence, and years lived with disability for 354 diseases and injuries for 195 countries and territories, 1990&#x2013;2017: a systematic analysis for the Global Burden of Disease Study 2017." The Lancet 392(10159): 1789-1858.
# For the UK (GBD Ref): For the UK the analyses is by Local Administrative Area (level 6 in GBD hierarchy). 
# Do a set of Dismod outputs with remission for cancers to test the difference in results. 
# use yml to define variables to facilitate reuse of code. See Carls suggestions.config.yml
# Get all data from the GBD
# add diabetes do CVD loop
# add road injuries spreadsheet and data (by victim type)
# apply RRs correction methods (see Jan's 2010 paper, this is what rr erzats function does)
# SCENARIOS: LYNN'S INTERVENTIONS OR CONTANCT EACH CITY REGION. THE CITY REGIONS AND THEN THE REST. 
# Modelling of lower respiratory infecito

# Change to own wd

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

source("code/functions.R")

## AVOID HARDCODING NAMES

year <- 2017

# year_trend <- 2007

i_age_cohort <- c(17, 22, 27, 32, 37, 42, 47, 52, 57, 62, 67, 72, 77, 82, 87, 92, 97)

i_sex <- c("male", "female")

# ---- chunk-2 ----

## Create baseline life tables

general_life_table_list_bl <- list()

index <- 1

for (age in i_age_cohort){
  for (sex in i_sex){
    # cat("age ", age, " and sex ", sex, "\n") #Uncomment to see index
    general_life_table_list_bl[[index]] <- RunLifeTable(in_idata = mslt_df,
                                                          in_sex = sex, in_mid_age = age)
    index <- index + 1
  }
}

## Uncommnet to check life table list


# ---- chunk-3 ----

## Create basline disease life tables (NEED TO MATCH ALL NAMES WITH ACRONYMNS, NOT MATCHING NOW, CHECK DATA GENERATIONS, BEST NOT TO DO MANUALLY)

disease_life_table_list_bl <- list()
index <- 1

for (age in i_age_cohort){
  for (sex in i_sex){
   for (d in 1:nrow(disease_short_names)){
      
      # Exclude non-males diseases
      if ((sex == "male" && disease_short_names$disease[d] == "breast cancer") || (sex == "male" && disease_short_names$disease[d] == "uterine cancer")) {
      }
      ## Exclude non-chronic diseases and road injuries and disease with no pif
      if (disease_short_names$is_not_dis[d] != 0 || disease_short_names$acronym[d] == "no_pif" || disease_short_names$acronym[d] == "other"){
      }
      else {
      
      disease_life_table_list_bl[[index]] <- RunDisease(in_idata = mslt_df, in_sex = sex, in_mid_age = age, in_disease = disease_short_names$sname[d])
      names(disease_life_table_list_bl)[index] <- paste(age, sex, disease_short_names$sname[d], sep = "_")
        
      index <- index + 1
             
      }
    }
  }
}

# View(disease_life_table_list_bl[[2]])


# ---- chunk 4 ---- TO DO

## add baseline diabetes prevalence 
## calculate 

# ---- chunk-5 ----

## Create non_disease lists, these are by age and sex for road injuries and lwri baseline and scenario, including calculation of difference in rates


##### DISCUSS WITH ROB NAMING CONVENTION, OR TEST LOOPS DEVELOPED WITH ALI TO AVOID

#### Non-disease (injuries and lri, applied directly to general life table mortality and ylds)

pifs_no_disease_deaths <- list()
index <- 1


for (age in i_age_cohort) {
  for (sex in i_sex) {
    for (d in 1:nrow(disease_short_names)){
      
      ## Exclude chronic disease and all-cause mortality and  pyld
      if (disease_short_names$is_not_dis[d] != 1 || disease_short_names$acronym[d] == "other") {
      }
      else {
        
        var_name_deaths <- paste0("pif_", disease_short_names$acronym[d], "_", "deaths")
        
        
        pifs_no_disease_deaths[[index]] <- GetPif(pif, age, sex, var_name_deaths)
        pifs_no_disease_deaths[[index]]$sex <- sex
        pifs_no_disease_deaths[[index]]$deaths <- var_name_deaths
        names(pifs_no_disease_deaths[[index]])[names(pifs_no_disease_deaths[[index]]) == var_name_deaths] <- "pif"
        
        index <- index + 1
        
      }
    }
  }
}


pifs_no_disease_ylds <- list()
index <- 1


for (age in i_age_cohort) {
  for (sex in i_sex) {
    for (d in 1:nrow(disease_short_names)){
      
      ## Exclude chronic disease and all-cause mortality and  pyld
      if (disease_short_names$is_not_dis[d] != 1 || disease_short_names$acronym[d] == "other") {
      }
      else {
        
        var_name_ylds<- paste0("pif_", disease_short_names$acronym[d], "_", "ylds")
        
        pifs_no_disease_ylds[[index]] <- GetPif(pif, age, sex, var_name_ylds)
        pifs_no_disease_ylds[[index]]$sex <- sex
        pifs_no_disease_ylds[[index]]$deaths <- var_name_ylds
        names(pifs_no_disease_ylds[[index]])[names(pifs_no_disease_ylds[[index]]) == var_name_ylds] <- "pif"
        
        
        index <- index + 1
        
      }
    }
  }
}

### Run non-disease life tables

non_disease_list <- list()
index <- 1


for (age in i_age_cohort) {
  for (sex in i_sex) {
    for (d in 1:nrow(disease_short_names)){
      
      ## Exclude chronic disease and all-cause mortality and  pyld
      if (disease_short_names$is_not_dis[d] != 1 || disease_short_names$acronym[d] == "other") {
      }
      else {
        non_disease_list[[index]] <-  RunNonDisease (mslt_df, in_sex = sex, in_mid_age = age, in_non_disease = disease_short_names$acronym[d])
        
        
        ## deaths sceanario
        non_disease_list[[index]][paste0("deaths_rate_sc_", disease_short_names$acronym[d])] <-
          non_disease_list[[index]][paste0("deaths_rate_", disease_short_names$acronym[d])] * (1 - pifs_no_disease_deaths[[index]]$pif)
        
        
        ## ylds scenario
        non_disease_list[[index]][paste0("ylds_rate_sc_", disease_short_names$acronym[d])] <-
          non_disease_list[[index]][paste0("ylds_rate_", disease_short_names$acronym[d])] * (1 - pifs_no_disease_ylds[[index]]$pif)
        
        
        ## Difference variable
        
        ## deaths difference
        non_disease_list[[index]][paste0("deaths_rate_diff_", disease_short_names$acronym[d])] <- non_disease_list[[index]][paste0("deaths_rate_", disease_short_names$acronym[d])] -
          non_disease_list[[index]][paste0("deaths_rate_sc_", disease_short_names$acronym[d])]
        ## ylds difference
        non_disease_list[[index]][paste0("ylds_rate_diff_", disease_short_names$acronym[d])] <- non_disease_list[[index]][paste0("ylds_rate_", disease_short_names$acronym[d])] -
          non_disease_list[[index]][paste0("ylds_rate_sc_", disease_short_names$acronym[d])]
        
        names(non_disease_list)[index] <- paste(age, sex, disease_short_names$acronym[d], sep = "_")
        
        index <- index + 1
        
      }
    }
  }  
}

# ---- chunk-6 ----

## Create scenario disease life tables. 



### Create scenario life tables with new pifs 

disease_life_table_list_sc <- list()
index <- 1


for (iage in i_age_cohort){
  for (isex in i_sex){
    for (d in 1:nrow(disease_short_names)){
      
      # Exclude non-males diseases
      # Exclude non-males diseases
      if ((sex == "male" && disease_short_names$disease[d] == "breast cancer") || (sex == "male" && disease_short_names$disease[d] == "uterine cancer")) {
      }
      ## Exclude non-chronic diseases and road injuries and disease with no pif
      if (disease_short_names$is_not_dis[d] != 0 || disease_short_names$acronym[d] == "no_pif" || disease_short_names$acronym[d] == "other"){
      }
      else {

        
        # i_age_cohort <- 27
        # i_sex <- "male"
        # d <- 3
        # 
        td1 <- mslt_df
        
        pif_disease <- filter(pif_expanded, age >= iage & sex == isex) %>% 
          select(age, sex, contains(disease_short_names$acronym[disease_short_names$sname == disease_short_names$sname[d]]))
        
         
        #[td1$age >= i_age_cohort & td1$sex == i_sex,]
        
        td1[td1$age >= iage & td1$sex == isex,][[paste("incidence", disease_short_names$sname[d], sep = "_")]] <- 
          td1[td1$age >= iage & td1$sex == isex,][[paste("incidence", disease_short_names$sname[d], sep = "_")]] * (1 - (pif_disease %>% as.data.frame())[,3])
        
        td1[is.na(td1)] <- 0
        
      
          
        # # Instead of idata, feed td to run scenarios
        disease_life_table_list_sc[[index]] <- RunDisease(in_idata = td1, in_sex = isex,
                                                           in_mid_age = iage, in_disease = disease_short_names$sname[d])
        
        
        names(disease_life_table_list_sc)[index] <- paste(age, sex, disease_short_names$sname[d], sep = "_")
        
        
        
        disease_life_table_list_sc[[index]]$diff_inc_disease <-
          disease_life_table_list_sc[[index]]$incidence_disease -   disease_life_table_list_bl[[index]]$incidence_disease
        disease_life_table_list_sc[[index]]$diff_prev_disease <-
          disease_life_table_list_sc[[index]]$px  - disease_life_table_list_bl[[index]]$px
        disease_life_table_list_sc[[index]]$diff_mort_disease <-
          disease_life_table_list_sc[[index]]$mx - disease_life_table_list_bl[[index]]$mx
        disease_life_table_list_sc[[index]]$diff_pylds_disease <-
          (disease_life_table_list_sc[[index]]$px - disease_life_table_list_bl[[index]]$px) * disease_life_table_list_bl[[index]]$dw_disease
        index <- index + 1
       }
    }
  }
}
## Uncommnet to check scenario life tables
# View(disease_life_table_list_sc[[3]])

# ---- chunk-8 ----



# ---- chunk-9 ---- ADD non-diseases and diabetes (when done)

## Generate total change in mortality rate

## Sum mortality rate change scenarios (mx_sc_total) (ONLY DOING MALES)

mx_sc_total <- list()
l_index <- 1
index <- 1
for (age in i_age_cohort){
  for (sex in i_sex){
    mortality_sum <- NULL
    create_new <- T
    
    for (d in 1:nrow(disease_short_names)) {
      if ((sex == "male" && disease_short_names$disease[d] == "breast cancer") || (sex == "male" && disease_short_names$disease[d] == "uterine cancer")) {
      }
      ## Exclude non-chronic diseases and road injuries and disease with no pif
      if (disease_short_names$is_not_dis[d] != 0 || disease_short_names$acronym[d] == "no_pif" || disease_short_names$acronym[d] == "other"){
      }
      else {
        
        if (create_new){
          mortality_sum <- select(disease_life_table_list_sc[[index]],
                                  c('age', 'sex'))
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
    mx_sc_total[[l_index]] <- mortality_sum
    
    l_index <- l_index + 1
  }
}

## Uncommnet to check sceanrio mortality and changes
# View(mx_sc_total[[16]])

## Generate total change in prevalent yld rates
## Total ylds rate= sum (change prevalence disease*dw)

pylds_sc_total <- list()
l_index <- 1
index <- 1
for (age in i_age_cohort){
  for (sex in i_sex){
    pylds_sum <- NULL
    create_new <- T
    
    for (d in 1:nrow(disease_short_names)) {
      if ((sex == "male" && disease_short_names$disease[d] == "breast cancer") || (sex == "male" && disease_short_names$disease[d] == "uterine cancer")) {
      }
      ## Exclude non-chronic diseases and road injuries and disease with no pif
      if (disease_short_names$is_not_dis[d] != 0 || disease_short_names$acronym[d] == "no_pif" || disease_short_names$acronym[d] == "other"){
      }
      else {
        
        if (create_new){
          pylds_sum <- select(disease_life_table_list_sc[[index]], c('age', 'sex'))
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
    pylds_sc_total[[l_index]] <- pylds_sum
    l_index <- l_index + 1
  }
}

## Uncommnet to check scenario pyld change
# View(pylds_sc_total[[2]])

## Calculate general life tables with modified mortality and pylds total
## Original mortality rate is modified by the mx_sc_total (total change in mortality from diseases)
## Original pyld rate is modified by the change in each disease pylds

general_life_table_list_sc <- list()
index <- 1


for (age in i_age_cohort){
  for (sex in i_sex){
    
    
    # cat("age ", age, " and sex ", sex, "\n")
    # modify idata's mortality and pyld total for the said scenario
    td2 <- mslt_df
    # td2 <- subset(td2, select = -c(mx, pyld_rate))
    td2[td2$age >= age & td2$sex == sex,][[paste("mx")]] <- general_life_table_list_bl[[index]]$mx + mx_sc_total[[index]]$total
    td2[td2$age >= age & td2$sex == sex,][[paste("pyld_rate")]] <- general_life_table_list_bl[[index]]$pyld_rate + pylds_sc_total[[index]]$total
    
    
    # Instead of idata, feed td to run scenarios
    general_life_table_list_sc[[index]] <- RunLifeTable(in_idata = td2, in_sex = sex, in_mid_age = age)
    #
    
    
    index <- index + 1
  }
}

# ---- chunk-12 ----

## In the following list "output_life_table", 32 data frames are nested per age and sex cohort

output_burden <- list()
l_index <- 1
index <- 1
for (age in i_age_cohort){
  for (sex in i_sex){
    
    # Males do not have breast cancer, that is why we need the if/else.
    # We create a TRUE/FALSE variable for the loop to move into the next disease
    
    create_new <- T
    for (d in 1:nrow(disease_short_names)) {
      if ((sex == "male" && disease_short_names$disease[d] == "breast cancer") || (sex == "male" && disease_short_names$disease[d] == "uterine cancer")) {
      }
      ## Exclude non-chronic diseases and road injuries and disease with no pif
      if (disease_short_names$is_not_dis[d] != 0 || disease_short_names$acronym[d] == "no_pif" || disease_short_names$acronym[d] == "other"){
      }
      else {
        
        if (create_new){
          output_burden_sc <- select(disease_life_table_list_sc[[index]],
                                     c('age', 'sex', 'incidence_disease', 'mx', 'px'))
          names(output_burden_sc)[names(output_burden_sc) == 'incidence_disease'] <-
            paste('incidence_disease', disease_short_names$sname[d], "sc", sep = "_")
          names(output_burden_sc)[names(output_burden_sc) == 'mx'] <-
            paste('mx', disease_short_names$sname[d], "sc", sep = "_")
          names(output_burden_sc)[names(output_burden_sc) == 'px'] <-
            paste('px', disease_short_names$sname[d], "sc", sep = "_")
          output_burden_bl <- select(disease_life_table_list_bl[[index]],
                                     c('incidence_disease', 'mx', 'px'))
          names(output_burden_bl)[names(output_burden_bl) == 'incidence_disease'] <-
            paste('incidence_disease', disease_short_names$sname[d], "bl", sep = "_")
          names(output_burden_bl)[names(output_burden_bl) == 'mx'] <-
            paste('mx', disease_short_names$sname[d], "bl", sep = "_")
          names(output_burden_bl)[names(output_burden_bl) == 'px'] <-
            paste('px', disease_short_names$sname[d], "bl", sep = "_")
          
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
            paste('inc_num_bl', disease_short_names$sname[d], sep = "_")
          names(output_burden_change)[names(output_burden_change) == 'inc_num_sc'] <-
            paste('inc_num_sc', disease_short_names$sname[d], sep = "_")
          names(output_burden_change)[names(output_burden_change) == 'inc_num_diff'] <-
            paste('inc_num_diff', disease_short_names$sname[d], sep = "_")
          names(output_burden_change)[names(output_burden_change) == 'mx_num_bl'] <-
            paste('mx_num_bl', disease_short_names$sname[d], sep = "_")
          names(output_burden_change)[names(output_burden_change) == 'mx_num_sc'] <-
            paste('mx_num_sc', disease_short_names$sname[d], sep = "_")
          names(output_burden_change)[names(output_burden_change) == 'mx_num_diff'] <-
            paste('mx_num_diff', disease_short_names$sname[d], sep = "_")
          
          ## Bind all lists
          
          output_burden_sc <- cbind(output_burden_sc, output_burden_bl)
          output_burden_sc <- cbind(output_burden_sc, output_burden_change)
          
          create_new <- F
          
          ## Here the calculations above are repeated, here is where the F is telling to move into the next disease
          
        }else{
          
          td3 <- select(disease_life_table_list_sc[[index]],
                        c('incidence_disease', 'mx', 'px'))
          names(td3)[names(td3) == 'incidence_disease'] <-
            paste('incidence_disease', disease_short_names$sname[d], "sc", sep = "_")
          names(td3)[names(td3) == 'mx'] <-
            paste('mx', disease_short_names$sname[d], "sc", sep = "_")
          names(td3)[names(td3) == 'px'] <-
            paste('px', disease_short_names$sname[d], "sc", sep = "_")
          
          td4 <- select(disease_life_table_list_bl[[index]],
                        c('incidence_disease', 'mx', 'px'))
          names(td4)[names(td4) == 'incidence_disease'] <-
            paste('incidence_disease', disease_short_names$sname[d], "bl", sep = "_")
          names(td4)[names(td4) == 'mx'] <-
            paste('mx', disease_short_names$sname[d], "bl", sep = "_")
          names(td4)[names(td4) == 'px'] <-
            paste('px', disease_short_names$sname[d], "bl", sep = "_")
          
          output_burden_change2 <- list()
          
          output_burden_change2$inc_num_bl <- disease_life_table_list_bl[[index]]$incidence_disease * (1 - disease_life_table_list_bl[[index]]$px) * general_life_table_list_bl[[l_index]]$Lx
          output_burden_change2$inc_num_sc <- disease_life_table_list_sc[[index]]$incidence_disease * (1 - disease_life_table_list_sc[[index]]$px) * general_life_table_list_sc[[l_index]]$Lx
          output_burden_change2$inc_num_diff <- (disease_life_table_list_sc[[index]]$incidence_disease * (1 - disease_life_table_list_sc[[index]]$px) * general_life_table_list_sc[[l_index]]$Lx) - (disease_life_table_list_bl[[index]]$incidence_disease * (1 - disease_life_table_list_bl[[index]]$px) * general_life_table_list_bl[[l_index]]$Lx)
          
          output_burden_change2$mx_num_bl <- disease_life_table_list_bl[[index]]$mx * general_life_table_list_bl[[l_index]]$Lx
          output_burden_change2$mx_num_sc <- disease_life_table_list_sc[[index]]$mx * general_life_table_list_sc[[l_index]]$Lx
          output_burden_change2$mx_num_diff <- (disease_life_table_list_sc[[index]]$mx * general_life_table_list_sc[[l_index]]$Lx) - (disease_life_table_list_bl[[index]]$mx * general_life_table_list_bl[[l_index]]$Lx)
          
          names(output_burden_change2)[names(output_burden_change2) == 'inc_num_bl'] <-
            paste('inc_num_bl', disease_short_names$sname[d], sep = "_")
          names(output_burden_change2)[names(output_burden_change2) == 'inc_num_sc'] <-
            paste('inc_num_sc', disease_short_names$sname[d], sep = "_")
          names(output_burden_change2)[names(output_burden_change2) == 'inc_num_diff'] <-
            paste('inc_num_diff', disease_short_names$sname[d], sep = "_")
          names(output_burden_change2)[names(output_burden_change2) == 'mx_num_bl'] <-
            paste('mx_num_bl', disease_short_names$sname[d], sep = "_")
          names(output_burden_change2)[names(output_burden_change2) == 'mx_num_sc'] <-
            paste('mx_num_sc', disease_short_names$sname[d], sep = "_")
          names(output_burden_change2)[names(output_burden_change2) == 'mx_num_diff'] <-
            paste('mx_num_diff', disease_short_names$sname[d], sep = "_")
          
          
          ## Bind all lists
          
          output_burden_sc <- cbind(output_burden_sc, td3)
          output_burden_sc <- cbind(output_burden_sc, td4)
          output_burden_sc$age_cohort <- age
          output_burden_sc <- cbind(output_burden_sc, output_burden_change2)
          
        }
        
        # cat(age, " - ", sex," - ",  disease," - ",  index, " - ", l_index,  "\n")
        index <- index + 1
      }
      
    }
    
    ## general_life_table_list_sc and general_life_table_list_bl (Lx)
    output_burden_lf_sc <- select(general_life_table_list_sc[[l_index]], c('Lx', 'Lwx'))
    names(output_burden_lf_sc)[names(output_burden_lf_sc) == 'Lx'] <- paste('Lx', "sc", sep = "_")
    names(output_burden_lf_sc)[names(output_burden_lf_sc) == 'Lwx'] <- paste('Lwx', "sc", sep = "_")
    
    output_burden_lf_bl <- select(general_life_table_list_bl[[l_index]], c('Lx', 'Lwx'))
    names(output_burden_lf_bl)[names(output_burden_lf_bl) == 'Lx'] <- paste('Lx', "bl", sep = "_")
    names(output_burden_lf_bl)[names(output_burden_lf_bl) == 'Lwx'] <- paste('Lwx', "bl", sep = "_")
    
    
    output_burden_lf_sc$Lx_diff <- general_life_table_list_bl[[l_index]]$Lx - general_life_table_list_sc[[l_index]]$Lx
    output_burden_lf_sc$Lwx_diff <- general_life_table_list_bl[[l_index]]$Lwx - general_life_table_list_sc[[l_index]]$Lwx
    
    output_burden_sc <- cbind(output_burden_sc, output_burden_lf_sc)
    output_burden_sc <- cbind(output_burden_sc, output_burden_lf_bl)
    
    
    output_burden[[l_index]] <- output_burden_sc
    l_index <- l_index + 1
    
  }
}

## Uncomment to check

# View(output_burden[[3]])


# ---- chunk-13 ----

#####Generate a data frame for all results and create function to get outcomes.

output_df <- plyr::ldply(output_burden, rbind)

# View(output_df)

##### UP TO HERE

# ---- chunk- 14 ----  TO DO, presentaiton of results. 

# Generate graphs change in mortality and incidence numbers by age/sex. (NEED TO ADJUST THIS CODE TO MORE DISEASES)
# The code is only set up for the 5 original PA related diseases. 

output_dir = "MSLT/output"

# i_age_cohort <- c(22, 27, 32, 37, 42, 47, 52, 57, 62, 67, 72, 77, 82, 87, 92, 97)
# i_sex <- c("male", "female")
i_measure <- c("deaths", "ylds") #" (years lived with disability)")
i_outcome <- c("mx", "inc")
output_dir <- "MSLT/output"


i_outcome <- c("mx", "inc")
p_list_male <- list()
p_list_female <- list()
male_index <- 1
female_index <- 1
for (age in i_age_cohort){
  for (sex in i_sex) {
    for (outcome in i_outcome) {
      for (disease in i_disease){
        
        if (sex == "male" && disease == "bc"){
          # cat("\n") #Uncomment to see list
        }
        if (sex == "male" && disease == "uc"){
          # cat("\n") #Uncomment to see list
        }
        if (sex == "female" && disease == "pc"){
          # cat("\n") #Uncomment to see list
        }
        else {
          
          p_index  <- PlotOutput(in_data = output_df, in_age = age, in_population = sex, in_outcomes = c("age", paste(outcome, "num", "bl", disease, sep = "_"), paste(outcome, "num", "sc", disease, sep = "_"), paste(outcome, "num", "diff", disease, sep = "_")), in_disease = GetQualifiedDiseaseName(disease))
          
          if (sex == "male"){
            
            p_list_male[[male_index]] <- p_index
            
            if (male_index %% 4 == 0 && male_index > 0){
              
              p1 <- p_list_male[[male_index - 3]] + theme(legend.position="none", axis.title.x = element_blank(),  axis.title.y = element_blank())
              p2 <- p_list_male[[male_index - 2]] + theme(legend.position="none", axis.title.x = element_blank(),  axis.title.y = element_blank())
              p3 <- p_list_male[[male_index - 1]] + theme(legend.position="none", axis.title.x = element_blank(),  axis.title.y = element_blank())
              p4 <- p_index + theme(legend.position="none", axis.title.x = element_blank(),  axis.title.y = element_blank())
              
              jpeg(
                (output_dir, "/", paste(age, sex, outcome, sep="_"), ".jpeg"))
              GridArrangSharedLegend (p1, p2, p3, p4, ncol = 2, nrow = 2, mainTitle = paste(ifelse(outcome == "mx", "Deaths", "Incidence"), sex, "cohort mid age", age),
                                          mainLeft = 'Cases', mainBottom = 'Age')
              dev.off()
              
            }
            
            male_index <- male_index + 1
            
          }
          
          if (sex == "female" && female_index > 0){
            p_list_female[[female_index]] <- p_index
            
            if (female_index %% 5 == 0){
              p1 <- p_list_female[[female_index - 4]] + theme(legend.position="none", axis.title.x = element_blank(),  axis.title.y = element_blank())
              p2 <- p_list_female[[female_index - 3]] + theme(legend.position="none", axis.title.x = element_blank(),  axis.title.y = element_blank())
              p3 <- p_list_female[[female_index - 2]] + theme(legend.position="none", axis.title.x = element_blank(),  axis.title.y = element_blank())
              p4 <- p_list_female[[female_index - 1]] + theme(legend.position="none", axis.title.x = element_blank(),  axis.title.y = element_blank())
              p5 <- p_index + theme(legend.position="none", axis.title.x = element_blank(),  axis.title.y = element_blank())
              
              jpeg(
                (output_dir, "/", paste(age, sex, outcome, sep="_"), ".jpeg"))
              GridArrangSharedLegend (p1, p2, p3, p4, p5, ncol = 2, nrow = 3, mainTitle = paste(ifelse(outcome == "mx", "Deaths", "Incidence"), sex, "cohort mid age", age), mainLeft = 'Cases', mainBottom = 'Age')
              dev.off()
              
            }
            female_index <- female_index + 1
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
    aggregate_frame_males[[index]] <- aggregate_frame_males[[index]] %>% select(-starts_with("age"), -starts_with("sex"))
    
    aggregate_frame_females[[index]] <- aggregate_frame_females[[index]] %>% select(-starts_with("age"), -starts_with("sex"))
    
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
  
  
  aggregate_frame_males2[[i]] <- aggregate_frame_males2[[i]] %>% select(-starts_with("age"), -starts_with("sex"))
  
  aggregate_frame_females2[[i]] <- aggregate_frame_females2[[i]] %>% select(-starts_with("age"), -starts_with("sex"))
  
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







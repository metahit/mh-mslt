### Function to run mslt and generate outputs
RunMSLT <- function(mslt_df, i_sex, i_age_cohort, disease_names, pif) {
  
  
  ### Functions requiered 
  source(paste0(relative_path_mslt, "/R/functions_MSLT.R"))
  # mslt_df=read_csv("~/mh-execute/inputs/mslt/bristol_mslt.csv")
  # disease_names=readRDS("~/mh-mslt/output/parameters/DISEASE_SHORT_NAMES.rds")
  # i_sex=c("male", "female")
  # i_age_cohort=seq(from=17, to=97, by =5)
  # pif=read_csv("~/mh-mslt/input/pif_place_holder.csv") ## PLACE HOLDER (from Aus)

  # 
  # ### Relative risks diabetes
  DIABETES_IHD_RR_F <- 2.82 ## c(2.82, CI (2.35, 3.38) get SD from CI
  DIABETES_STROKE_RR_F <- 2.28 ## c(2.28) CI (1.93, 2.69) get SD from CI
  DIABETES_IHD_RR_M <- 2.16 ## c(2.16, CI (1.82, 2.56) get SD from CI
  DIABETES_STROKE_RR_M <- 1.83 ## c(1.83) CI (1.60, 2.08) get SD from CI
  
  DISEASE_SHORT_NAMES <- disease_names
  
  
  ### USE Australian pifs for diseases as not ready for city regions
  ### Get pif, generated in mh-execute and saved in input mh-mslt
  pif_expanded <- pif %>%
    mutate(pif_cyclist_deaths=1.1,
           pif_pedestrian_deaths=1.1,
           pif_cyclist_ylds=1.1,
           pif_pedestrian_ylds=1.1,
           pif_motor_deaths=1.1,
           pif_motorcyclist_deaths=1.1,
           pif_motor_ylds=1.1,
           pif_motorcyclist_ylds=1.1,
           pif_road_deaths=1.1,
           pif_road_ylds=1.1,
           pif_lri_ylds=0.03,
           pif_lri_deaths=0.03,
           pif_copd=0.03) %>%
    dplyr::slice(rep(1:dplyr::n(), each = 5)) %>% ### expand to 1-yr group (repeat values)
    mutate(age=rep(seq(16,100,1), times = 2))

  
  # dataframe of the age and sex cohorts (crossing just does a cross product) for loop below
  age_sex_cohorts <- crossing(data.frame(age=i_age_cohort),
                              data.frame(sex=c('male', 'female'))) %>%
    dplyr::mutate(cohort=paste0(age,"_",sex))
  
  ### Run baseline general dataframes
  general_life_table_list_bl <- list()
  
  
  for (i in 1:nrow(age_sex_cohorts)){
    suppressWarnings(
      general_life_table_list_bl[[i]] <- RunLifeTable(
        in_idata    = mslt_df,
        in_sex      = age_sex_cohorts$sex[i],
        in_mid_age  = age_sex_cohorts$age[i],
        death_rates = NA ## mortality trends data if available, not for now. We would need trends for each city region.
      ))
    names(general_life_table_list_bl)[i] <- age_sex_cohorts$cohort[i]
  }
  
  # convert the list of dataframes to single dataframes
  general_life_table_bl <- bind_rows(general_life_table_list_bl, .id = "age_group") %>%
    mutate(age_group = as.numeric(gsub("_.*","",age_group)))
  
  ### Run disease life tables
  
  disease_cohorts <- DISEASE_SHORT_NAMES %>%
    # Exclude non-diseases, road injuries, and diseases with no pif
    dplyr::filter(is_not_dis == 0 & acronym != 'no_pif' & acronym != 'other' ) %>%
    dplyr::select(sname,acronym,males,females)
  
  # adding the age and sex cohorts:
  age_sex_disease_cohorts <- crossing(age_sex_cohorts,disease_cohorts) %>%
    mutate(cohort=paste0(age,'_',sex,'_',sname)) %>%
    # Exclude non-male diseases (and non-female if there were any)
    filter( (sex=='male' & males==1) | (sex=='female' & females==1)) %>%
    dplyr::select(age,sex,sname,acronym,cohort) %>%
    # ishd and strk have the prerequisite disease dmt2
    mutate(prerequsite=ifelse(sname %in% c("ishd","strk"),paste0(age,"_",sex,"_dmt2"),0)) %>%
    # ensuring prequisites are calculated first
    arrange(age,sex,prerequsite,sname)
  
  
  disease_life_table_list_bl <- list()
  
  for (i in 1:nrow(age_sex_disease_cohorts)){
    disease_life_table_list_bl[[i]] <- RunDisease(
      in_idata         = mslt_df,
      in_mid_age       = age_sex_disease_cohorts$age[i],
      in_sex           = age_sex_disease_cohorts$sex[i],
      in_disease       = age_sex_disease_cohorts$sname[i],
      incidence_trends = NA,
      mortality_trends = NA
    )
    names(disease_life_table_list_bl)[i] <- age_sex_disease_cohorts$cohort[i]
  }
  
  ### Run non diseases (injuries and lower respiratory disease)

  non_disease_cohorts <- DISEASE_SHORT_NAMES %>%
    # Exclude non-diseases, road injuries, and diseases with no pif
    dplyr::filter(is_not_dis == 1 & acronym != 'no_pif' & acronym != 'other' ) %>%
    dplyr::select(sname,acronym,males,females)
  
  # adding the age and sex cohorts:
  age_sex_non_disease_cohorts <- crossing(age_sex_cohorts,non_disease_cohorts) %>%
    mutate(cohort=paste0(age,'_',sex,'_',sname)) %>%
    dplyr::select(age,sex,sname,acronym,cohort)
  
  non_disease_life_table_list_bl <- list()
  
  for (i in 1:nrow(age_sex_non_disease_cohorts )){ 
    
    non_disease_life_table_list_bl[[i]] <- RunNonDisease(
      in_idata         = mslt_df,
      in_sex           = age_sex_non_disease_cohorts$sex[i],
      in_mid_age       = age_sex_non_disease_cohorts$age[i],
      in_non_disease   = age_sex_non_disease_cohorts$sname[i]
    )
    names(non_disease_life_table_list_bl)[i] <- age_sex_non_disease_cohorts$cohort[i]
  }  
  
  ### Run scenario disease life tables
  
  disease_relative_risks <- tribble(
    ~sex    , ~prerequsite, ~disease , ~relative_risk       ,
    "male"  ,  "dmt2"     ,  "ishd"  ,  DIABETES_IHD_RR_M   ,
    "female",  "dmt2"     ,  "ishd"  ,  DIABETES_IHD_RR_F   ,
    "male"  ,  "dmt2"     ,  "strk"  ,  DIABETES_STROKE_RR_M,
    "female",  "dmt2"     ,  "strk"  ,  DIABETES_STROKE_RR_F
  )
  
  disease_life_table_list_sc <- list()
  
  for (i in 1:nrow(age_sex_disease_cohorts)){
    # i=39
    td1_age_sex <- mslt_df %>% ### new mslt dataframe with modified incidence rates
      filter(age >= age_sex_disease_cohorts$age[i] & sex == age_sex_disease_cohorts$sex[i])
    
    pif_colname <- paste0('pif_',age_sex_disease_cohorts$acronym[i])
    
    pif_disease <- pif_expanded %>%
      filter(age >= age_sex_disease_cohorts$age[i] & sex == age_sex_disease_cohorts$sex[i]) %>%
      dplyr::select(age,sex,pif_colname)
    
    # adjustment for diabetes effect on ihd and stroke
    if(age_sex_disease_cohorts$prerequsite[i] != 0){
      # get name for pif column
      target_disease <- paste0("pif_",age_sex_disease_cohorts$acronym[i])
      # get prerequisite disease cohort name (i.e., age_sex_dmt2 for diabetes)
      dia_col <- age_sex_disease_cohorts$prerequsite[i]
      # select relative risk of disease given diabetes (depends on sex, not age)
      relative_risk <- disease_relative_risks %>%
        filter(sex == age_sex_disease_cohorts$sex[i] &
                 disease == age_sex_disease_cohorts$sname[i]) %>%
        pull(relative_risk)
      # (store old pif)
      # old_pif <- pif_disease[[target_disease]]
      # diabetes pif = - { scenario prevalence - baseline prevalence } * (RR - 1)  / { baseline prevalence * (RR - 1) + 1 }
      scenario_prevalence <- disease_life_table_list_sc[[dia_col]]$px
      baseline_prevalence <- disease_life_table_list_bl[[dia_col]]$px
      pif_dia <- -(scenario_prevalence - baseline_prevalence)*(relative_risk-1)/
        (baseline_prevalence * (relative_risk-1) + 1)
      # modify pif for target disease: new pif =  (1 - old pif) * (1 - diabetes pif)
      pif_disease[[target_disease]] <- 1- (1-pif_disease[[target_disease]]) * (1-pif_dia)
      # print(sum(old_pif-pif_disease[[target_disease]]))
    }
    
    incidence_colname <- paste0('incidence_', age_sex_disease_cohorts$sname[i])
    new_col <- td1_age_sex%>%pull(incidence_colname) * (1 - (pif_disease%>%pull(pif_colname)))
    new_col[is.na(new_col)] <- 0
    td1_age_sex[[incidence_colname]] <- new_col
    
    ## Instead of idata, feed td to run scenarios. Now all diseases are run again, with the effect of diabetes
    ## on cardiovascular diseases taken into account. 
    
    disease_life_table_list_sc[[i]] <- RunDisease(
      in_idata         = td1_age_sex,
      in_sex           = age_sex_disease_cohorts$sex[i],
      in_mid_age       = age_sex_disease_cohorts$age[i],
      in_disease       = age_sex_disease_cohorts$sname[i],
      incidence_trends = NA,
      mortality_trends = NA
    )
    names(disease_life_table_list_sc)[i] <- age_sex_disease_cohorts$cohort[i]
  }
  
  
  
  for (cohort in age_sex_disease_cohorts$cohort) {
    disease_life_table_list_sc[[cohort]]$diff_inc_disease <-
      disease_life_table_list_sc[[cohort]]$incidence_disease - disease_life_table_list_bl[[cohort]]$incidence_disease
    
    disease_life_table_list_sc[[cohort]]$diff_prev_disease <-
      disease_life_table_list_sc[[cohort]]$px - disease_life_table_list_bl[[cohort]]$px
    
    disease_life_table_list_sc[[cohort]]$diff_mort_disease <-
      disease_life_table_list_sc[[cohort]]$mx - disease_life_table_list_bl[[cohort]]$mx
    
    disease_life_table_list_sc[[cohort]]$diff_pylds_disease <-
      (disease_life_table_list_sc[[cohort]]$px - disease_life_table_list_bl[[cohort]]$px) * 
      (disease_life_table_list_bl[[cohort]]$dw_disease)
  }
  
  
  # convert the list of dataframes to single dataframes
  disease_life_table_bl <- bind_rows(disease_life_table_list_bl, .id = "age_sex_disease_cohort") %>%
    mutate(age_sex_disease_cohort = as.numeric(gsub("_.*","",age_sex_disease_cohort))) %>%
    dplyr::rename(age_group=age_sex_disease_cohort,
           cause=disease)
  
  disease_life_table_sc <- bind_rows(disease_life_table_list_sc, .id = "age_sex_disease_cohort") %>%
    mutate(age_sex_disease_cohort = as.numeric(gsub("_.*","",age_sex_disease_cohort))) %>%
    dplyr::rename(age_group=age_sex_disease_cohort,
           cause=disease)


  ### Run scenario non diseases

non_disease_life_table_list_sc <- list()

for (i in 1:nrow(age_sex_non_disease_cohorts)){
  # i=6
  td1_age_sex <- mslt_df %>% ### new mslt dataframe with modified injuries and lri deaths and ylds rates
    filter(age >= age_sex_non_disease_cohorts$age[i] & sex == age_sex_non_disease_cohorts$sex[i])
  
  pif_colname_deaths <- paste0('pif_',age_sex_non_disease_cohorts$acronym[i], '_deaths')
  pif_colname_ylds <- paste0('pif_',age_sex_non_disease_cohorts$acronym[i], '_ylds')
  
  pif_non_disease <- pif_expanded %>%
    filter(age >= age_sex_non_disease_cohorts$age[i] & sex == age_sex_non_disease_cohorts$sex[i]) %>%
    dplyr::select(age,sex,pif_colname_deaths,pif_colname_ylds)
  
  
  death_colname <- paste0('deaths_rate_', age_sex_non_disease_cohorts$sname[i])
  ylds_colname <- paste0('ylds_rate_', age_sex_non_disease_cohorts$sname[i])
  
  
  new_deaths <- td1_age_sex%>%pull(death_colname) * (1 - (pif_non_disease%>%pull(pif_colname_deaths)))
  new_deaths[is.na(new_deaths)] <- 0
  
  new_ylds <- td1_age_sex%>%pull(ylds_colname) * (1 - (pif_non_disease%>%pull(pif_colname_ylds)))
  new_ylds[is.na(new_ylds)] <- 0
  
  td1_age_sex[[death_colname]] <- new_deaths
  td1_age_sex[[ylds_colname]] <- new_ylds
  
  ## Instead of idata, feed td to run scenarios. Now all non_diseases are run again
  
  
  non_disease_life_table_list_sc[[i]] <- RunNonDisease(
    in_idata         = td1_age_sex,
    in_sex           = age_sex_non_disease_cohorts$sex[i],
    in_mid_age       = age_sex_non_disease_cohorts$age[i],
    in_non_disease   = age_sex_non_disease_cohorts$sname[i]
  )
  names(non_disease_life_table_list_sc)[i] <- age_sex_non_disease_cohorts$cohort[i]
  
}

### Difference rates

for (cohort in age_sex_non_disease_cohorts$cohort) {
  non_disease_life_table_list_sc[[cohort]]$diff_mort <-
    non_disease_life_table_list_sc[[cohort]]$deaths_rate -  non_disease_life_table_list_bl[[cohort]]$deaths_rate
  
  non_disease_life_table_list_sc[[cohort]]$diff_pylds <-
    non_disease_life_table_list_sc[[cohort]]$ylds_rate -  non_disease_life_table_list_bl[[cohort]]$ylds_rate }



# convert the list of dataframes to single dataframes
non_disease_life_table_bl <- bind_rows(non_disease_life_table_list_bl, .id = "age_sex_non_disease_cohort") %>%
  mutate(age_sex_non_disease_cohort = as.numeric(gsub("_.*","",age_sex_non_disease_cohort))) %>%
  dplyr::rename(age_group=age_sex_non_disease_cohort,
         cause=non_disease,
         mx=deaths_rate)

non_disease_life_table_sc <- bind_rows(non_disease_life_table_list_sc, .id = "age_sex_non_disease_cohort") %>%
  mutate(age_sex_non_disease_cohort = as.numeric(gsub("_.*","",age_sex_non_disease_cohort))) %>%
  dplyr::rename(age_group=age_sex_non_disease_cohort,
         cause=non_disease,
         mx=deaths_rate)

  ### Add up mortality and pylds changes

### Diseases: Sum mortality rate and pylds change scenarios
mx_pylds_sc_total_disease_df <- disease_life_table_sc %>%
  group_by(age_group,sex,age) %>%
  dplyr::summarise(mortality_sum=sum(diff_mort_disease,na.rm=T),
                   pylds_sum=sum(diff_pylds_disease,na.rm=T)) %>%
  ungroup() %>%
  mutate(age_sex_cohort=paste0(age_group,'_',sex))

### Non-diseases
mx_pylds_sc_total_non_disease_df <- non_disease_life_table_sc %>%
  group_by(age_group,sex,age) %>%
  dplyr::summarise(mortality_sum=sum(diff_mort,na.rm=T),
                   pylds_sum=sum(diff_pylds,na.rm=T)) %>%
  ungroup() %>%
  mutate(age_sex_cohort=paste0(age_group,'_',sex))


### Run scenario life tables

general_life_table_list_sc <- list()

for (i in 1:nrow(age_sex_cohorts)){
  # modify idata's mortality and pyld total for the said scenario
  mx_pylds_sc_total_disease_df_cohort <- mx_pylds_sc_total_disease_df %>%
    filter(age_sex_cohort==age_sex_cohorts$cohort[i]) %>%
    dplyr::select(age,mortality_sum,pylds_sum)
  
  mx_pylds_sc_total_non_disease_df_cohort <- mx_pylds_sc_total_non_disease_df %>%
    filter(age_sex_cohort==age_sex_cohorts$cohort[i]) %>%
    dplyr::select(age,mortality_sum,pylds_sum)
  
  ### Modify rates in static MSLT  (pylds are always static, mx can include future trends)
  #### With diseases changes in mortality and ylds
  td2 <- mslt_df %>%
    filter(sex==age_sex_cohorts$sex[i]) %>%
    left_join(mx_pylds_sc_total_disease_df_cohort,by="age") %>%
    mutate(mx=mx+replace_na(mortality_sum,0),
           pyld_rate=pyld_rate+replace_na(pylds_sum,0)) %>%
    dplyr::select(-mortality_sum,-pylds_sum)
  
  #### With diseases changes in mortality and ylds
  td3 <-  td2 %>%
    filter(sex==age_sex_cohorts$sex[i]) %>%
    left_join(mx_pylds_sc_total_non_disease_df_cohort,by="age") %>%
    mutate(mx=mx+replace_na(mortality_sum,0),
           pyld_rate=pyld_rate+replace_na(pylds_sum,0)) %>%
    dplyr::select(-mortality_sum,-pylds_sum)
  
  # ### Modify death rates with future trends NOT AVAILABLE FOR METAHIT
  # td3 <- death_projections %>%
  #   mutate(cohort=paste(age_cohort, sex, sep = "_")) %>% # variable to match change in mortality rates df
  #   filter(cohort==age_sex_cohorts$cohort[i]) %>%
  #   left_join(mx_pylds_sc_total_disease_df_cohort) %>%
  #   mutate(rate=rate+replace_na(mortality_sum,0))%>%
  #   dplyr::select(-mortality_sum,-pylds_sum)   
  
  
  
  suppressWarnings(
    general_life_table_list_sc[[i]] <- RunLifeTable(
      in_idata    = td3,
      in_sex      = age_sex_cohorts$sex[i],
      in_mid_age  = age_sex_cohorts$age[i],
      death_rates = NA
    ))
  names(general_life_table_list_sc)[i] <- age_sex_cohorts$cohort[i]
}

# convert the list of dataframes to single dataframes
general_life_table_sc <- bind_rows(general_life_table_list_sc, .id = "age_group") %>%
  mutate(age_group = as.numeric(gsub("_.*","",age_group)))


## In the following list 'output_life_table', 34 data frames are nested per age and sex cohort
## Outputs are generated following the index order of disease life tables baseline and scenarios where ##diabetes is     firstcalculated as it impacts on cardiovascular diseases. 

### Generate outputs dataframe
#### Diseases life tables
dia_index <- which(DISEASE_SHORT_NAMES$sname=='dmt2')
dia_order <- c(dia_index,c(1:nrow(DISEASE_SHORT_NAMES))[-dia_index])

### Combine diseases and general life tables for scenarios
### Step needed to calculate numbers (rates*people cohort)
scenario_d <- inner_join(disease_life_table_sc %>%
                           dplyr::select(age_group,sex,age,cause,incidence_disease,mx,px),
                         general_life_table_sc %>%
                           dplyr::select(age_group,sex,age,Lx,ex,Lwx,ewx),
                         by=c("age","sex","age_group")) %>%
  mutate(intervention="sc")

baseline_d <- inner_join(disease_life_table_bl %>%
                           dplyr::select(age_group,sex,age,cause,incidence_disease,mx,px),
                         general_life_table_bl %>%
                           dplyr::select(age_group,sex,age,Lx,ex,Lwx,ewx),
                         by=c("age","sex","age_group")) %>%
  mutate(intervention="bl")

disease_combined <- bind_rows(scenario_d,baseline_d) %>%
  pivot_wider(names_from  = intervention,
              values_from = c(incidence_disease,mx,px,Lx,ex,Lwx,ewx))  %>%
  mutate(inc_num_bl   = incidence_disease_bl*(1-px_bl)*Lx_bl,
         inc_num_sc   = incidence_disease_sc*(1-px_sc)*Lx_sc,
         inc_num_diff = inc_num_sc-inc_num_bl,
         mx_num_bl    = mx_bl*Lx_bl,
         mx_num_sc    = mx_sc*Lx_sc,
         mx_num_diff  = mx_num_sc-mx_num_bl) %>%
  dplyr::select(c(-mx_sc, -mx_bl, -px_sc, -px_bl, -Lx_sc, -Lx_bl, -ex_sc, -ex_bl, -Lwx_sc, -Lwx_bl, -ewx_sc, -ewx_bl)) %>%    dplyr::select(-starts_with(c( "incidence_"))) %>%
  pivot_wider(names_from  = cause,
              values_from = inc_num_bl:mx_num_diff)


### Non_diseases life tables

scenario_nd <- inner_join(non_disease_life_table_sc %>%
                            dplyr::select(age_group,sex,age,cause,mx,ylds_rate, diff_mort, diff_pylds),
                          general_life_table_sc %>%
                            dplyr::select(age_group,sex,age,Lx),
                          by=c("age","sex","age_group")) %>%
  mutate(intervention="sc")

baseline_nd <- inner_join(non_disease_life_table_sc %>%
                            dplyr::select(age_group,sex,age,cause,mx,ylds_rate, diff_mort, diff_pylds),
                          general_life_table_bl %>%
                            dplyr::select(age_group,sex,age,Lx),
                          by=c("age","sex","age_group")) %>%
  mutate(intervention="bl")

non_disease_combined <- bind_rows(scenario_nd,baseline_nd) %>%
  pivot_wider(names_from  = intervention,
              values_from = c(mx, ylds_rate, Lx,
                              values_fill=0)) %>%
  mutate(ylds_num_bl   = ylds_rate_bl*Lx_bl,
         ylds_num_sc   = ylds_rate_sc*Lx_sc,
         ylds_num_diff = ylds_num_sc-ylds_num_bl,
         mx_num_bl    = mx_bl*Lx_bl,
         mx_num_sc    = mx_sc*Lx_sc,
         mx_num_diff  = mx_num_sc-mx_num_bl) %>%
  dplyr::select(c(cause, sex, age, age_group, ylds_num_bl, ylds_num_sc, ylds_num_diff, mx_num_bl, mx_num_sc, mx_num_diff)) %>%  
  pivot_wider(names_from  = cause,
              values_from = ylds_num_bl:mx_num_diff)

### General life tables

general_lf <- bind_rows(
  general_life_table_sc %>%
    dplyr::select(age_group,sex,age,Lx,ex,Lwx,ewx) %>%
    mutate(intervention="sc"),
  general_life_table_bl %>%
    dplyr::select(age_group,sex,age,Lx,ex,Lwx,ewx) %>%
    mutate(intervention="bl")) %>%
  pivot_wider(names_from  = intervention,
              values_from = c(Lx,ex,Lwx,ewx), 
              values_fill=0) %>%
  mutate(Lx_diff  = Lx_sc-Lx_bl,
         Lwx_diff = Lwx_sc-Lwx_bl,
         ex_diff  = ex_sc-ex_bl,
         ewx_diff = ewx_sc-ewx_bl)


######## Dataframe with all outputs by age and sex cohort over the simulation years (years of the cohort)
output_df <- inner_join(disease_combined,
                        general_lf,
                        by=c("age","sex","age_group")) %>%
  inner_join(non_disease_combined, 
             by=c("age","sex","age_group"))

### Generte outputs

#### Summary data frame by age and sex and total 

######## Dataframe with all outputs aggregated by year of simlation by sex
output_df_agg_sex  <- output_df   %>% ### Create a simulation year columns
  group_by(age_group, sex, .add=TRUE) %>%
  dplyr::mutate(year = 1:dplyr::n()) %>%
  ungroup() %>%
  dplyr::select(sex, year, Lx_bl, Lx_sc, Lx_diff, Lwx_bl, Lwx_sc, Lwx_diff, contains("num")) %>%
  group_by(year, sex, .add=TRUE) %>%
  dplyr::summarise_if(is.numeric, funs(sum)) %>%
  ungroup()
# 
# ######## Dataframe with all outputs aggregated by year of simulation all
output_df_agg_all  <- output_df   %>% ### Create a simulation year columns
  group_by(age_group, sex, .add=TRUE) %>%
  dplyr::mutate(year = 1:dplyr::n()) %>%
  ungroup() %>%
  dplyr::select(sex, year, Lx_bl, Lx_sc, Lx_diff, Lwx_bl, Lwx_sc, Lwx_diff, contains("num")) %>%
  group_by(year, .add=TRUE) %>%
  dplyr::summarise_if(is.numeric, funs(sum)) %>%
  ungroup()

### Create age groups variable, easier to read

output_df <- output_df %>%
  mutate(age_group_2 = case_when(
    age_group == 17 ~ "16-19",
    age_group == 22 ~ "20-24",
    age_group == 27 ~ "25-29",
    age_group == 32 ~ "30-34",
    age_group == 37 ~ "35-39",
    age_group == 42 ~ "40-44",
    age_group == 47 ~ "45-49",
    age_group == 52 ~ "50-54",
    age_group == 57 ~ "55-59",
    age_group == 62 ~ "60-64",
    age_group == 67 ~ "65-69",
    age_group == 72 ~ "70-74",
    age_group == 77 ~ "75-79",
    age_group == 82 ~ "80-84",
    age_group == 87 ~ "85-89",
    age_group == 92 ~ "90-94",
    age_group == 97 ~ "95 plus")) %>%
  mutate(cohort=paste(sex, age_group, sep = "_")) 


population <- mslt_df %>% dplyr::select(age_cat, sex, population_number) %>%
  dplyr::filter(population_number!=0) %>%
  dplyr::rename(age_group=age_cat)

output_df <- output_df %>% left_join(population)%>%
  dplyr::rename(`Age group` = age_group_2)


##################### Below outcomes for presentation ####################################################

# Table: Life expectancy and health adjusted life expectancy 
output_life_expectancy_change <- output_df[!duplicated(output_df$cohort), c("Age group", "cohort", "sex", "ex_bl", "ex_sc", "ewx_bl", "ewx_sc", "ex_diff", "ewx_diff", "population_number")] %>%
  dplyr::rename(`Life expectancy at baseline` = ex_bl, 
                `Life expectancy scenario` = ex_sc, 
                `Health adjusted life expectancy baseline` = ewx_bl, 
                `Health adjusted life expectancy scenario` = ewx_sc) %>%
  dplyr::mutate(`Difference in life expectancy in days` = ex_diff * 365, 
                `Difference in health adjusted life expectancy in days` = ewx_diff* 365) %>% 
  mutate_if(is.numeric, round, digits = 3) %>%
  dplyr::select(-c(ex_diff, ewx_diff, cohort)) %>%
  relocate(population_number, .after = sex)%>%
  dplyr::rename('Population cohort'=population_number)

output_life_expectancy_change <- output_life_expectancy_change[order(output_life_expectancy_change$sex),] 

# Table: Life years and health adjusted life years ----

output_life_years_change <- output_df %>% 
  group_by(sex, `Age group`, cohort, .add=TRUE) %>%
  dplyr::summarise_if(is.numeric, funs(sum)) %>%
  ungroup() %>%
  dplyr::select(`Age group`,cohort, sex,Lx_diff, Lwx_diff, population_number) %>%
  dplyr::rename(`Life years` = Lx_diff, 
                `Health adjusted life years` = Lwx_diff)  %>% 
  mutate_if(is.numeric, round) %>%
  relocate(population_number, .after = sex)%>%
  dplyr::rename('Population cohort'=population_number) %>%
  dplyr::select(-cohort)

# Table: Diseases deaths, incidence and ylds ----

output_diseases_change <- output_df %>% 
  group_by(sex, `Age group`, cohort, .add=TRUE) %>%
  dplyr::summarise_if(is.numeric, funs(sum)) %>% 
  mutate_if(is.numeric, round) %>%
  relocate(population_number, .after = sex)%>%
  dplyr::rename('Population cohort'=population_number) %>%
  dplyr::select(-cohort) %>%
  dplyr::select(`Age group`, sex, contains("diff")) %>%
  dplyr::select(-contains(c("Lx", "Lwx", "ex")))

return(list(output_df=output_df,LifeYears=output_life_years_change, LifeExpectancy=output_life_expectancy_change, Cause=output_diseases_change, 
            output_df_agg_sex=output_df_agg_sex, output_df_agg_all=output_df_agg_all))

}


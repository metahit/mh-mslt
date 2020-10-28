### Functions to create MSLT dataframe.

calculateGBDwider <- function(gbd, local_government_areas) {

  disease <- DISEASE_SHORT_NAMES %>% dplyr::filter(acronym!="no_pif") %>% dplyr::select(disease) ### process diseases with PIFS
  

   # gbd <- gbd
   # local_government_areas <- local_government_areas
  
  ## Clean names columns
  gbd <- gbd
  # remove '_name' from column names
  names(gbd) <- gsub(pattern = "_name", replacement = "", x = names(gbd))
  gbd <- gbd %>%
    dplyr::select(-contains("id")) %>%
    mutate(cause = tolower(cause)) %>%
    dplyr::filter(cause %in% disease$disease)
  
  ## Calculate population numbers
  gbd_tmp <- gbd %>%
    dplyr::select(-upper,-lower) %>%
    mutate(metric=tolower(metric)) %>%
    # this name is too long
    mutate(measure = ifelse(measure=='YLDs (Years Lived with Disability)','ylds',measure)) %>%
    mutate(age = ifelse(age=="95 plus", "95 to 120", age)) %>%
    mutate(age = ifelse(age=="Under 5", "0 to 5", age)) %>%
    pivot_wider(names_from="metric", values_from="val") %>%
    mutate(rate = rate / 100000) %>%
    mutate(pop = number / rate) %>%
    dplyr::select(measure,sex,age,cause,location,number,cityregion,pop) %>%
    group_by(measure, sex, age, cause, cityregion) %>%
    summarise(number = sum(number), pop = sum(pop)) %>%
    ungroup() %>%
    # Calculate rates per one
    mutate(rate=number/pop)
  
    #Aggregate by city regions and recalculate rates
  
  gbd_pop <- gbd_tmp %>%
    ## filter and select pop data only
    filter(cause == "all causes", measure == "Deaths") %>%
    dplyr::select(age,sex, cityregion, pop)
  
  ## Dataframe with rates per one
  gbd_rate <- gbd_tmp %>%
    # left_join(gbd_pop, by=c("age","sex")) %>%
    dplyr::select(measure,sex,age,cause,rate,cityregion,number) %>%
    rowwise() %>%
    mutate(from_age = as.numeric(str_split(age,' to ')[[1]][1])) %>%
    mutate(to_age = as.numeric(str_split(age,' to ')[[1]][2])) %>%
    mutate(age_cat = from_age + 2) %>%
    # using rowwise() turns the dataframe into a tibble
    data.frame()
  
  gbd_wider <- gbd_rate %>% 
    mutate(disease = tolower(abbreviate(cause))) %>%
    mutate(measure = tolower(measure)) %>%
    mutate(age_sex = paste(age_cat, tolower(sex), sep = "_")) %>%
    pivot_wider(id_cols = c(measure, sex, age, cityregion, disease, number,rate, age_cat, age_sex), 
                values_from = c(rate, number), names_from = c(measure, disease),
                names_glue = "{measure}_{.value}_{disease}") %>%
    left_join(gbd_pop, by = c("age", "sex", "cityregion")) %>% 
    mutate(sex = tolower(sex)) %>%
    rename(area=cityregion) %>%
    arrange(area, sex, age_cat)
  return(gbd_wider)
  
}  


calculateMSLT <- function(gbd_wider, location, disease) {
   location="bristol"
   gbd_wider <- gbd_wider %>% filter(area==location)
   load("C:/Metahit/mh-mslt/input/city regions/Output disbayes/cityregions_smoothed_res.rda")
   disease <- DISEASE_SHORT_NAMES %>% dplyr::filter(acronym!="no_pif") %>% dplyr::select(disease) ### process diseases with PIFS

  ## Create MSLT dataframe
  mslt_df <- data.frame(age = rep(c(0:100), 2), sex = append(rep("male", 101), 
                                                             rep("female", 101))) %>%
    dplyr::mutate(sex_age_cat = paste(sex, age, sep="_")) %>%
    dplyr::mutate(age_cat = case_when(age == 2 ~ 2,
                                      age == 7  ~ 7,
                                      age == 12  ~ 12,
                                      age == 17  ~ 17, 
                                      age == 22  ~ 22,
                                      age == 27  ~ 27,
                                      age == 32  ~ 32, 
                                      age == 37  ~ 37, 
                                      age == 42  ~ 42, 
                                      age == 47  ~ 47,
                                      age == 52  ~ 52, 
                                      age == 57  ~ 57, 
                                      age == 62  ~ 62, 
                                      age == 67  ~ 67, 
                                      age == 72  ~ 72,
                                      age == 77  ~ 77, 
                                      age == 82  ~ 82,
                                      age == 87  ~ 87,
                                      age == 92 ~ 92,
                                      age == 97 ~ 97)) %>%
    
    dplyr::mutate(age_cat_2 = case_when(age_cat == 2 ~  "0 to 4",
                                        age_cat == 7 ~  "5 to 9",
                                        age_cat ==  12 ~ "10 to 14",
                                        age_cat ==  17  ~ "15 to 19", 
                                        age_cat ==  22  ~ "20 to 24",
                                        age_cat ==  27  ~ "25 to 29",
                                        age_cat ==  32  ~ "30 to 34", 
                                        age_cat ==  37  ~ "35 to 39", 
                                        age_cat ==  42  ~ "40 to 44", 
                                        age_cat ==  47  ~ "45 to 49",
                                        age_cat ==  52  ~ "50 to 54", 
                                        age_cat ==  57  ~ "55 to 59", 
                                        age_cat ==  62  ~ "60 to 64", 
                                        age_cat ==  67  ~ "65 to 69", 
                                        age_cat ==  72  ~ "70 to 74",
                                        age_cat ==  77  ~ "75 to 79", 
                                        age_cat ==  82  ~ "80 to 84",
                                        age_cat ==  87  ~ "85 to 89",
                                        age_cat ==  92  ~ "90 to 94", 
                                        age_cat ==  97   ~ "95 to 100"))
  
  ## Get GBD data and interpolated rates
  
  gbd_df <- gbd_wider %>% filter(area==location)
  
  gbd_df[is.na(gbd_df)] <- 0 
  
  #### Disability weights
  
  # the row sum of all ylds_number_*disease* without ylds_number_allc
  all_ylds_count <- dplyr::select(gbd_df, contains("ylds_number"), age_sex) %>%
    dplyr::select(-ylds_number_allc) %>% 
    mutate(yld_total_included=rowSums(.[sapply(., is.numeric)], na.rm = TRUE))

  
  
  gbd_df <- gbd_df %>%
    left_join(dplyr::select(all_ylds_count, age_sex, yld_total_included)) %>%
    dplyr::mutate(ylds_rate_allc_adj_1 = (ylds_number_allc - yld_total_included)/pop)
  
  
  interpolateFunction <- function(valuesToInterpolate){
    age_group=0:100
    # only use ages where there is a value present
    age_group[is.na(valuesToInterpolate)] <- NA
    # removing the na entries
    age_group=age_group %>% .[!is.na(.)]
    valuesToInterpolate=valuesToInterpolate %>% .[!is.na(.)]
    # make the interpolation function
    # BZ: added log, rates as are cannot be interpolated (get negative values). We interpolate the log and then but with exp function for results
    InterFunc <- stats::splinefun(age_group, log(valuesToInterpolate),
                                  method="monoH.FC", ties=mean)
    # return interpolated values
    return(InterFunc(0:100))
  }
  
  # all values get their own column, expanding out to every age number
  mslt_df_longer <- mslt_df %>%
    left_join(gbd_df%>%dplyr::select(-age,-age_sex)) %>%
    pivot_longer(names_to=c("measure","rate_num","disease"),
                 names_sep="_",
                 cols= deaths_rate_allc:ylds_number_utrc) %>% ### BZ: how can we do this without manual entry?
    ## drop rows for combination of values measure=deaths and disease=mjdd and measure=incidence and disease=hypertensive heart disease
    dplyr::filter(!disease %in% c("hyhd", "mjdd", "acml"))

  
  
  # rows only represent age, sex and disease, everything else in columns.
  # Data has to be interpolated from 5-year age groups to 1-year age groups.
  mslt_df_by_disease <- mslt_df_longer %>%
    pivot_wider(names_from=c("measure","rate_num"),
                values_from=value) %>%
    dplyr::mutate(dw_adj=(ylds_number/prevalence_number)/(1-ylds_rate_allc_adj_1)) %>%
    dplyr::mutate(dw_adj=ifelse(is.nan(dw_adj),0,dw_adj)) %>%
    arrange(disease,sex,age) %>%
    group_by(disease,sex) %>%
    # interpolate dw_adj
    dplyr::mutate(dw_adj=exp(interpolateFunction(dw_adj))) %>%
    ## Interpolate mortality and ylds (all cause mortality is from Melbourne data)
    dplyr::mutate(deaths_rate=exp(interpolateFunction(deaths_rate))) %>%
    dplyr::mutate(ylds_rate=exp(interpolateFunction(ylds_rate))) %>%
    ## not sure if we were supposed to interpolate this one
    dplyr::mutate(ylds_rate_allc_adj_1=exp(interpolateFunction(ylds_rate_allc_adj_1))) %>%
    ungroup()
  
  mslt_df_wider <- mslt_df_by_disease %>%
    dplyr::select(age,sex,sex_age_cat, age_cat, age_cat_2,ylds_rate_allc_adj_1,
                  disease,deaths_rate,ylds_rate,dw_adj, pop)%>%
    pivot_wider(id_cols = c(age,sex,sex_age_cat, age_cat,age_cat_2,ylds_rate_allc_adj_1,pop),
                names_from=disease,
                values_from=c(deaths_rate,ylds_rate,dw_adj)) %>%
    dplyr::rename(pyld_rate=ylds_rate_allc_adj_1)
  
  
  
  ### Add Disbayes outputs rates per one

  disease <- cityregions_smoothed_res %>% dplyr::filter(area==location) %>% dplyr::select(med, gender, disease)
  
  ## Sort data
  ### create column one with outcome and year
  disease <- cbind(
    mes=rownames(disease), disease)
  
  ### Separate avoce in outcome and year
  disease <- cbind(disease, (str_split_fixed(disease$mes, fixed('['), 2)))

  disease <- disease[ (disease$`1` %in% c("inc", "cf", "prev")), ]
  disease$`1` <- as.character(disease$`1`)
  disease$`2` <- as.character(disease$`2`)
  disease$`2` <- gsub("].*", "",disease$`2`)


  ### Rename columns
  names(disease)[names(disease) == "1"] <- "rates"
  names(disease)[names(disease) == "2"] <- "age"
  names(disease)[names(disease) == "gender"] <- "sex"
   
  ### Rename string values inc to incidence, cf to case fatality and prev to prevalence
   
  disease <- disease %>%
    mutate(rates = str_replace(rates, "inc", "incidence"))  %>%
    mutate(rates = str_replace(rates, "cf", "case_fatality"))  %>%
    mutate(rates = str_replace(rates, "prev", "prevalence"))
  
  ### Age to int
  disease$age <- as.integer(disease$age)
  
  ### Pivot wider
  disease <- disease %>% dplyr::select(med, sex, disease, rates, age) %>%
    pivot_wider(names_from=c("rates","disease"),
                 values_from=med) 
  
  mslt_df_wider <- left_join(mslt_df_wider, disease) 
  
  ### Replace NaN, inf for 0 (younger ages to not have chronic diseases and some diseases are men/female only)
  
  mslt_df_wider <- mslt_df_wider %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))
  mslt_df_wider[mslt_df_wider == Inf] <- 0
  
  ### Rename variables
  
  mslt_df_wider <- mslt_df_wider %>% 
    rename(mx = deaths_rate_allc, 
           population_number=pop) 

  return(mslt_df_wider)
}


# compare <- read_csv(paste0(relative_path_execute, "/inputs/mslt/bristol_mslt.csv")) %>% arrange(sex, age)
# # # 
# difference <- mslt_df_wider$population_number- compare$population_number #minor diff
# difference <- mslt_df_wider$mx- compare$mx ## minor diff
# difference <- mslt_df_wider$pyld_rate- compare$pyld_rate ## significant, seems issue with original data, check all_ylds_counts order (female, age)
# difference <- mslt_df_wider$incidence_brsc- compare$incidence_brsc
# difference <- mslt_df_wider$incidence_copd- compare$incidence_copd ## there seems to be a systematic issue with females
# difference <- mslt_df_wider %>% select(age, sex, starts_with("case_fatality"))
# compare2 <- select(compare, age, sex, starts_with("case_fatality"))
# compare3 <- left_join(difference, compare2, by=c("age", "sex"))
# 
# plot_case_fatality <- compare3 %>% filter(sex=="female") %>%
#   ggplot(aes(x=age, y=case_fatality_ishd.x)) +
#   geom_line(aes(x = age, y = case_fatality_ishd.x),colour = "red") +
#   geom_line(aes(x = age, y = case_fatality_ishd.y),colour = "green")


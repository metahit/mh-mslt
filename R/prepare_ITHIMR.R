### Prepare data for CRA processing in MH project

source("~/mh-mslt/R/prepare_DATA.R")


### Get data and add lad and utla info to then aggregate by city region
  
  local_goverment_areas= paste0(relative_path_execute, "/inputs/mh_regions_lad_lookup.csv")
  lad= paste0(relative_path_mslt,"/input/Local_Authority_District_to_Region_(December_2017)_Lookup_in_England.csv")
  utla=paste0(relative_path_mslt,"/input/Lower_Tier_Local_Authority_to_Upper_Tier_Local_Authority_(December_2017)_Lookup_in_England_and_Wales.csv")
  gbd_data= paste0(relative_path_mslt, "/input/gbd/GBD2019/ITHIM/")

  
  
### Sort data
  source("~/mh-mslt/R/prepare_DATA.R")
  
  gbd_data <- PrepareData(
    
    local_goverment_areas= paste0(relative_path_execute, "/inputs/mh_regions_lad_lookup.csv"),
    lad= paste0(relative_path_mslt,"/input/Local_Authority_District_to_Region_(December_2017)_Lookup_in_England.csv"),
    utla=paste0(relative_path_mslt,"/input/Lower_Tier_Local_Authority_to_Upper_Tier_Local_Authority_(December_2017)_Lookup_in_England_and_Wales.csv"),
    gbd_data= paste0(relative_path_mslt, "/input/gbd/GBD2019/ITHIM/"))


### Calculate baseline data by area (city regions)

gbd <- gbd_data

## Calculate population numbers
gbd_tmp <- gbd %>%
  dplyr::select(-upper,-lower) %>%
  mutate(metric=tolower(metric)) %>%
  mutate(age = ifelse(age=="95 plus", "95 to 99", age)) %>%
  mutate(age = ifelse(age=="Under 5", "0 to 5", age)) %>%
  pivot_wider(names_from="metric", values_from="val") %>%
  mutate(rate = rate / 100000) %>%
  mutate(pop = number / rate) %>%
  dplyr::select(measure,sex,age,cause,location,number,cityregion,pop) %>%
  group_by(measure, sex, age, cause, cityregion) %>% ### Adds localities within city regions
  dplyr::summarise(number = sum(number), pop = sum(pop)) %>%
  ungroup()

## Population data city regions

gbd_pop <- gbd_tmp %>%
  ## filter and select pop data only
  dplyr::filter(cause == "all causes", measure == "Deaths") %>% 
  dplyr::select(age, sex, cityregion, pop)


## Add up individual diseases for head-neck-cancer

gbd_tmp_2 <- gbd_tmp %>% dplyr::filter(cause %in% c("larynx cancer", "lip and oral cavity cancer",
                                                      "nasopharynx cancer", "other pharynx cancer")) %>%
  mutate(cause="head and neck cancer") %>%
  group_by(measure, cause, sex, age, cityregion) %>%
  summarise(number = sum(number)) %>%
  ungroup() %>%
  left_join(gbd_pop)

gbd_final <- gbd_tmp %>% 
  filter(!cause %in% c("larynx cancer", "lip and oral cavity cancer",
                       "nasopharynx cancer", "other pharynx cancer")) %>%
  bind_rows(gbd_tmp_2)


### Save files per city region

for (c in c(unique(gbd_final$cityregion))) {
  
  data <- filter(gbd_final, cityregion==c)
  
   write_csv(data, paste0(relative_path_execute, "/inputs/gbd/", c, ".csv"))

}


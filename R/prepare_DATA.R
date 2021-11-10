#### Function to organise local goverment areas

PrepareData <- function(local_goverment_areas, lad, utla, gbd_data){
  
  # local_goverment_areas= paste0(relative_path_execute, "/inputs/mh_regions_lad_lookup.csv")
  # lad= paste0(relative_path_mslt,"/input/Local_Authority_District_to_Region_(December_2017)_Lookup_in_England.csv")
  # utla=paste0(relative_path_mslt,"/input/Lower_Tier_Local_Authority_to_Upper_Tier_Local_Authority_(December_2017)_Lookup_in_England_and_Wales.csv")
  # gbd_data= paste0(relative_path_mslt, "/input/gbd/GBD2019/METAHIT/")

  ### Local areas names
  regions <- c("East Midlands", "East of England", "Greater London", "North East England", 
               'North West England', "South East England", "South West England", "Yorkshire and the Humber", "West Midlands")
  # LTLAs to city regions and regions 
  local_government_areas <- read_csv(local_goverment_areas) %>% rename(region="gordet")
  lad <-  read_csv(lad)
  utla <- read_csv(utla)
  ## Make UTLA to region lookup
  lad_wales <- c("Isle of Anglesey", "Gwynedd", "Conwy", "Denbighshire", "Flintshire", 
                 "Wrexham", "Ceredigion", "Pembrokeshire", "Carmarthenshire", 
                 "Swansea", "Neath Port Talbot", "Bridgend", "Vale of Glamorgan", 
                 "Cardiff", "Rhondda Cynon Taf", "Caerphilly", "Blaenau Gwent", 
                 "Torfaen", "Monmouthshire", "Newport", "Powys", "Merthyr Tydfil")
  utlareg <- utla %>% 
    filter(!duplicated(UTLA17NM)) %>%
    select(LTLA17NM, UTLA17NM) %>%
    rename("LAD17NM"=LTLA17NM) %>%
    left_join(lad, by="LAD17NM") %>%
    filter(!(LAD17NM %in% lad_wales))
  
  ## GBD data
    
  list_of_files <- list.files(path = gbd_data,
                              pattern = "\\.csv$",
                              full.names = TRUE)
  gbd <- do.call(rbind, lapply(list_of_files, read.csv))
  
  ## Attach geographical info to each GBD estimate
  ## Manual step - this should cover them all 
  
  gbd$location_name[gbd$location_name=="St Helens"] <- "St. Helens"
  gbd <- gbd %>% mutate(
    areatype = case_when(location_name %in% c("United Kingdom","Australia")  ~ "country",
                         location_name %in% c("England","Scotland","Wales","Northern Ireland")  ~ "nation",
                         location_name %in% regions  ~ "region",
                         location_name %in% unique(utla$LTLA17NM) ~ "ltla",
                         location_name %in% unique(utla$UTLA17NM) ~ "utla"
    )
  )
  stopifnot(all(!is.na(gbd$areatype)))
  ## Determine which estimates are in city regions 
  gbd <- gbd %>%
    left_join(local_government_areas, by=c("location_name"="lad11nm")) %>%
    mutate(in_cityregion = !is.na(cityregion)) %>%
    filter(areatype %in% c("ltla","utla"))
  
  names(gbd) <- gsub(pattern = "_name", replacement = "", x = names(gbd))
  gbd <- gbd %>%
    select(-contains("id")) %>%
    mutate(cause = tolower(cause)) %>% 
    filter(in_cityregion) ### Only keep locations in city regions

return(gbd)
}
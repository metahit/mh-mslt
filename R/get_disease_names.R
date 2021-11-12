##### Get coding table 

GetDiseaseTable <- function(disease_names_execute, gbd_data) {
  
  
  # gbd_data= paste0(relative_path_mslt, "/input/gbd/GBD2019/METAHIT/")
 
  # disease_names_execute=paste0(relative_path_execute, "/inputs/dose_response/disease_outcomes_lookup_new.csv")

  list_of_files <- list.files(path = gbd_data,
                              pattern = "\\.csv$",
                              full.names = TRUE)
  gbd <- do.call(rbind, lapply(list_of_files, read.csv))
  
  disease_names_execute <- read.csv(disease_names_execute,
                                    as.is=T,fileEncoding="UTF-8-BOM") %>%
    dplyr::select(GBD_name, acronym) %>%
    mutate(disease = tolower(GBD_name)) 
  
  ## add head and neck cancer and delete individual diseases
  
  disease_names_execute[nrow(disease_names_execute) + 1,] = c("head and neck cancer","head-and-neck-cancer", "head and neck cancer")
  disease_names_execute[nrow(disease_names_execute) + 1,] = c("rectum cancer","rectum-cancer", "rectum cancer")
  disease_names_execute <- disease_names_execute %>% dplyr::filter(!GBD_name %in% c("Larynx cancer", "Lip and oral cavity cancer",
                                                                                    "Nasopharynx cancer", "Other pharynx cancer"))
  
  
  DISEASE_SHORT_NAMES <- disease_names_execute %>%
    mutate(sname = abbreviate(disease)) %>%
    mutate(is_not_dis = ifelse(( grepl("injuries", disease) | # grepl determines if the disease string contains 'injuries'
                                   grepl("all causes", disease) |
                                   grepl("lower respiratory infections", disease)), 
                               1, 0)) %>%
    mutate(is_not_dis = case_when(sname == "allc"  ~  2,
                                  sname == "lwri"  ~  1,
                                  sname == "npls" ~ 2,
                                  sname == "crdd" ~ 2,
                                  # ## Code for major depressive disorder (no deaths) and hypertensive heart disease (no incidence)
                                  sname == "hyhd"  ~  3,
                                  sname == "dprd"  ~  3,
                                  TRUE  ~  is_not_dis)) %>%
    mutate(
      males = ifelse(disease %in% c("uterine cancer", "breast cancer"), 0, 1),
      females = ifelse(disease %in% "prostate cancer", 0, 1),
      #sname = gsub("'", '', sname),
      acronym = ifelse(is.na(acronym), sapply(strsplit(disease, " "), head, 1), acronym))
  return(DISEASE_SHORT_NAMES)
  
}

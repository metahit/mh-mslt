##### Get coding table 

GetDiseaseTable <- function(disease_names_execute) {
  
# disease_names_execute <- read_csv(file.path(relative_path_execute, "inputs/dose_response/disease_outcomes_lookup.csv"))

disease_names_execute  <-  disease_names_execute  %>%
  select(GBD_name, acronym) %>%
  mutate(disease = tolower(GBD_name))

DISEASE_SHORT_NAMES <- data.frame(disease = tolower(as.character(unique(gbd$cause_name))), 
                                  sname = tolower(abbreviate(unique(gbd$cause_name, max = 2))),
                                  stringsAsFactors = F) %>%
  dplyr::mutate(is_not_dis = ifelse((str_detect(disease, "injuries") |
                                       str_detect(disease, "All causes") |
                                       str_detect(disease, "Lower respiratory infections")), 
                                    1, 0) ) %>%
  dplyr::mutate(is_not_dis = case_when(sname == "allc"  ~  2,
                                       sname == "lwri"  ~  1,
                                       ## Code for major depressive disorder (no deaths) and hypertensive heart disease (no incidence)
                                       sname == "hyhd"  ~  3,
                                       sname == "mjdd"  ~  3,
                                       TRUE  ~  is_not_dis)) %>%
  left_join(disease_names_execute, by="disease") %>%
  dplyr::mutate(acronym = ifelse(str_detect(disease, "injuries"), disease, acronym),
                acronym = word(acronym, 1),
                males = ifelse(disease %in% c("uterine cancer", "breast cancer"), 0, 1),
                females = ifelse(disease %in% "prostate cancer", 0, 1),
                sname = gsub("'", '', sname),
                acronym = ifelse(is.na(acronym), "no_pif", acronym))

return(DISEASE_SHORT_NAMES)

}
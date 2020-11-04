#### Function to organise local goverment areas

GetGEO<- function(data_local){
# data <- read_csv(file.path(relative_path_execute, "inputs/mh_regions_lad_lookup.csv"))
names_non_cr <- c("United Kingdom", "England", "East Midlands", "East of England", "Greater London", "North East England", 
                  'North West England', "South East England", "South West England", "West Midlands", "Yorkshire and the Humber", 
                  "Northern Ireland", "Scotland", "Wales")
local_government_areas <- data_local
for (i in names_non_cr){
  local_government_areas <- rbind(local_government_areas, rep(i, ncol(local_government_areas)))
}
local_government_areas  <- local_government_areas %>%
  dplyr::filter(!is.na(cityregion)) %>%
  dplyr::rename(location = lad11nm) %>%
  dplyr::mutate(location = gsub('St. Helens', 'St Helens', location))

return(local_government_areas)
}
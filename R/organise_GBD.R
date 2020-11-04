#### Organise GBD data for city regions 

getGBD <- function(path, local_government_areas) {

# path=relative_path_gbd
# local_govermenet_areas_local_goverment_areas
myfiles = list.files(path, pattern="*.csv", full.names=TRUE)

gbd <-  ldply(myfiles, read_csv) %>%
  dplyr::filter(location_name %in% local_government_areas$location)


## Join geographical data to GBD data original look up table. First check similarities between locations in gbd data and local_goverment_areas data.

### Compare gdd location_name with local_goverment_areas locations (local authority districs (lad))

gbd_loc <- unique(gbd$location_name)
ons_lad <- unique(local_government_areas$location)
diff <- setdiff(ons_lad, gbd_loc) ### difference ony

### GBD data for nottingham city regions is not available local authority district (Ashfield, Bassetlaw, Broxtow, Gedling, Mansfield, Newark and        Sherwood,Rushcliffe) but for Nottingham and Nottinghamshire. Then we used Notthinghan and Nottinghamshire gbd data for Nottinham city region
### Replace as per above. City of London still missing from GBD data.

local_government_areas <- local_government_areas %>% mutate(location=ifelse(cityregion=="nottingham" & location!="Nottingham", "Nottinghamshire", location))

## check difference 

gbd_loc <- unique(gbd$location_name)
ons_lad <- unique(local_government_areas$location)
diff <- setdiff(ons_lad, gbd_loc)


## Join local goverment data to get city regions names and then aggregate data (numbers) by city region.

gbd <- gbd %>% left_join(local_government_areas, by=c("location_name"="location"))


return(gbd)

}

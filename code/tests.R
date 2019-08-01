# ---- Test disbayes with one disease ----

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


test_disbayes2 <- readRDS("data/city regions/bristol/dismod/input/Ishd_female.rds")

test_disbayes <- ihdlondon
#
datstan <- c(as.list(test_disbayes2), nage=nrow(test_disbayes))
inits <- list(
   list(cf=rep(0.0101, datstan$nage)),
   list(cf=rep(0.0201, datstan$nage)),
   list(cf=rep(0.0056, datstan$nage)),
   list(cf=rep(0.0071, datstan$nage))
)

 gbdcf_test <- stan("disbayes-master/gbdcf-unsmoothed.stan", data=datstan, init=inits)

 gbdcf_test_summary <- summary(gbdcf_test)$summary
 
# ---- Plot disbayes and dismod outcomes to compare ----


 ## Case fatality from dismod
 
 dismod_output_case_fatality <- readxl::read_xlsx("data/Disbayes/compare_dismod.xlsx", sheet = 1) %>% select(age, sex, starts_with("case"))
 
 ## Incidence from dismod
 
 dismod_output_incidence <- readxl::read_xlsx("data/Disbayes/compare_dismod.xlsx", sheet = 1) %>% select(age, sex, starts_with("incidence"))
 
 ## Join comparison dataframe
 
 dismod_compare <- dismod_output_case_fatality %>% left_join(dismod_output_incidence)


 
 View(dismod_compare)
 
 ### Join
 
 ### try these code here
 
 ## Plots data
 
  dismod <- dismod_compare
  dismod$sex_age_cat <- paste(sex, age, sep = "_")
  
  # test_compare <- merge(dismod, disbayes, id = "sex_age_cat")
  # test_compare_melted <- reshape2::melt(test_compare, id.var="sex_age_cat")
  
   ## Add groups for comparison graphs
  
  dismod$groups1 <- rep(c("Dismod"), each=202)
  disbayes$groups2 <- rep(c("Disbayes"), each=202)
 
 ## Loop create plots
 compare_plot_list <- list()
 index <- 1
 
 for (sex in i_sex) {
   for (output in output_disease) {
   for (d in 1:nrow(disease_short_names)){
     
     if (disease_short_names$is_not_dis[d] == 0){

      d1 <- filter(dismod, sex == i_sex)
      
      d2 <- filter(disbayes, sex == i_sex)
      
      p <- ggplot() +
        geom_line(data=d1, aes_string(x = 'age', y = paste(output, disease_short_names$sname[d], "dismod", sep = "_"), color = "groups1")) +
        geom_line(data=d2, aes_string(x = 'age', y = paste(output, disease_short_names$sname[d], sep = "_"), color = "groups2")) +
         scale_color_discrete("")
  
        p <-  p + xlab('Age') + ylab (paste("Rate", output, disease_short_names$sname[d], sep = " " )) + labs(title = "Compare Dismod vs Disbayes")

       

        theme_classic()

  
  ggsave(p, file=paste("disbayes_compare/",output, "_", disease_short_names$sname[d], "_", sex, ".tiff", sep=""), width = 14, height = 10, units = "cm")

  compare_plot_list[[index]] <- p
  
  index <- index + 1
     
      }
    }
  }
 } 
 
 
 
 ##### Test loop using table from road for road injuries
 
 ### Add matching age categories
 ### NEED TO TAKE INTO ACCOUNT IN THE LOOP THAT PIFS ARE ONLY CALCULATED FOR ADULTS
 ### NEED TO AGREE NAMING CONVENTION WITH PIFS (ROB), BEST IF WE BOTH IMPLEMENT ABBREVIATION OF ORIGINAL GBD DISEASES RATHER THAN MANUAL ENTRIES.
 
 pif <- read_csv("data/pif.csv")

 pif$age [pif$age_cat =="16-19"] <- 17
 pif$age [pif$age_cat =="20-24"] <- 22
 pif$age [pif$age_cat =="25-29"] <- 27
 pif$age [pif$age_cat =="30-34"] <- 32
 pif$age [pif$age_cat =="35-39"] <- 37
 pif$age [pif$age_cat =="40-44"] <- 42
 pif$age [pif$age_cat =="45-49"] <- 47
 pif$age [pif$age_cat =="50-54"] <- 52
 pif$age [pif$age_cat =="55-59"] <- 57
 pif$age [pif$age_cat =="60-64"] <- 62
 pif$age [pif$age_cat =="65-69"] <- 67
 pif$age [pif$age_cat =="70-74"] <- 72
 pif$age [pif$age_cat =="75-79"] <- 77
 pif$age [pif$age_cat =="80-84"] <- 82
 pif$age [pif$age_cat =="85-89"] <- 87
 pif$age [pif$age_cat =="90-94"] <- 92
 pif$age [pif$age_cat =="95-120"] <- 97
 
 ## Change names to get rid of risk factors combinations in the name (BEST IF I DO NOT HAVE TO DO THIS MANUALLY)

names(pif)[names(pif) == "scen_pif_pa_ap_noise_no2_ihd"] <- "pif_ihd"
names(pif)[names(pif) == "scen_pif_pa_ap_stroke"] <- "pif_stroke"
names(pif)[names(pif) == "scen_pif_pa_colon" ] <- "pif_colon"
names(pif)[names(pif) == "scen_pif_pa_t2d"] <- "pif_t2d"
names(pif)[names(pif) == "scen_pif_pa_endo"] <- "pif_endo"
names(pif)[names(pif) == "scen_pif_pa_ap_lc"] <- "pif_lc"
names(pif)[names(pif) == "scen_pif_ap_lri"] <- "pif_lri"
names(pif)[names(pif) == "scen_pif_ap_copd"] <- "pif_copd"
names(pif)[names(pif) == "scen_pif_pa_breast"] <- "pif_breast"

names(pif)[names(pif) == "scen_cyclist_Fatal"] <- "pif_cyclist_deaths"
names(pif)[names(pif) == "scen_pedestrian_Fatal"] <- "pif_pedestrian_deaths"
names(pif)[names(pif) == "scen_cyclist_Serious"] <- "pif_cyclist_ylds"
names(pif)[names(pif) == "scen_pedestrian_Serious"] <- "pif_pedestrian_ylds"
names(pif)[names(pif) == "scen_car/taxi_Fatal"] <- "pif_motor_deaths"
names(pif)[names(pif) == "scen_motorcycle_Fatal"  ] <- "pif_motorcyclist_deaths"
names(pif)[names(pif) == "scen_car/taxi_Serious"] <- "pif_motor_ylds"
names(pif)[names(pif) == "scen_motorcycle_Serious"  ] <- "pif_motorcyclist_ylds"


## Repeat pif lri for deaths and ylds

pif$pif_lri_deaths <- pif$pif_lri
pif$pif_lri_ylds <- pif$pif_lri

### mslt_df names are not matching pifs names, need to change this, preferably, not manually

#### MANUALLY TO CHECK THAT IT WORKS FOR ROAD INJURIES

names(mslt_df)[names(mslt_df) == "deaths_rate_pdri"] <- "deaths_rate_pedestrian"
names(mslt_df)[names(mslt_df) == "ylds (years lived with disability)_rate_pdri"] <- "ylds_rate_pedestrian"

names(mslt_df)[names(mslt_df) == "deaths_rate_cyri"] <- "deaths_rate_cyclist"
names(mslt_df)[names(mslt_df) == "ylds (years lived with disability)_rate_cyri"] <- "ylds_rate_cyclist"

names(mslt_df)[names(mslt_df) == "deaths_rate_mtri"] <- "deaths_rate_motorcyclist"
names(mslt_df)[names(mslt_df) == "ylds (years lived with disability)_rate_mtri"] <- "ylds_rate_motorcyclist"

names(mslt_df)[names(mslt_df) == "deaths_rate_mvri"] <- "deaths_rate_motor"
names(mslt_df)[names(mslt_df) == "ylds (years lived with disability)_rate_mvri"] <- "ylds_rate_motor"


names(mslt_df)[names(mslt_df) == "deaths_rate_otri"] <- "deaths_rate_other"
names(mslt_df)[names(mslt_df) == "ylds (years lived with disability)_rate_otri"] <- "ylds_rate_other"

names(mslt_df)[names(mslt_df) == "deaths_rate_lwri"] <- "deaths_rate_lri"
names(mslt_df)[names(mslt_df) == "ylds (years lived with disability)_rate_lwri"] <- "ylds_rate_lri"


### Expand Pifs table


  
p <- filter(pif, sex == "male")

outage <- min(p$age):100

ind <- findInterval(outage, p$age)
pif_expanded <- p[ind,]
pif_expanded$age <- outage

p_1 <- filter(pif, sex == "female")

outage <- min(p_1$age):100

ind <- findInterval(outage, p_1$age)
pif_expanded_1 <- p_1[ind,]
pif_expanded_1$age <- outage


pif_expanded <- rbind(pif_expanded, pif_expanded_1)




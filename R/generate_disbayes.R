#### City regions (Combined Authority Areas)
# Read data

### Small numbers or diseases without mortality (depression)

library(disbayes)
library(dplyr)
library(tidyverse)

### Data for city regions

data <- read_rds("~/mh-mslt/input/city regions/Input disbayes/disbayes_input_8_11_21.rds") %>%
  filter(areatype=="cityregion") %>% filter(!disease %in% "Depressive disorders")
                                                

### add trends (TO DO)

disbayes_outputs_cr <- list()
index <- 1
for (g in unique(data$gender)){
     for (d in unique(data$disease)) {
          for (a in unique(data$area)) {
            
            if (g == "Male" && d %in% c("Breast_cancer", "Uterine cancer")){
              cat("\n")
            }
            else {
                        
# g="Female"
# d="Colon and rectum cancer" 
# a="England"
input <- filter(data, gender==g, disease==d, area==a)


  
dbres <- disbayes(dat = input, age = "age",
                  inc_num = "inc_num", inc_denom = "inc_denom", 
                  prev_num = "prev_num", prev_denom = "prev_denom",
                  mort_num = "mort_num", mort_denom = "mort_denom",
                  rem_num = "rem_num", rem_denom = "rem_denom",
                  hp_fixed = list(scf=1.2, sinc=TRUE), ### fixes issue with diseases with very small numbers ("Chronic myeloid leukemia")
                  method="opt",                  
                  eqage = ifelse(d=="Alzheimers disease and other dementias",40, 
                                 ifelse(d %in% c("Uterine cancer", "Stomach cancer"), 30,
                                        ifelse(d=="Ischemic heart disease", 30, 50))))

# Summarise outcomes
summ <- tidy(dbres) %>%
  filter(var %in% c("cf", "rem", "inc", "prev", "mort")) %>%
  select(age, var, `2.5%`, `50%`, `97.5%`) %>%
  mutate(disease=d,
         region=a,
         sex=g)


disbayes_outputs_cr[[index]] <- summ
index <- index + 1
            }
          }
     }
}

mslt_data_cr <- bind_rows(disbayes_outputs_cr)
### Check data



cf <- mslt_data_cr %>% filter(region=="Bristol", sex=="Female", disease=="Breast cancer",
                             var=="cf")

ggplot(cf, aes(x=age)) + 
  geom_pointrange(aes(y=`50%`, ymin=`2.5%`, ymax=`97.5%`),
                  data=cf, col="blue", alpha=0.5) +
  geom_pointrange(aes(y=`50%`, ymin=`2.5%`, ymax=`97.5%`),
                  data=cf, col="black", alpha=0.5)


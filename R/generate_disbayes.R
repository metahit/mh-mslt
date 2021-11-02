#### City regions (Combined Authority Areas)
# Read data

library(disbayes)
library(dplyr)
library(tidyverse)

### Data for city regions
data <- read_rds("~/mh-mslt/input/city regions/Input disbayes/disbayes_input_2_11_21.rds") 




### add remission and and trends (TO DO)

disbayes_outputs_cr <- list()
index <- 1
for (g in unique(data$gender)) {
     for (d in unique(data$disease)) {
          for (a in unique(data$area)) {
g="Female"
d="Breast cancer"
a="Bristol"
input <- filter(data, gender==g, disease==d, area==a)

dbres <- disbayes(dat = input, age = "age",
                  inc_num = "inc_num", inc_denom = "inc_denom", 
                  prev_num = "prev_num", prev_denom = "prev_denom",
                  mort_num = "mort_num", mort_denom = "mort_denom",
                  rem_num = "rem_num", rem_denom = "rem_denom",
                  inc_model="indep",
                  eqage = 40)

## Summarise outcomes
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

### Check data


cf <- inputs_mslt %>% filter(var=="cf")

ggplot(cf, aes(x=age)) + 
  geom_pointrange(aes(y=`50%`, ymin=`2.5%`, ymax=`97.5%`),
                  data=cf, col="blue", alpha=0.5) +
  geom_pointrange(aes(y=`50%`, ymin=`2.5%`, ymax=`97.5%`),
                  data=cf, col="black", alpha=0.5)


## Run disbayes for the ith area/gender/disease combination 
## This script is called by metahit-hpc-slurm.sh
## which distributes the analyses for all combinations over a HPC cluster 

library("devtools")
library("disbayes")
options(mc.cores = 4)

cityregions <- c("bristol","liverpool","london","westmidlands","sheffield","northeast","greatermanchester","nottingham","leeds")
englandregions <- c("East Midlands", "East of England", "Greater London", "North East England", "North West England", "South East England", "South West England", "West Midlands", "Yorkshire and the Humber")
ukcountries <- c("England", "Wales", "Scotland", "Northern Ireland")
uk <- c("United Kingdom")

load(file="data/metahit_areas.rda")
## Change according to cityregions/countries/regions
## cdat <- metahit_areas[metahit_areas$area %in% cityregions,]
## cdat <- metahit_areas[metahit_areas$area %in% englandregions,]
## cdat <- metahit_areas[metahit_areas$area %in% ukcountries,]
cdat <- metahit_areas[metahit_areas$area %in% uk,]

task_id_string <- Sys.getenv("SLURM_ARRAY_TASK_ID")
task_id <- as.numeric(task_id_string)
runs_todo <- 1:nrow(cdat) 
i <- runs_todo[task_id]

datadir <- "data/city regions/Input disbayes"
dat <- readRDS(file.path(datadir, cdat$file[i]))

db <- disbayes(dat = dat,
               inc = "inc", 
               inc_denom = "pop", 
               prev_num = "prevn", 
               prev_denom = "prevdenom",
               mort = "mort",
               mort_denom = "pop",
               eqage = 50,
               smooth = TRUE
               )
ress <- summary_disbayes_fit(db$fit)
resu <- summary_disbayes_fit(db$fitu)
res <- list(ress, resu) 

label <- paste(cdat$area[i], cdat$gender[i], cdat$disease[i], i, sep="_")

## Change according to cityregions/countries/regions
filename <- paste0("data/city regions/Output disbayes/temp/countries/", label, ".rda")
save(res, file=filename)
cat(sprintf("Completed case %s\n", i), file="metahit-hpc-log.txt", append=TRUE)

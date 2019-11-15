## Process results from analyses done on HPC cluster

library(dplyr)
library(ggplot2)

## First convert results from one file per area x gender x disease 
## to 3 x 2 = 6 files, each containing all estimates from
## (cities or regions or countries) x (smoothed or unsmoothed model) 

load(file="data/metahit_areas.rda")
results_dir <- "data/city regions/Output disbayes"

cityregions <- c("bristol","liverpool","london","westmidlands","sheffield","northeast","greatermanchester","nottingham","leeds")
regions <- c("East Midlands", "East of England", "Greater London", "North East England", "North West England", "South East England", "South West England", "West Midlands", "Yorkshire and the Humber")
countries <- c("England", "Wales", "Scotland", "Northern Ireland")
uk <- "United Kingdom"

process_results <- function(area_str){
    cdat <- metahit_areas %>% filter(area %in% get(area_str))
    nrun <- nrow(cdat)
    resu <- ress <- vector(nrun, mode="list")
    for (i in 1:nrun){
        label <- paste(cdat$area[i], cdat$gender[i], cdat$disease[i], i, sep="_")
        load(file=sprintf("%s/temp/%s/%s.rda", results_dir, area_str, label))
        ress[[i]] <- res[[1]]
        resu[[i]] <- res[[2]]
        for (j in c("area","gender","disease"))
            ress[[i]][j] <- resu[[i]][j] <- cdat[i,j]
        ress[[i]]$model <- "smoothed"
        resu[[i]]$model <- "unsmoothed"
    }
    sobjname <- sprintf("%s_smoothed_res", area_str)
    uobjname <- sprintf("%s_unsmoothed_res", area_str)
    assign(sobjname, do.call("rbind", ress))
    assign(uobjname, do.call("rbind", resu))
    save(list=sobjname, file=sprintf("%s/%s_smoothed_res.rda", results_dir, area_str))
    save(list=uobjname, file=sprintf("%s/%s_unsmoothed_res.rda", results_dir, area_str))
}

process_results("cityregions")
process_results("regions")
process_results("uk")


## Generate plots showing case fatality vs age curves
## Each plot shows posteriors from both smoothed and unsmoothed model, all diseases
## One plot for each area x gender 

generate_plots <- function(area_str){
    outdir <- sprintf("%s/%s", results_dir, area_str)
    load(file=sprintf("%s/%s_smoothed_res.rda", results_dir, area_str))
    load(file=sprintf("%s/%s_unsmoothed_res.rda", results_dir, area_str))
    resdf <- get(sprintf("%s_smoothed_res", area_str))
    resdfu <- get(sprintf("%s_unsmoothed_res", area_str))

    for (area in get(area_str)){
        for (gender in c("female","male")){
            dat <- resdf[resdf$area==area & resdf$gender==gender & grepl("cf\\[", rownames(resdf)),]
            dat$age <- 1:101
            datu <- resdfu[resdfu$area==area & resdfu$gender==gender & grepl("cf\\[", rownames(resdfu)),]
            datu$age <- 1:101
            p <- ggplot(dat, aes(x=age)) +
              geom_pointrange(data=datu, aes(y=med, ymin=lower95, ymax=upper95), col="gray") +
              geom_pointrange(aes(y=med, ymin=lower95, ymax=upper95), col="black") +
              facet_wrap(vars(disease), ncol=2) +  
              coord_cartesian(ylim=c(0, 1)) +
              scale_x_continuous(breaks=seq(0,100,10)) + 
              ylab("Case fatality (median, 95% CrI)")  +
              xlab("Age (years)") +
              ggtitle(sprintf("%s - %s", area, gender))
            pdf(sprintf("%s/%s_%s.pdf", outdir, area, gender))
            print(p)
            dev.off()
        }
    }
}

generate_plots("cityregions")
generate_plots("regions")
generate_plots("countries")
generate_plots("uk")



### Generate plots comparing same disease+gender for different city regions 
### Point estimates only, from the smoothed model 

load(file=sprintf("%s/cityregions_smoothed_res.rda",results_dir)
load(file=sprintf("%s/countries_smoothed_res.rda",results_dir)

plot_pointests <- function(gender){ 
    resdf <- cityregions_smoothed_res
    resdf <- resdf[resdf$gender==gender,]
    resdf <- resdf[grep("cf\\[", rownames(resdf)),]
    resdf$age <- 1:101

    p <- ggplot(resdf, aes(x=age, y=med, col=area)) +
      geom_line() +
      coord_cartesian(ylim=c(0, 0.1)) +
      facet_wrap(vars(disease), ncol=2) +
      ylab("Case fatality")

    ## Overlay point estimate for whole of England 

    resdf <- countries_smoothed_res
    resdf <- resdf[resdf$gender==gender & resdf$area=="England",]
    resdf <- resdf[grep("cf\\[", rownames(resdf)),]
    resdf$age <- 1:101

    p <- p +
      geom_line(data=resdf, size=1.5, lty=2) +
      ggtitle(sprintf("%s, point estimates for city regions and England", gender))

    pdf(sprintf("%s/pointests_%s.pdf",results_dir, gender))
    print(p)
    dev.off()
} 

plot_pointests("male")
plot_pointests("female")

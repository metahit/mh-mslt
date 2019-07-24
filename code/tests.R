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
 
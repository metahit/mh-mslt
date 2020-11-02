# ---- Chunk 1: Disbayes ----
### Disbayes works in linux virtual machine


library(disbayes)
source('mh-mslt/code/functions.R')

### Diseases short names is prepared in data_prep. Saved as it takes a long time to run code data_prep. 
DISEASE_SHORT_NAMES <- read_csv(paste0(relative_path_mslt, '/data/parameters/disease_names.csv'))

### Add unique index to input_disbayes to speed it up

disbayes_inputs$index <- paste(disbayes_inputs$cityregion, disbayes_inputs$sex, disbayes_inputs$disease, sep = "_")



### Loop to generate disbayes outputs for: diseases, sex and area. Uses GenOutDisbayes. Assumptions and inputs for disbayes
### need changing in the function. 

disbayes_output<- list()

index <- 1

for (i in c(unique(disbayes_inputs$index))) {


      data <- dplyr::filter(disbayes_inputs, index == i)
        
    if (data$sex == "male" &&  data$disease  %in% c("brsc", "utrc") || data$sex == "female" &&  data$disease == "prsc"){}

      else {

     
    disbayes_output[[index]] <- disbayes:::summary.disbayes((disbayes:::disbayes(data)), 
                                                                                        vars=c("cf","inc"))
    names(disbayes_output)[index] <- paste0(i)
    
    index <- index + 1
  }
}




## Posterior medians and 95% credible intervals for all unknowns in the model
summ <- disbayes:::summary.disbayes(resu)

## Handy tool to extract specific variables from this 
output <- disbayes:::summary.disbayes(resu, vars=c("cf","inc")) 

## Plot smoothed and unsmoothed estimates 
disbayes:::plot.disbayes(resu)


## Plot just smoothed estimates
plot(summ, variable="cf")


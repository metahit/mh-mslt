### Sort GBD for life table

#### Packages to run code
library(readr)
library(dplyr)
library(stringr)

### 
### RunLocDF calculates population numbers from GBD data and rates. 

RunLocDf <- function(i_data) {
  
  gbd_df <- NULL 
  
  for (ag in 1:length(unique(i_data[["age"]]))){
    for (gender in c("Male", "Female")){
      age_sex_df <- NULL
      for (dm in 1:length(disease_measures_list)){
        for (d in 1:nrow(DISEASE_SHORT_NAMES)){
          dn <- DISEASE_SHORT_NAMES$disease[d]
          dmeasure <- disease_measures_list[dm] %>% as.character()
          
          agroup <- unique(i_data[["age"]])[ag]
          
          idf <- dplyr::filter(i_data, sex == gender & age == agroup & measure == dmeasure & cause == dn) 
          
          if (nrow(idf) > 0){
            
            population_numbers <- dplyr::filter(idf, metric == "Number") %>% dplyr::select("val", "lower", "upper")
            
            idf_rate <- dplyr::filter(idf, metric == "Rate") %>% dplyr::select("val") 
            
            current_idf_rate <- idf_rate
            
            current_population_numbers <- population_numbers
            
            idf$population_number <- 0
            
            if (idf_rate$val != 0 && population_numbers$val != 0)
              idf$population_number <- (100000 * population_numbers$val) / idf_rate$val
            
            else{
              
              current_idf_rate <- idf_rate
              
              current_population_numbers <- population_numbers
              
              idf <- dplyr::filter(i_data, sex == gender & age == agroup & measure == dmeasure & val > 0) 
              
              idf <- dplyr::filter(idf, cause == unique(idf$cause)[1])
              
              idf$cause <- dn
              
              population_numbers <- dplyr::filter(idf, metric == "Number") %>% dplyr::select("val", "lower", "upper")
              #, "lower", "upper")
              
              idf_rate <- dplyr::filter(idf, metric == "Rate") %>% dplyr::select("val") 
              
              idf$population_number <- 0
              
              if (idf_rate$val != 0 && population_numbers$val != 0)
                idf$population_number <- (100000 * population_numbers$val) / idf_rate$val
              
            }
            
            
            idf$rate_per_1 <- round(current_idf_rate$val / 100000, 6)
            
            
            idf[[tolower(paste(dmeasure, "rate", DISEASE_SHORT_NAMES$sname[d], sep = "_"))]] <- idf$rate_per_1
            
            idf[[tolower(paste(dmeasure, "med", DISEASE_SHORT_NAMES$sname[d], sep = "_"))]] <- current_population_numbers$val
            idf[[tolower(paste(dmeasure, "lower95", DISEASE_SHORT_NAMES$sname[d], sep = "_"))]] <- current_population_numbers$lower
            idf[[tolower(paste(dmeasure, "upper95", DISEASE_SHORT_NAMES$sname[d], sep = "_"))]] <- current_population_numbers$upper
            
            
            
            idf <- dplyr::filter(idf, metric == "Number")
            
            if (is.null(age_sex_df)){
              
              age_sex_df <- dplyr::select(idf, age, sex, population_number, location, names(idf)[ncol(idf) - 2], names(idf)[ncol(idf) - 1] , names(idf)[ncol(idf)])
              
              
              names(idf)[ncol(idf)]
              names(idf)[ncol(idf) - 1]
            }
            else{
              
              age_sex_df <- cbind(age_sex_df, dplyr::select(idf, names(idf)[ncol(idf) - 2], names(idf)[ncol(idf) - 1] , names(idf)[ncol(idf)]))
              
              # browser()
            }
          }
        }
      }
      
      if (is.null(gbd_df)){
        
        gbd_df <- age_sex_df
      }
      else{
        
        age_sex_df[setdiff(names(gbd_df), names(age_sex_df))] <- 0
        gbd_df[setdiff(names(age_sex_df), names(gbd_df))] <- 0
        
        gbd_df <- rbind(gbd_df, age_sex_df)
        
        
        
        gbd_df$sex_age_cat <- paste0(gbd_df$sex, gbd_df$age, sep = "_")
        
        
        
        ## Add numberical age categories
        
        gbd_df$age_cat <- 0
        gbd_df$age_cat [ gbd_df$age =="Under 5"] <- 2
        gbd_df$age_cat [ gbd_df$age =="5 to 9"] <- 7
        gbd_df$age_cat [ gbd_df$age =="10 to 14"] <- 12
        gbd_df$age_cat [ gbd_df$age =="15 to 19"] <- 17
        gbd_df$age_cat [ gbd_df$age =="20 to 24"] <- 22
        gbd_df$age_cat [ gbd_df$age =="25 to 29"] <- 27
        gbd_df$age_cat [ gbd_df$age =="30 to 34"] <- 32
        gbd_df$age_cat [ gbd_df$age =="35 to 39"] <- 37
        gbd_df$age_cat [ gbd_df$age =="40 to 44"] <- 42
        gbd_df$age_cat [ gbd_df$age =="45 to 49"] <- 47
        gbd_df$age_cat [ gbd_df$age =="50 to 54"] <- 52
        gbd_df$age_cat [ gbd_df$age =="55 to 59"] <- 57
        gbd_df$age_cat [ gbd_df$age =="60 to 64"] <- 62
        gbd_df$age_cat [ gbd_df$age =="65 to 69"] <- 67
        gbd_df$age_cat [ gbd_df$age =="70 to 74"] <- 72
        gbd_df$age_cat [ gbd_df$age =="75 to 79"] <- 77
        gbd_df$age_cat [ gbd_df$age =="80 to 84"] <- 82
        gbd_df$age_cat [ gbd_df$age =="85 to 89"] <- 87
        gbd_df$age_cat [ gbd_df$age =="90 to 94"] <- 92
        gbd_df$age_cat [ gbd_df$age =="95 plus"] <- 97
        
        ## Change sex variable to lower case
        
        gbd_df$sex <- tolower(gbd_df$sex)
        
        ## Create age_sex category
        
        gbd_df$sex_age_cat <- paste(gbd_df$sex,gbd_df$age_cat, sep = "_"  )
        
        ## Order data
        
        gbd_df <- gbd_df[order(gbd_df$sex, gbd_df$age_cat),]
        
        
      }
    }
  }
  # ## Calculate rates per one. Needed for mslt_code
  
  for (d in 1:nrow(DISEASE_SHORT_NAMES)){
    for (dm in 1:length(disease_measures_list)){
      # dn <- DISEASE_SHORT_NAMES$disease[d]
      dmeasure <- disease_measures_list[dm] %>% as.character() %>% tolower
      

        
        var_rate <- c(paste(tolower(paste(dmeasure, "rate", DISEASE_SHORT_NAMES$sname[d], sep = "_"))))
        var_med <- c(paste(tolower(paste(dmeasure, "med", DISEASE_SHORT_NAMES$sname[d], sep = "_"))))
        
        
        gbd_df[[var_rate]] <- gbd_df[[var_med]] /
          gbd_df$population_number
      
    }
  }
  return(gbd_df)
}

### GenLTDF creates dataframe in one year age (mortality rates)

GenLTDF <- function(i_data) { 
  
  lt_df <- data.frame(age = rep(c(0:100), 2), sex = append(rep("male", 101), 
                                                             rep("female", 101)))
  
  ## Add age groups for cohort modelling
  
  lt_df$age_cat [lt_df$age == 2] <- 2
  lt_df$age_cat [lt_df$age == 7] <- 7
  lt_df$age_cat [lt_df$age == 12] <- 12
  lt_df$age_cat [lt_df$age == 17] <- 17
  lt_df$age_cat [lt_df$age == 22] <- 22
  lt_df$age_cat [lt_df$age == 27] <- 27
  lt_df$age_cat [lt_df$age == 32] <- 32
  lt_df$age_cat [lt_df$age == 37] <- 37
  lt_df$age_cat [lt_df$age == 42] <- 42
  lt_df$age_cat [lt_df$age == 47] <- 47
  lt_df$age_cat [lt_df$age == 52] <- 52
  lt_df$age_cat [lt_df$age == 57] <- 57
  lt_df$age_cat [lt_df$age == 62] <- 62
  lt_df$age_cat [lt_df$age == 67] <- 67
  lt_df$age_cat [lt_df$age == 72] <- 72
  lt_df$age_cat [lt_df$age == 77] <- 77
  lt_df$age_cat [lt_df$age == 82] <- 82
  lt_df$age_cat [lt_df$age == 87] <- 87
  lt_df$age_cat [lt_df$age == 92] <- 92
  lt_df$age_cat [lt_df$age == 97] <- 97
  
  
    lt_df$sex_age_cat <- paste(lt_df$sex, lt_df$age, sep = "_"  )
    
    gbd_popn_df <- dplyr::select(i_data, population_number, sex_age_cat)
    
    lt_df <- left_join(lt_df, gbd_popn_df)
    
    ### Interpolate rates from 5 year age groups to 1
    
    lt_df$mx <- 1
    
    index <- 1  
    
    for(sex_index in c("male", "female")) {
       
          
         
            
          var_name1 <- "deaths_rate_allc"
          
          data <- dplyr::filter(i_data, sex == sex_index) %>% dplyr::select(age, sex, age_cat, sex_age_cat, var_name1)
          
          x <- data$age_cat
          y <- log(data[[var_name1]])
          
          InterFunc <- stats::splinefun(x, y, method = "monoH.FC", ties = mean)
          
          interpolated <- as.data.frame(InterFunc(seq(0, 100, 1)))
          age <- seq(0, 100, by = 1)
          interpolated <- cbind(interpolated, age)
          interpolated[,1] <- exp(interpolated[,1])
          ## Add column with sex to create age_sex category to then merge with input_life table
          interpolated$sex <- paste(sex_index)
          interpolated$sex_age_cat <- paste(interpolated$sex, interpolated$age, sep = "_")
          ## Change name of column death to mx and ylds to pyld_rate to then merge
          ## with input_life table
          colnames(interpolated)[1] <- var_name1

          
          lt_df[lt_df$sex_age_cat == interpolated$sex_age_cat 
                  & lt_df$sex == sex_index, ][["mx"]] <- interpolated[[var_name1]]
          
          index <- index + 1
      
          }
    
      return(lt_df)
  
}

### RunLife

RunLifeTable <- function(in_idata, in_sex, in_mid_age)
{
  
  # Create a life table starting data frame from input data. 
  
  lf_df <- in_idata[in_idata$age >= in_mid_age & in_idata$sex == in_sex,] 
  lf_df <- lf_df[,colnames(lf_df) %in% c('sex', 'age', 'mx')]
  
  # Create list life table variables. First as vector and then added to the data frame at the end.
  ## We model up to 100, that is the reason for the age limit in the function
  
  # probability of dying
  
  qx <-  ifelse(lf_df$age < 100, 1 - exp(-1 * lf_df$mx), 1)
  
  # number of survivors year 1 simulation
  
  num_row <- nrow(lf_df)
  lx <- rep(0,num_row)
  lx[1] <- as.numeric(in_idata$population_number[in_idata$age == in_mid_age & in_idata$sex == in_sex]) 
  
  # number died in year 1 simulation
  
  dx <- rep(0,num_row)
  dx[1] <- lx[1] * qx[1]
  
  # number of survivors and who die from year 2 onwards. 
  
  for (i in 2:num_row){
    lx[i] <- lx[i - 1] - dx[i - 1]
    dx[i] <- lx[i] * qx[i]
  }
  
  # number of persons lived by cohort to age x + 1/2 (average people)
  
  Lx <- rep(0,num_row)
  
  # for years up to 99
  
  for (i in 1:(num_row-1))
    Lx[i] <- (lx[i] + lx[i + 1]) / 2
  
  # for year 100, cohort dies at 100 if anyone left
  
  Lx[num_row] <- lx[num_row] / lf_df$mx[num_row]
  
  
  # create life expectancy variable
  ex <- rep(0,num_row)
  for (i in 1:num_row){
    ex[i] <- sum(Lx[i:num_row]) / lx[i]
  }
  
  # create health adjusted life years variable 
  
  # Lwx <- Lx * (1 - lf_df$pyld_rate)
  
  # create health adjusted life expectancy variable
  # ewx <- rep(0,num_row)
  # for (i in 1:num_row){
  #   ewx[i] <- sum(Lwx[i:num_row]) / lx[i]
  # }
  
  lf_df$qx <- qx
  lf_df$lx <- lx
  lf_df$dx <- dx
  lf_df$Lx <- Lx
  lf_df$ex <- ex
  # lf_df$Lwx <- Lwx
  # lf_df$ewx <- ewx
  lf_df
}
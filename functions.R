######### FUNCTIONS ##########################################################
# This script stores the functions used in the app 
# Author: Shazia Ruybal-Pesántez
##############################################################################

# TODO
# - edit function descriptions

##############################################################################
# readSeroData function
# --------------------------
#
# This function imports the raw data from the Lightcycler 480 machine (eg WEHI)
# and matches the sample names from the plate layout based on their plate/well
# location
#  
#
# PARAMETERS: 
#   - raw_data_file: string with the raw data .txt filename 
#
# OUTPUT:
#   - Data frame with sample-matched qpcr data
##############################################################################

readSeroData <- function(raw_data){
    
    tbl_list <- list()
    
    ## read all data
    full <- read_excel(raw_data$datapath)
    df <- as.data.frame(full)
    
    median_row_number <- which(df$xPONENT == "Median")
    count_row_number <- which(df$xPONENT == "Count")
    endcount_row_number <- which(df$xPONENT == "Avg Net MFI")
    
    tbl_list[[1]] <- df
    
    results <- read_excel(raw_data$datapath, skip = median_row_number+1)

    ## Find all blank rows (i.e. rows that are all NA).
    ## Then keep rows preceding the first blank row.
    blank_row_number <- which(rowSums(is.na(results)) == length(names(results)))[1]
    if(is.na(blank_row_number)){
      results = results
    }else{
      results <- results[1:(blank_row_number-1),]
    }
    
    ## Exclude column that corresponds to "Total events"
    results <- results %>% dplyr::select(-`Total Events`)
    
    # Change "NaN" to 0s
    results <- results %>% mutate_all(funs(gsub("NaN", 0, .)))
    
    tbl_list[[2]] <- results
    
    ##########
    ## Load the counts to check for run quality control  
    counts <- read_excel(raw_data$datapath, skip = count_row_number+1, n_max = endcount_row_number-count_row_number-2, col_names = T)
    counts <- as_tibble(counts)
  
  tbl_list[[3]] <- counts
  
  ##########
  ## Save the MFI values for the blank sample(s) for run quality control
  blanks <- results %>% dplyr::filter(grepl("Blank", Sample, ignore.case = T))
  
  tbl_list[[4]] <- blanks
  
  ##########
  ## Save the MFI values for the standards for run quality control
  stds <- results %>% dplyr::filter(grepl("^S", Sample, ignore.case = T))
  
  tbl_list[[5]] <- stds
  
  ##########
  
  run <- read_excel(raw_data$datapath, n_max = median_row_number)
  run <- as.data.frame(run)
  
  run <- run %>% dplyr::select(Program:xPONENT)
  
  tbl_list[[6]] <- run 
  
  return(tbl_list)
}

##############################################################################
# readPlateLayout function
# --------------------------
#
# This function imports the plate layout 
#  
#
# PARAMETERS: 
#   - plate_layout_file: string with the plate layout .xlsx filename 
#
# OUTPUT:
#   - Data frame with sample-matched qpcr data
##############################################################################

readPlateLayout <- function(plate_layout){
 
  p <- read_excel(plate_layout$datapath)
  p <- as.data.frame(p)
  
  return(p)
}


##############################################################################
# getCounts function
# --------------------------
#
# This function gets the count data 
#  
#
# PARAMETERS: 
#   - raw_data_file: string with the raw data .xlsx filename 
#
# OUTPUT:
#   - Data frame with sample-matched qpcr data
##############################################################################

getCounts <- function(raw_data){
  
  counts <- readSeroData(raw_data)[[3]]
  
  counts <- counts %>%
    clean_names() %>% 
    dplyr::select(-c(sample, total_events)) %>% # can maybe "keep" relevant columns + protein names?
    dplyr::mutate(location=gsub(".*,", "", location)) %>%
    dplyr::mutate(location=substr(location, 1, nchar(location)-1))  %>% 
    tidyr::pivot_longer(-location, names_to = "protein", values_to = "count") %>% 
    dplyr::mutate(warning = case_when(
      count<15~1,
      count>=15~0
    )) %>%
    dplyr::select(location, warning) %>%
    dplyr::group_by(location) %>%
    dplyr::summarise(sum = sum(warning)) %>%
    dplyr::mutate(colour = case_when(
      sum>=1 ~ "repeat",
      sum<1 ~ "sufficient beads"
    )) %>%
    dplyr::mutate(row = substr(location, 1, nchar(location)-1)) %>%
    dplyr::mutate(col = substr(location, 2, nchar(location))) %>%
    dplyr::mutate(row = gsub("1", "", row)) %>%
    dplyr::mutate(row = as.factor(row)) %>%
    dplyr::mutate(col = as.numeric(col))
  
  return(counts)
}

##############################################################################
# plotCounts function
# --------------------------
#
# This function gets the count data and plots the plate image
#  
#
# PARAMETERS: 
#   - raw_data_file: string with the raw data .xlsx filename 
#
# OUTPUT:
#   - Data frame with sample-matched qpcr data
##############################################################################

plotCounts <- function(raw_data, experiment_name){
  bead_counts <- getCounts(raw_data)
  bead_counts %>% 
    ggplot(mapping = aes(x = col, y = fct_rev(row), fill = colour), fill = summary)+
    geom_tile(aes(height = 0.90, width = 0.90)) +
    scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                       position = "top")+
    scale_fill_manual(values = c("sufficient beads" = "#91bfdb", "repeat" = "#d73027"))+
    theme_linedraw()+
    labs(x = "", y = "", title = experiment_name , fill = "")
}


##############################################################################
# plotBlanks function
# --------------------------
#
# This function gets the blank sample data and plots the blank sample MFI values
#  
#
# PARAMETERS: 
#   - raw_data_file: string with the raw data .xlsx filename 
#   - experiment_name: string with the experimet name (reactive)
#
# OUTPUT:
#   - Data frame with sample-matched qpcr data
##############################################################################

plotBlanks <- function(raw_data, experiment_name){
  blanks <- readSeroData(raw_data)[[4]]
  blanks %>% 
    dplyr::select(-Location) %>% 
    pivot_longer(-Sample, names_to = "protein", values_to = "MFI") %>% 
    ggplot(aes(x = factor(protein), y = as.numeric(MFI), fill = Sample)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_hline(yintercept = 50, linetype = "dashed", color = "grey") +
    labs(x = "protein", 
         y = "MFI",
         title = experiment_name) +
    theme_linedraw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
}

##############################################################################
# plotStds function
# --------------------------
#
# This function gets the standards data and plots the standard curves
#  
#
# PARAMETERS: 
#   - raw_data_file: string with the raw data .xlsx filename 
#   - experiment_name: string with the experimet name (reactive)
#
# OUTPUT:
#   - Data frame with sample-matched qpcr data
##############################################################################

plotStds <- function(raw_data, experiment_name){
  stds <- readSeroData(raw_data)[[5]]
  stds %>% 
    dplyr::select(-Location) %>% 
    pivot_longer(-Sample, names_to = "protein", values_to = "MFI") %>% 
    mutate(Sample = factor(Sample, c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10"))) %>% 
    mutate(MFI = as.numeric(MFI)) %>% 
    ggplot(aes(x = Sample, y = MFI, color = protein, group = protein)) + 
    geom_point() + 
    geom_line() +
    scale_y_log10(breaks = c(0, 10, 100, 1000, 10000)) +
    labs(x = "standard curve", 
         y = "log(MFI)",
         title = experiment_name) +
    facet_wrap(~protein) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
}

##############################################################################
# runModel function
# --------------------------
#
# This function fits a 5-parameter logistic standard curve to the dilutions
# of the positive controls for each protein and converts the MFI values 
# into relative antibody units (RAU)
# Written by Connie Li Wai Suen
#
# PARAMETERS: 
#   - raw_data_file: string with the raw data .xlsx filename 
#   - plate_layout_file: string with the plate layout .xlsx filename 
#   - experiment_name: string with the experiment name (reactive)
#
# OUTPUT:
#   - Data frame with converted RAU data
##############################################################################

runModel <- function(raw_data, plate_layout){
  L <- readSeroData(raw_data)[[2]]
  layout <- readPlateLayout(plate_layout)
  
  ## Protein name list obtained after removing variable names: "Location" and "Sample"
  proteins <- names(L[, -c(1:2)]); proteins
  ## Add new variable to data frame to indicate the first letter of sample type ("B", "C", "S", "U")
  ## "B" = blank, "C"=control, "S"=standard (dilution of the pool), "U"=sample
  L$type.letter <- substr(L$Sample, start=1, stop=1)
  dilution <- c(1/50, 1/100, 1/200, 1/400, 1/800, 1/1600, 1/3200, 1/6400, 1/12800, 1/25600)
  dilution.scaled <- dilution*25600; dilution.scaled
  
  ##########################################################################################################
  #### LOG-LOG MODEL 
  ##########################################################################################################
  
  results.df.wide <- NULL
  model_list <- NULL
  for (i in proteins){
    results.df <- NULL
    ## Taking the mean of duplicates for each standard
    ## and storing in object std in the following order: S1, S2, S3, ..., S9, S10.
    std <- NULL
    b <- c <- d <- e <- NULL
    for (r in 1:nrow(L)){
      if (L$type.letter[r]=="S"){
        std <- c(std, as.numeric(L[r,i])) 
      }
    }
    
    ## Log-log model to obtain a more linear relationship
    ## and therefore make it easier to interpolate around the lower asymptote.
    log.std <- log(as.numeric(std)); log.std
    
    ## Five-parameter logistic function is given by the expression:
    ## f(x) = c + \frac{d-c}{(1+\exp(b(\log(x)-\log(e))))^f}
    model1 <- drm(log.std ~ dilution, fct=LL.5(names=c("b", "c", "d", "e", "f")))
    summary(model1)
    
    ## Save output of model for each protein
    model_list[[i]] <- model1
    
    #Sys.sleep(0.1)  ## Suspends execution for 0.1 second to prevent RStudio errors when plotting within the loop.
    #plot(model1, main=i)
    
    ## F(x) = ((A-D)/(1+((x/C)^B))) + D    ## where A=minimum asymptote, B=Hill slope, C=ED50, D=Maximum asymptote
    ## x = C*(((A-D)/(F(x)-D))-1)^(1/B) = e*(((c-d)/(log(mfi.X)-d))-1)^(1/b)
    b <- coef(summary(model1))[1]; b  ## slope
    c <- coef(summary(model1))[2]; c  ## lower asymptote
    d <- coef(summary(model1))[3]; d  ## upper asymptote
    e <- coef(summary(model1))[4]; e  ## ED50
    f <- coef(summary(model1))[5]; f  ## asymmetry parameter (f=1 for 4PL curves)
    
    
    ##########################################################################################################
    #### MFI TO RAU CONVERSION
    ##########################################################################################################
    
    for (r in 1:nrow(L)){
      results <- NULL
      if (L$type.letter[r]=="U"){
        mfi.X <- as.numeric(L[r, i])
        y <- log(mfi.X)
        
        if (y > max(log.std)) {
          dil.X <- max(dilution)
        } else {
          dil.X <- e*(( ((d-c)/(y-c))^(1/f) - 1 )^(1/b) )
        }
        dil.X <- ifelse(dil.X > 0.02, 0.02, dil.X)
        dil.X <- ifelse((is.na(dil.X) & y>log.std[2]), 0.02, dil.X)       ## Setting observations with very high MFI to 1/50.
        dil.X <- ifelse(dil.X < 1/51200, 1/51200, dil.X)
        dil.X <- ifelse((is.na(dil.X) & y<max(log.std)), 1/51200, dil.X)  ## Setting observations with very low MFI to 1/51200.
        location.X  <- L[r, "Location"]
        sample.X  <- L[r, "Sample"]
        results <- cbind(Location=location.X, Sample=sample.X, MFI=mfi.X, Dilution=dil.X, DilutionReciprocal=1/dil.X, MinStd=min(std), MaxDilution=min(dilution), MaxStd=max(std), MinDilution=max(dilution))
        results.colnames <- c("Location", "Sample", paste0(i, "_", c("MFI", "Dilution", "DilutionReciprocal", "MinStd", "MaxDilution", "MaxStd", "MinDilution")))
        colnames(results) <- results.colnames
      }
      results.df <- rbind(results.df, results) 
    }
    if (is.null(results.df.wide)){
      results.df.wide <- results.df
    } else { results.df.wide <- merge(results.df.wide, results.df, by=c("Location", "Sample")) }
  }
  
  ##########################################################################################################
  #### MODEL RESULTS AND PLOTS
  ##########################################################################################################
  
  ## Save all model results
  model_results <- NULL
  for (i in names(model_list)){
    title <- as.character(i)
    model_results[[i]] <- plot(model_list[[i]], main = title) 
  }

  ##########################################################################################################
  #### MERGE DATA
  ##########################################################################################################
  
  ## Bind to location
  results.df.wide <- as.data.frame(results.df.wide)
  results.location <- matrix(unlist(strsplit(as.character(results.df.wide$Location), ",")), ncol=2, byrow=T)[,2]
  results.location <- substr(results.location, 1, nchar(results.location)-1)
  results.df.wide <- cbind(Location.2=results.location, results.df.wide)
  
  ## Matching SampleID from plate layout to corresponding sample.
  location.1 <- matrix(unlist(strsplit(L$Location, ",")), ncol=2, byrow=T)[,2]
  location.1 <- substr(location.1, 1, nchar(location.1)-1)
  location.2 <- data.frame(Location.2=location.1, alpha=gsub("[[:digit:]]", "", location.1), numeric=gsub("[^[:digit:]]", "", location.1), SampleID=NA, stringsAsFactors = FALSE)
  for (i in location.2[, "Location.2"]){
    location.2[location.2$Location.2==i, "SampleID"] <- layout[layout$Plate==location.2[location.2$Location.2==i, "alpha"], colnames(layout)==location.2[location.2$Location.2==i, "numeric"]]
  }
  
  ## Using join() from plyr package to add SampleID information to results.df.wide. (default or given folder location and unique name)
  results.df.wide <- plyr::join(results.df.wide, location.2[,c("Location.2", "SampleID")], by="Location.2", type="left") 
  
  ## Move SampleID to first column
  results.df.wide <- results.df.wide[, c("SampleID", colnames(results.df.wide)[!(colnames(results.df.wide) %in% "SampleID")])]
  
  # Make all columns after 1st 4 numeric
  results.df.wide[,5:ncol(results.df.wide)] = lapply(results.df.wide[,5:ncol(results.df.wide)], as.character)
  results.df.wide[,5:ncol(results.df.wide)] = lapply(results.df.wide[,5:ncol(results.df.wide)], as.numeric)
  
  ##########################################################################################################
  #### OUTPUT
  ##########################################################################################################
  
  #### Save just MFI and RAU for downstream analyses
  col_selection <- grepl("SampleID|_MFI|\\_Dilution$", colnames(results.df.wide))
  #colnames(results.df.wide)[col_selection]
  MFI_RAU_results <- results.df.wide[, col_selection]
  
  # Output
  return(list(results.df.wide, MFI_RAU_results, model_results))
}

##############################################################################
# plotModel function
# --------------------------
#
# This function gets the model results data and plots the model fits
#  
#
# PARAMETERS: 
#   - raw_data_file: string with the raw data .xlsx filename 
#   - plate_layout_file: string with the plate layout .xlsx filename 
#
# OUTPUT:
#   - Data frame with sample-matched qpcr data
##############################################################################

plotModel <- function(raw_data, plate_layout){
  model_results <- runModel(raw_data, plate_layout)[[3]]
  
  plots_model <- lapply(seq_along(model_results), function(x){
    ggplot(data = model_results[[x]], 
           aes(x = dilution, y = `1`)) + 
      geom_line() +
      scale_x_log10(breaks = c(1e-5, 1e-4, 1e-3, 1e-2, 0.03),
                    labels = c("0.00001", "0.0001", "0.001", "0.01", "0.03")) +
      labs(x = "antibody dilution",
           y = "standard curve",
           title = names(model_results[x])) +
      theme_bw()
  })
  
  gridExtra::grid.arrange(grobs = plots_model, nrow = 3)
}


##############################################################################
# classifyExposure function
# --------------------------
#
# This function classifies unknown samples as recently exposed or not 
# (Note: runModel() needs to be run first to convert to RAU)
#  
#
# PARAMETERS: 
#   - raw_data_file: string with the raw data .xlsx filename 
#   - algorithm: user-selected algorithm choice
#
# OUTPUT:
#   - Data frame with exposure status for every sample
#   - Summary table with positive/negative results for each classifier
##############################################################################

classifyExposure <- function(raw_data, plate_layout, classifier1, classifier2, classifier3){
  data <- runModel(raw_data, plate_layout)[[2]]
  
  rf1 <- classifier1
  rf2 <- classifier2
  rf3 <- classifier3

  data <- data %>%  rename_at(vars(contains("CSpike", ignore.case = T) & ends_with("_Dilution", ignore.case = T)),
                              ~"CSpikeH_AW_Dilution") %>%
                    rename_at(vars(contains("CRBD", ignore.case = T) & ends_with("_Dilution", ignore.case = T)),
                              ~"CRBD_WT_Dilution") %>%
                    rename_at(vars(contains("CNPH", ignore.case = T) & ends_with("_Dilution", ignore.case = T)),
                              ~"CNPH_AC_Dilution") %>%
                    rename_at(vars(contains("CS2", ignore.case = T) & ends_with("_Dilution", ignore.case = T)),
                              ~"CS2H_NA_Dilution") %>%
                    rename_at(vars(contains("CS1", ignore.case = T) & ends_with("_Dilution", ignore.case = T)),
                              ~"CS1H_NA_Dilution") %>%
                    rename_at(vars(contains("OC43", ignore.case = T) & ends_with("_MFI", ignore.case = T)),
                              ~"OC43Spike_S_MFI") %>%
                    rename_at(vars(contains("NL63", ignore.case = T) & ends_with("_MFI", ignore.case = T)),
                              ~"NL63NPEc_P_MFI") %>%
                    rename_at(vars(contains("HKU", ignore.case = T) & ends_with("_MFI", ignore.case = T)),
                              ~"HKU1S1H_AW_MFI") %>%
                    rename_at(vars(contains("229", ignore.case = T) & ends_with("_MFI", ignore.case = T)),
                              ~"P229ES1H_S_MFI")

  data %>% dplyr::select(SampleID, CS1H_NA_Dilution, P229ES1H_S_MFI, CS2H_NA_Dilution, CNPH_AC_Dilution,CRBD_WT_Dilution, HKU1S1H_AW_MFI, CSpikeH_AW_Dilution,OC43Spike_S_MFI,NL63NPEc_P_MFI)
  
  # cat("------- Antigen names have been renamed. Starting classification....\n")
  
  predict <- data %>% select(SampleID)

  predict <- mutate(predict,`Prediction all` = predict(rf1, data))
  predict <- mutate(predict,`Prediction less than 3 months` = predict(rf2, data))
  predict <- mutate(predict,`Prediction greater than 3 months`= predict(rf3, data))

  # cat("------- Prediction has completed....\n")
  data_sero <- data %>% dplyr::select(SampleID) %>% left_join(predict, by = "SampleID")

  return(data_sero)
}

##############################################################################
# plotBoxplotMFI function
# --------------------------
#
# This function plots the MFI values for each protein 
#  
#
# PARAMETERS: 
#   - raw_data_file: string with the raw data .xlsx filename 
#   - experiment_name: string with the experiment name (reactive)
#
# OUTPUT:
#   - Box plots with MFI values for each protein
##############################################################################

plotBoxplotMFI <- function(raw_data, plate_layout){
  
  data <- runModel(raw_data, plate_layout)[[2]]
  
  data %>% 
    select(SampleID, ends_with("_MFI")) %>%
    pivot_longer(-SampleID, names_to = "protein", values_to = "dilution") %>%
    ggplot(aes(x= protein, y = dilution, fill = protein)) +
    geom_boxplot() +
    scale_y_log10(breaks = c(0, 10, 100, 1000, 10000)) + 
    scale_fill_brewer(palette = "Paired", type = "qual") +
    labs(x = "protein",
         y = "MFI") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
}

##############################################################################
# plotBoxplotRAU function
# --------------------------
#
# This function plots the RAU values for each protein 
#  
#
# PARAMETERS: 
#   - raw_data_file: string with the raw data .xlsx filename 
#   - experiment_name: string with the experiment name (reactive)
#
# OUTPUT:
#   - Box plots with MFI values for each protein
##############################################################################

plotBoxplotRAU <- function(raw_data, plate_layout){
  
  data <- runModel(raw_data, plate_layout)[[2]]
  
  data %>% 
    select(SampleID, ends_with("_Dilution")) %>%
    pivot_longer(-SampleID, names_to = "protein", values_to = "dilution") %>%
    ggplot(aes(x= protein, y = dilution, fill = protein)) +
    geom_boxplot() +
    scale_y_log10(breaks = c(1e-5, 1e-4, 1e-3, 1e-2, 0.03),
                  labels = c("0.00001", "0.0001", "0.001", "0.01", "0.03")) +
    scale_fill_brewer(palette = "Paired", type = "qual") +
    labs(x = "protein",
         y = "Antibody dilution") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
}

##############################################################################
# summaryClassification function
# --------------------------
#
# This function classifies unknown samples as recently exposed or not 
# (Note: runModel() needs to be run first to convert to RAU)
#  
#
# PARAMETERS: 
#   - raw_data_file: string with the raw data .xlsx filename 
#   - algorithm: user-selected algorithm choice
#
# OUTPUT:
#   - Summary table with positive/negative results for each classifier
##############################################################################

# summaryClassification <- function(raw_data, plate_layout, classifier1, classifier2, classifier3){
#   data <- runModel(raw_data, plate_layout)[[2]]
#   
#   data <- data %>%  rename_at(vars(contains("CSpike", ignore.case = T) & ends_with("_Dilution", ignore.case = T)), 
#                               ~"CSpikeH_AW_Dilution") %>% 
#     rename_at(vars(contains("CRBD", ignore.case = T) & ends_with("_Dilution", ignore.case = T)), 
#               ~"CRBD_WT_Dilution") %>% 
#     rename_at(vars(contains("CNPH", ignore.case = T) & ends_with("_Dilution", ignore.case = T)), 
#               ~"CNPH_AC_Dilution") %>% 
#     rename_at(vars(contains("CS2", ignore.case = T) & ends_with("_Dilution", ignore.case = T)), 
#               ~"CS2H_NA_Dilution") %>% 
#     rename_at(vars(contains("CS1", ignore.case = T) & ends_with("_Dilution", ignore.case = T)), 
#               ~"CS1H_NA_Dilution") %>% 
#     rename_at(vars(contains("OC43", ignore.case = T) & ends_with("_MFI", ignore.case = T)), 
#               ~"OC43Spike_S_MFI") %>% 
#     rename_at(vars(contains("NL63", ignore.case = T) & ends_with("_MFI", ignore.case = T)), 
#               ~"NL63NPEc_P_MFI") %>% 
#     rename_at(vars(contains("HKU", ignore.case = T) & ends_with("_MFI", ignore.case = T)), 
#               ~"HKU1S1H_AW_MFI") %>% 
#     rename_at(vars(contains("229", ignore.case = T) & ends_with("_MFI", ignore.case = T)), 
#               ~"P229ES1H_S_MFI")
#   
#   data %>% select(SampleID, CS1H_NA_Dilution, P229ES1H_S_MFI, CS2H_NA_Dilution, CNPH_AC_Dilution,CRBD_WT_Dilution, HKU1S1H_AW_MFI, CSpikeH_AW_Dilution,OC43Spike_S_MFI,NL63NPEc_P_MFI)  
#   
#   predict <- data %>% select(SampleID)
#   
#   predict <- mutate(predict,`Prediction all` = predict(classifier1, data)) 
#   predict <- mutate(predict,`Prediction 3 months` = predict(classifier2, data)) 
#   predict <- mutate(predict,`Prediction greater 3 months`= predict(classifier3, data)) 
#   
#   total_results<-gather(predict,`PredictionType`,`result`,2:4) %>% group_by(`PredictionType`,`result`) %>% summarise(n=n(),.groups = "keep")
#   
#   total_results<-spread(total_results,PredictionType,n)
#   
#   total_results<-total_results[,c(1,3,2,4)]
#   total_results[total_results=="negative"] = "Negative"
#   total_results[total_results=="positive"] = "Positive"
#   
#   return(total_results)
# }

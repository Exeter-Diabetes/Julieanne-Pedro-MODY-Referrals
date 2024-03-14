
library(xlsx)
library(tidyverse)

Investigate_rules_patients_old <- read.xlsx("Investigate_missingness_patients_old.xlsx", 1) %>%
  distinct(MODYNo, .keep_all = TRUE)

Investigate_rules_patients <- read.xlsx("Investigate_missingness_patients_new.xlsx", 1) %>%
  distinct(MODYNo, .keep_all = TRUE)


new_table <- merge_tables(Investigate_rules_patients_old, Investigate_rules_patients)


write.xlsx(new_table, "Investigate_rules_patients_updated.xlsx", row.names = FALSE, showNA = FALSE)


merge_tables <- function(old_table, new_table) {
  
  ## load libraries
  require(tidyverse)

  ## Make sure MODYNo is included in both tables
  if (is.null(old_table$MODYNo)) {stop("'old_table' should include the MODYNo column")}
  if (is.null(new_table$MODYNo)) {stop("'new_table' should include the MODYNo column")}
  ## Make sure Comment is included in old_table
  if (is.null(old_table$Comment)) {stop("'old_table' should include the Comment column")}
  ## Make sure Checked is included in both tables
  if (is.null(old_table$Checked)) {stop("'old_table' should include the Checked column")}
  if (is.null(new_table$Checked)) {stop("'new_table' should include the Checked column")}


  #:------------------------------------
  ## collect patient id's
  
  # patients in old but not on new
  patient_id_old <- which(!(old_table$MODYNo %in% new_table$MODYNo))
  
  # patients in old and new
  patient_id_both <- which(old_table$MODYNo %in% new_table$MODYNo)

  # patients in new but not in old
  patient_id_new <- which(!(new_table$MODYNo %in% old_table$MODYNo))



  #:------------------------------------
  ## Start building newer table with the combined information from both tables
  
  ## Take patients only on old table
  dataset_new <- old_table %>%
    slice(patient_id_old) %>%
    select(-Comment)
  
  ## Take patients on both the old and new table
  for (i in patient_id_both) {
    
    # check if patient has the same entries
    if (identical(
      
      old_table %>%
      slice(i) %>%
      select(colnames(dataset_new), -Checked),
      
      Investigate_rules_patients %>%
      filter(MODYNo == old_table$MODYNo[i]) %>%
      select(colnames(dataset_new), -Checked)
      
    ) == TRUE) {
      
      # if true, just add old one to the dataset
      dataset_new <- dataset_new %>%
        rbind(
          old_table %>%
            slice(i) %>%
            select(colnames(dataset_new))
        )
      
    } else {
      
      # if false, just add new one to the dataset
      dataset_new <- dataset_new %>%
        rbind(
          new_table %>%
            filter(MODYNo == old_table$MODYNo[i]) %>%
            select(colnames(dataset_new))
        )
      
    }
  }

  ## Take patients only on new table
  dataset_new <- dataset_new %>%
    rbind(
      new_table %>%
        slice(patient_id_new) %>%
        select(colnames(dataset_new))
    )
  
  
  
  ## Add additional variables
  # if there are new variables
  if ((ncol(old_table)-2) != (ncol(new_table)-1)) {
    dataset_new <- dataset_new %>%
      left_join(
        new_table %>% 
          select(-colnames(dataset_new), "MODYNo"),
        by = c("MODYNo")
      )
  }
  
  # add old comments
  dataset_new <- dataset_new %>%
    left_join(
      old_table %>%
        select("MODYNo", "Comment"),
      by = c("MODYNo")
    ) %>%
    select(c(colnames(new_table), "Comment")) %>%
    arrange(Checked)
  
  
  
  dataset_final <- dataset_new[rowSums(is.na(dataset_new[,c(3,5,6,7,8,9,10,11,12)])) == 9,] %>%
    drop_na("Comment") %>%
    rbind(
      dataset_new[rowSums(is.na(dataset_new[,c(3,5,6,7,8,9,10,11,12)])) < 9,]
    )
  
  
  #:------------------------------------
  ## Return final table
  return(dataset_final)
  
}














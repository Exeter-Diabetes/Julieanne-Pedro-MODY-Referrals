library(tidyverse)

Investigate_rules_patients_old <- read.csv("Investigate_rules_patients_old.csv") %>%
  arrange(MODYNo)

Investigate_rules_patients <- read.csv("Investigate_rules_patients.csv") %>%
  arrange(MODYNo)


#:------- Patient ids

# patients in old but not on new
patient_id_old <- which(!(Investigate_rules_patients_old$MODYNo %in% Investigate_rules_patients$MODYNo))

# patients in old and new
patient_id_both <- which(Investigate_rules_patients_old$MODYNo %in% Investigate_rules_patients$MODYNo)

# patients in new but not in old
patient_id_new <- which(!(Investigate_rules_patients$MODYNo %in% Investigate_rules_patients_old$MODYNo))


### Start creating the new dataset

# patients in old
dataset_new <- Investigate_rules_patients_old %>%
  slice(patient_id_old) %>%
  select(-Comment)


for (i in patient_id_both) {

  # check if patient has the same entries
  if (identical(
    Investigate_rules_patients_old %>%
      slice(i) %>%
      select(colnames(dataset_new), -Checked),
    Investigate_rules_patients %>%
      slice(i) %>%
      select(colnames(dataset_new), -Checked)
  ) == TRUE) {
    
    # if true, just add old one to the dataset
    dataset_new <- dataset_new %>%
      rbind(
        Investigate_rules_patients_old %>%
          slice(i) %>%
          select(colnames(dataset_new))
      )
    
  } else {
    
    # if false, just add new one to the dataset
    dataset_new <- dataset_new %>%
      rbind(
        Investigate_rules_patients %>%
          slice(i) %>%
          select(colnames(dataset_new))
      )
    
  }
}



# patients in new
dataset_new <- dataset_new %>%
  rbind(
    Investigate_rules_patients %>%
      slice(patient_id_new) %>%
      select(colnames(dataset_new))
  )



## Add additional variables and old comments
# if there are new variables
if ((ncol(Investigate_rules_patients_old)-2) != (ncol(Investigate_rules_patients)-1)) {
  dataset_new <- dataset_new %>%
    left_join(
      Investigate_rules_patients %>% 
        select(-colnames(dataset_new), "MODYNo"),
      by = c("MODYNo")
    )
}

# add old comments
dataset_new <- dataset_new %>%
  left_join(
    Investigate_rules_patients_old %>%
      select("MODYNo", "Comment"),
    by = c("MODYNo")
  ) %>%
  select(c(colnames(Investigate_rules_patients), "Comment")) %>%
  arrange(Checked)


write.csv(dataset_new, "new_Investigate_rules_patients.csv", row.names = FALSE)

#' Formatting Referral data
#'
#' This file contains the function needed for formatting the referral data.
#'
#' @param dataset original unformatted referral dataset
#' @param dataset.case_control case-control dataset which patients that need to be removed
#' @param ethnicity_groups table containing the sorted ethnicities into all groups
#' @param ethnicity_labels table containing the conversion labels for each of the ethnicity groups
#' @param diagnosis TRUE or FALSE flag on whether excluded patient numbers should be printed
#' @param type 'Type 1' or 'Type 2' variable to help define the dataset
#' @param ethnicity 'White' or 'Non-White' variable to help define the ethnicity
#' @param gene_type 'Primary' or 'Secondary' variable specifying the genes being tested (other genes could be sensitivity analysis)
#' @param proband 'Proband' or 'All' variable to help select the patients needed
#' @param investigate 'True' or 'False' variable which when TRUE, ignores the variables type, 
#'            ethnicity and proband and outputs a table with rows for individual patients and 
#'            columns for each of the variables that should be investigated in the records.
#'
#' @return dataset ready for analysis
#'
#'
#' @examples
#' \dontrun{
#' 
#' ## load the excel tables like so (so that it understands NA properly:
#' library(readxl)
#' ethnicity_groups <- read_excel("ethnicity_groups.xlsx", na = "null")
#' ethnicity_labels <- read_excel("ethnicity_labels.xlsx", na = "null")
#' }
#'
#' @export
formatting <- function(dataset, dataset.case_control, ethnicity_groups, ethnicity_labels, diagnosis = FALSE, type = NULL, ethnicity = NULL, gene_type = "Primary", proband = NULL, investigate = FALSE) {
  
  ### Function checks
  ## Ensure type of diabetes is chosen
  if (is.null(type)) {stop("'type' of diabetes must be defined: 'Type 1' or 'Type 2'.")}
  if (!(type %in% c("Type 1", "Type 2"))) {("'type' of diabetes must be defined: 'Type 1' or 'Type 2'.")}
  ## Ensure ethnicity is chosen 
  if (is.null(ethnicity)) {stop("'ethnicity' must be defined: 'White' or 'Non-White'.")}
  if (!(ethnicity %in% c("White", "Non-White"))) {("'ethnicity' must be defined: 'White' or 'Non-White'.")}
  ## Ensure gene_type is chosen 
  if (is.null(gene_type)) {stop("'gene_type' must be defined: 'Primary' or 'Secondary'.")}
  if (!(gene_type %in% c("Primary", "Secondary"))) {("'gene_type' must be defined: 'Primary' or 'Secondary'.")}
  ## Ensure proband is chosen correctly
  if (is.null(proband)) {stop("'proband' must be defined: 'Proband' or 'All'.")}
  if (!(proband %in% c("Proband", "All"))) {("'proband' must be defined: 'Proband' or 'All'.")}
  ## Ensure investigate is chosen correctly
  if (is.null(investigate)) {stop("'investigate' must be defined: 'TRUE' or 'FALSE'.")}
  if (!(investigate %in% c("TRUE", "FALSE"))) {("'investigate' must be defined: 'TRUE' or 'FALSE'.")}
  
  ### Libraries needed
  require(tidyverse)
  
  
  #:------------------------------------------------------------------------
  ####
  ####    Exclusion procedure
  ####
  
  ### set up dataset for change
  dataset_formatted <- dataset
  
  ### set up dataset for investigate
  if (investigate == TRUE) {
    dataset_investigate <- dataset_formatted %>%
      select(MODYNo)
  }
  
  
  ###
  # Make sure there are no repeated rows
  if (length(unique(dataset_formatted$MODYNo)) != nrow(dataset_formatted)) {
    stop("There are repeated individuals based on the MODY number")
  }
  
  ###
  # Make sure to remove case-control patients
  if (sum(dataset_formatted$MODYNo %in% dataset.case_control$id) > 0) {
    dataset_formatted <- dataset_formatted %>%
      filter(!(MODYNo %in% dataset.case_control$id))
  }
  
  ###
  
  #:----- Age range
  
  ## Nothing needs to be done here as the dataset is already in the range needed
  
  if (diagnosis == TRUE) {
    print("###### Age of diagnosis #######")
    print("######## Range 1 - 35 #########")
    print(summary(dataset_formatted$`Age Diag`))
  }
  
  
  #:----- Status specification
  
  #####
  # Just a check for whether there is a new type of reported status that we haven't seen yet
  # This is being added because we will be rerunning this on newer versions of the dataset which may have new status
  status_test <- dataset_formatted$Status %in% c("Autoimmune disease", "Autoimmune Disease", "Diabetes & Deafness", "Diabetes & liver adenomas", "Diabetes & Renal Cysts", 
                                                 "Diabetes & Renal Developmental abnormality", "Diabetes & Renal Developmental Abnormality", "Diabetes & Renal Disease",
                                                 "Diabetes and Lipodystrophy", "Diabetic", "Exercise induced hypoglycaemia", "FBG > 6.0", "FBG > 6.0\"", "GDM", 
                                                 "Genital Tract Malformations", "Genital Tract Malformations & Diabetes", "HbA1c > 39", "HbA1c > 39 mmol/mol", 
                                                 "HbA1C > 39 mmol/mol", "HbA1c > 39mmol/mmol", "Hba1c > 39mmol/mol", "HbA1c > 39mmol/mol", "HbA1c > 39mol/mol", 
                                                 "HbA1c > 42 mmol/mol", "HbA1c >37", "HbA1c >38mmol/mol", "HbA1c >39", "HbA1c >39mmol/l", "HbA1c >39mmol/mol", 
                                                 "HbA1c >40mmol/mol", "Hba1c 38mmol/mol", "HbA1c 38mmol/mol", "Hyperinsulinism", "hyperinsulinism progressed to diabetes", 
                                                 "Hyperinsulinism progressed to diabetes", "Hypoglycaemia", "Hypoglycaemia progressed to diabetes", 
                                                 "Hypoglycaemia progressed to IGT", "Hypoglycaemia progressed to Remitting diabetes", "Impaired fasting glycaemia", 
                                                 "Impaired Glucose Tolerance", "Insulin Resistance", "Insulinoma", "IPEX", "Kabuki", "Kabuki syndrome", 
                                                 "Ketosis prone diabetes", "Ketosis Prone diabetes", "Lipodystrophy", "MDP Syndrome", "nephrocalcinosis & diabetes", 
                                                 "Not Diabetic", "PNDM", "Raised HbA1c", "Raised HbA1c >5.7", "Relapsing/Remitting Diabetes", "Remitting Diabetes", 
                                                 "Renal Cysts & Diabetes", "Renal Cysts no Diabetes", "Renal Developmental abnormality", "Renal Developmental Abnormality", 
                                                 "Renal X", "TNDM", "TRMA", "Unaffected", "Unknown", "Wolfram Syndrome", NA)
  
  # If there are other status, stop the function so that it can be fixed
  if (sum(status_test) != nrow(dataset_formatted)) {
    print("There is new status which were not considered:")
    print(unique(dataset_formatted$Status[!status_test]))
    stop()
  }
  ####
  
  # Set 'Keep' or 'Remove' to those with the status we want/don't, respectively
  dataset_formatted <- dataset_formatted %>%
    mutate(Status_interim = ifelse(!(Status %in% c("Diabetic", "GDM", "FBG > 6.0", "Impaired Glucose Tolerance", "FBG > 6.0\"", "Impaired fasting glycaemia",
                                                   "Renal Cysts & Diabetes", "Diabetes & Renal Cysts", "Hyperinsulinism progressed to diabetes", "hyperinsulinism progressed to diabetes",
                                                   "Autoimmune disease", "Autoimmune Disease", "Diabetes & Deafness", "Diabetes & liver adenomas",
                                                   "Diabetes & Renal Developmental abnormality", "Diabetes & Renal Developmental Abnormality", 
                                                   "Diabetes & Renal Disease", "Diabetes and Lipodystrophy", "Genital Tract Malformations & Diabetes",
                                                   "Hypoglycaemia progressed to diabetes", "nephrocalcinosis & diabetes")), "Remove", "Keep"))
  
  
  if (diagnosis == TRUE) {
    print("### Exclude specific status ###")
    print(dataset_formatted %>% filter(Status_interim == "Remove") %>% select(Status) %>% unique() %>% unlist() %>% as.vector())
    print("###############################")
    print(table(dataset_formatted$Status, dataset_formatted$Status_interim, useNA = "ifany"))
    print("###############################")
    print(table(dataset_formatted$Status_interim))
  }
  
  # Remove the rows not needed
  dataset_formatted <- dataset_formatted %>%
    filter(Status_interim == "Keep") %>%
    select(-Status_interim)
  
  #:----- Split between Type 1 or Type 2 people
  
  #####
  # Just a check for whether there is a new type of reported initial treatments that we haven't seen yet
  # This is being added because we will be rerunning this on newer versions of the dataset which may have new initial treatments
 initial_treatment_test <- dataset_formatted$`Initial Trtmnt` %in% c("?", "Diazoxide", "diet", "Diet", "DIET", "DIET & INS", "DIET & OHA", "Gliclazide", "glucose", 
                                                                     "Glucose", "Humulin", "ins during preg", "Ins during preg", "Ins in preg", "insulin", "Insulin", 
                                                                     "INSULIN", "insulin during preg", "Insulin during pregnancy", "insulin pump", "Keto diet", "MDI", 
                                                                     "metformin", "Metformin", "Metformin 500mg + diet", "Nifedipine", "non specified", "NONE", 
                                                                     "not known", "not Known", "Not known", "Not Known", "not kown", "not provided", "Not stated",
                                                                     "Not Stated", "Novonorm", "oha", "OHA", "OHA & INS", "unknown", "Unknown", "Unsure", NA,
                                                                     "Mixtard BD", "MDF", "SCII")
  
  # If there are other initial treatments, stop the function so that it can be fixed
  if (sum(initial_treatment_test) != nrow(dataset_formatted)) {
    print("There is new initial treatments which were not considered:")
    print(unique(dataset_formatted$`Initial Trtmnt`[!initial_treatment_test]))
    stop()
  }
  ####
  
  
  ## Split patients whether we need the Type 1 dataset or the Type 2 dataset
  dataset_formatted <- dataset_formatted %>%
    mutate(progressed_to_insulin = ifelse(
      # rule for Type 2: initial treatment missing + not type 1
      is.na(`Initial Trtmnt`) | `Initial Trtmnt` == "insulin during preg" | `Initial Trtmnt` == "Insulin during pregnancy",
      "Type 2",
      ifelse(
        # rules for Type 1: insulin treated within 6 months TRUE, OR, insulin treated within 6 months FALSE but initial treatment is insulin
        InsulinTreatedWithin6Months == "TRUE" | (InsulinTreatedWithin6Months == "FALSE" & (`Initial Trtmnt` == "Insulin" | `Initial Trtmnt` == "MDI" | `Initial Trtmnt` == "Humulin" | `Initial Trtmnt` == "insulin" | `Initial Trtmnt` == "INSULIN" | `Initial Trtmnt` == "insulin pump" | `Initial Trtmnt` == "OHA & INS")),
        "Type 1",
        "Type 2"
      )
    ))
  
  if (diagnosis == TRUE) {
    print("###### Type 1 vs Type 2 #######")
    print(table(dataset_formatted$`Initial Trtmnt`, dataset_formatted$progressed_to_insulin, useNA = "ifany"))
  }
  
  ## If 'investigate' is FALSE, then select one of the types
  if (investigate == "FALSE") {
    ## Select the specific Type and discard the variable created to define them
    if (type == "Type 1") {
      dataset_formatted <- dataset_formatted %>%
        filter(progressed_to_insulin == "Type 1") %>%
        select(-progressed_to_insulin)
    } else if( type == "Type 2") {
      dataset_formatted <- dataset_formatted %>%
        filter(progressed_to_insulin == "Type 2") %>%
        select(-progressed_to_insulin)
    }
  }
  
  
  
  #:----- Ethnicity specification
  
  ## Split patients depending on the reported ethnicity
  
  #####
  # Just a check for whether there is a new type of reported ethnicity that we haven't seen yet
  # This is being added because we will be rerunning this on newer versions of the dataset which may have new ethnicities
  ethnicity_test <- dataset_formatted$EthnicOrigin %in% c("Aboriginal", "Afghanistan", "African", "African Arabic", "African Carribean", "Afro-caribbean", "Afro caribbean", "Afro carribean",
                                                          "Afrocaribbean", "Afrocarribean", "Albanian", "Algerian", "Any other group", "Arab", "Arabic", "Arabic (Palestinian)", "Arabic (sudanese)", 
                                                          "Arabic: Egyptian", "Argentinian", "Aryan", "Ashkenazi", "Ashkenazi Jew", "Ashkenazi Jewish", "Asian", "Asian (Bangladesh)", 
                                                          "Asian (Bangladeshi)", "Asian (British Pakistani)", "Asian (Fllipino)", "Asian (Indian)", "Asian (Iranian)", "Asian (Iraqi)", 
                                                          "Asian (Kurdistan)", "Asian (Nepalese)", "Asian (Pakistani)", "Asian (Palestinian)", "Asian (South Asian)","Asian (South East)", 
                                                          "Asian (Sri Lankan)", "Asian (Thai)", "Asian (Turkish)", "Asian (Vietnamese)", "Asian British", "Asian Indian", "Asian Sindhi",
                                                          "Asian Sri Lankan", "Asian/Indian", "Australian", "Baloch", "Bangladeshi", "Bengali", "Black", "Black (African)","Black (Afro-Carribbean)", 
                                                          "Black (British)", "Black (Caribbean)", "Black (Carribbean)", "Black (Sudanese)", "Black African", "Black British", "Black Caribbean",
                                                          "Black Carribean", "Brazil", "Brazilian", "British", "British Bangladeshi", "British Indian", "British Pakistani", "Bulgarian", "Canadian first nation", 
                                                          "Caribbean", "Caribean", "carribean", "Carribean", "Caucasian", "Central American", "Chinese", "Chinese (Hong Kong)", "Cook Island Māori", 
                                                          "Cuban/black", "Czech", "Duch/Native American", "Dutch", "Eastern European", "Ecuador", "Egyptian", "English", "Eritrea", "Eritrean",
                                                          "Ethiopian", "Etrtia", "European", "Fijian", "Fijian Indian", "Filipino", "Finnish", "French Canadian", "German", "Greek", "Gwamaa", "Hispanic",
                                                          "Hungarian", "Icelandic", "indian", "Indian", "INDIAN", "Indian/White mixed", "Indonesian", "Intalian/Maltese/British", "Iran", "Iranian", 
                                                          "Iraqi", "Irish", "Irish White", "Italian", "Japanese", "Jewish", "Jordan", "Kinh", "Kinh Ethnic", "Korean", "Kurdish", "Kurdistan", 
                                                          "Latin American", "Lebanese", "Libya", "Malayan", "Maltese", "Maori", "Māori", "Mauritian", "Maurtian", "Mexican", "Middle Eastern", "Mixed", 
                                                          "Mixed - White & Israeli", "Mixed (Asian-Indian/British Indian)", "Mixed (Eastern Euro and Iraqi)", "Mixed (English-Indonesian)", 
                                                          "Mixed (European, Chinese)", "Mixed (Fijian-Indian)", "Mixed (Filippino/Irish)", "Mixed (Half Jamacain)", "Mixed (Mauritian/Punjabi Indian)", 
                                                          "Mixed (White & Black)", "Mixed (White & Hispanic)", "Mixed (White African)", "Mixed (white/Indigenous Canadian)", "Mixed Ashkenazi - Argentina", 
                                                          "Mixed black & white", "Mixed english and pakistan", "Mixed white & black caribbean", "Mixed white asian", "Mixed: White-Asian", "Moldovan", 
                                                          "N Irish", "N/A", "Nepalese", "New Zealand", "New Zealander", "Nigerian", "Niuean", "Non-white", "North African", "North American", 
                                                          "Northern European", "Norwegian/French/Ukrainian", "Not known", "not sated", "Not sated", "not stated", "Not stated", "Not Stated",
                                                          "Not Stated (African)", "Not Stated (British Bangladeshi)", "Not Stated (British Indian)", "Not Stated (British)", "Not Stated (Greek)", 
                                                          "Not Stated (Iranian)", "Not stated (Maori)", "Not Stated (Mauritian)", "Not Stated (Moroccan)", "Not Stated (Polish)", "Not Stated (Somalian)", 
                                                          "Not Stated (Spanish)", "Not Stated (Swedish)", "nto stated", "NZ European", "Other Asian", "Other Mixed", "Other White", "Other White Background", 
                                                          "Pacifika", "Pakistan", "Pakistani", "Palestinian", "Pashto", "Persian", "Philipinnes", "Phillipines", "Phillipino", "Polish", 
                                                          "Polynesian", "Polynesian (Maori)", "Polynesian (Tongan)", "Portuguese", "Romani Gypsy", "Romanian", "Samoan", "Saraiki", "SE Asian", 
                                                          "Sikh", "Singapore", "Somali", "Somalia", "Somalian", "South African", "South American", "South American/Portuguese", "South Asian", 
                                                          "South East Asian", "Southern European", "Spanish", "Sri Lankan", "Sri Lankan Asian", "Sri Lanken", "Srilankan", "Sudanese", 
                                                          "Sudanese/British", "Swedish", "Syrian", "Thai", "Tongan", "Turkish", "Turkish Cypriot", "Turkish/Greek", "Turksih", "UK", 
                                                          "Ukrainian", "Ukranian", "Unknown", "white", "White", "White & Asian", "White & black", "White & Black African", "White & Black Carribean", 
                                                          "White (albanaian)", "White (Albanian)", "White (Ashkenazi Jewish)", "White (Australian-Polish European)", "White (Australian European)", 
                                                          "White (Australian)", "White (british)", "White (British)", "White (Eastern European)", "White (Egyptian)", "White (english)", 
                                                          "White (greek)", "White (Hungarian)", "White (Irish)", "White (New Zealand European)", "White (Nothern Irish)", "White (NZ European)", 
                                                          "White (Scottish)", "White (south african)", "White (Venezualan)", "White American", "White british", "White British", "White Caucasian", 
                                                          "White Ireland", "White Irish", "White Italian", "White Middle Eastern", "whtie", "Whtie", NA)
  
  # If there are other ethnicities, stop the function so that it can be fixed
  if (sum(ethnicity_test) != nrow(dataset_formatted)) {
    print("There is new ethnicities which were not considered:")
    print(unique(dataset_formatted$EthnicOrigin[!ethnicity_test]))
    stop()
  }
  ####
  
  dataset_formatted <- dataset_formatted %>%
    mutate(EthnicOrigin = ifelse(is.na(EthnicOrigin), "NA", EthnicOrigin),
           Eth10 = "undefined",
           Eth5 = "undefined")
  
  for (i in 1:nrow(dataset_formatted)) {
    
    # ethnicity entry
    value_eth <- dataset_formatted$EthnicOrigin[i]
    
    # check what eth10 it is
    dataset_formatted$Eth10[i] <- as.character(ethnicity_labels[which(ethnicity_labels[,1] == as.numeric(ethnicity_groups[which(ethnicity_groups[,1] == value_eth), 2])),2])
    
    # check what eth5 it is
    dataset_formatted$Eth5[i] <- as.character(ethnicity_labels[which(ethnicity_labels[,1] == as.numeric(ethnicity_groups[which(ethnicity_groups[,1] == value_eth), 2])),4])
    
  }
  
  
  if (diagnosis == TRUE) {
    print("###### White or Non-White #######")
    print(table(dataset_formatted$Eth10 ,dataset_formatted$Eth5, useNA = "ifany"))
  }
  
  
  ## Select the specific Ethnicity and discard the variable created to define them
  if (investigate == FALSE) {
    if (ethnicity == "White") {
      dataset_formatted <- dataset_formatted %>%
        filter(Eth5 == "White")
    } else if(ethnicity == "Non-White") {
      dataset_formatted <- dataset_formatted %>%
        filter(Eth5 != "White")
    }
  }
  
  
  
  
  #:----- Gene specification
  
  #####
  # Just a check for whether there is a new type of reported gene that we haven't seen yet
  # This is being added because we will be rerunning this on newer versions of the dataset which may have new gene
  gene_test <- dataset_formatted$Gene %in% c("?UQCRH", "3243", "6q24", "9pdel", "ABCC8", "ABCC8 & HNF4A", "AIRE", "CEL", "CISD2", 
                                             "CTLA4", "DNAJC3", "EIF2AK3", "FOXP3", "GATA4", "GATA6", "GCK", "HNF1a", "HNF1A", 
                                             "HNF1A & GCK", "HNF1A & HNF4A", "HNF1b", "HNF1B", "HNF4a", "HNF4A", "HNF4A & INSR", 
                                             "IL2RA", "INS", "INSR", "KCNJ11", "LMNA", "MAFA", "MANF", "NeuroD1", "NEUROD1", 
                                             "NEUROG3", "NGN3", "PAX6", "PDX1", "PIK3R1", "PPARG", "PTF1A", "RFX6", "SLC19A2", 
                                             "SLC29A3", "STAT1", "TRMT10A", "WFS1", "ZBTB20", "ZFP57", "ZNF808", NA,
                                             "STAT3", "LRBA", "BWS", "ADA")
  
  # If there are other gene, stop the function so that it can be fixed
  if (sum(gene_test) != nrow(dataset_formatted)) {
    print("There is new gene which were not considered:")
    print(unique(dataset_formatted$Gene[!gene_test]))
    stop()
  }
  ####
  
  # Set 'Keep' or 'Remove' to those with the gene we want/don't, respectively
  dataset_formatted <- dataset_formatted %>%
    mutate(gene_type = ifelse(Gene %in% c("GCK", "HNF1a", "HNF1A", "HNF4a", "HNF4A", "HNF1A & GCK", "HNF1A & HNF4A"), "Primary", ifelse(is.na(Gene), NA, "Secondary")))
  
  
  if (diagnosis == TRUE) {
    print("### Only include specific genes ###")
    print(c("", "HNF1a", "HNF1A", "HNF4a", "HNF4A", "GCK"))
    print("###############################")
    print(table(dataset_formatted$Gene, dataset_formatted$gene_type, useNA = "ifany"))
    print("###############################")
    print(table(dataset_formatted$gene_type, useNA = "ifany"))
  }
  
  
  if (investigate == FALSE){
    if (gene_type == "Primary") {
      dataset_formatted <- dataset_formatted %>%
        filter(gene_type %in% c("Primary", NA)) %>%
        select(-gene_type)
    } else {
      dataset_formatted <- dataset_formatted %>%
        filter(gene_type == "Secondary") %>%
        select(-gene_type)
    }
  }
  

  #:----- Proband
  
  ### Needs to be done, but require the specific variable
  dataset_formatted <- dataset_formatted %>%
    mutate(proband = ifelse(endsWith(MODYNo, "01"), "Proband", "Not Proband"))
  
  
  if (diagnosis == TRUE) {
    print("### Probands end with 01 ###")
    print("###############################")
    print(table(dataset_formatted$proband, useNA = "ifany"))
  }
  
  ## Select the specific Proband choice made
  if (investigate == FALSE) {
    if (proband == "Proband") {
      dataset_formatted <- dataset_formatted %>%
        filter(proband == "Proband")
    }
  }
  
  
  
  ###
  # Maybe add relationship to include non-blood relation
  ###
  
  
  
  #:------------------------------------------------------------------------
  ####
  ####    Set up variables
  ####
  
  
  #:----- Ethnicity
  dataset_formatted <- dataset_formatted %>%
    mutate(Eth10 = factor(Eth10),
           Eth5 = factor(Eth5))
  
  #:----- Parent history
  dataset_formatted <- dataset_formatted %>%
    mutate(pardm = ifelse(
      # If parents have history
      MotherDM == "Yes" | FatherDM == "Yes",
      1,
      ifelse(
        # If parents do not have history: combinations: N/N
        (MotherDM == "No" & FatherDM == "No"),
        0,
        NA
        )
      ),
      # this variable is checking whether the parent history is both parents or not 
      #  (the mody calculator was made for one of the parents having diabetes, not both)
      pardm_breakdown = ifelse(
        MotherDM == "Yes" & FatherDM == "Yes",
        2,
        ifelse(
          (MotherDM == "Yes" & FatherDM == "No") | (MotherDM == "No" & FatherDM == "Yes"),
          1,
          ifelse(
            # If parents do not have history: combinations: N/N
            (MotherDM == "No" & FatherDM == "No"),
            0,
            NA
          )
        )
      ))
  
  #:----- Current age
  dataset_formatted <- dataset_formatted %>%
    mutate(agerec = as.numeric(`Age Collected`))
  
  #:----- Age at diagnosis
  dataset_formatted <- dataset_formatted %>%
    mutate(agedx = as.numeric(`Age Diag`))
  
  #:----- BMI
  
  ### Need to make sure BMI values are within sensible ranges
  if (investigate == TRUE) {
    ## low values
    dataset_investigate <- dataset_investigate %>%
      left_join(
        dataset_formatted %>%
          select(MODYNo, BMI, agerec) %>%
          mutate(
            BMI_check = ifelse(
              (agerec > 17 & BMI < 15) | BMI > 50,
              TRUE,
              NA
            )
          ) %>%
          select(-BMI, -agerec),
        by = c("MODYNo")
      )
  }
  
  dataset_formatted <- dataset_formatted %>%
    mutate(bmi = as.numeric(BMI),
           bmi = ifelse(
             bmi < 2 | bmi > 100,
             NA,
             bmi
           )) %>%
    select(-BMI)
  
  
  
  
  #:----- HbA1c (%)
  
  
  dataset_formatted <- dataset_formatted %>%
    # numeric everything
    mutate(HbA1c = as.numeric(HbA1c),
           Hba1c2 = as.numeric(Hba1c2),
           IFCCHBA1C = as.numeric(IFCCHBA1C)) %>%
    # erroneous values 
    mutate(Hba1c2 = ifelse(Hba1c2 == 0, NA, Hba1c2)) %>%
    # turn everything to %
    mutate(
      HbA1c = ifelse(
        HbA1c > 20,
        (HbA1c/10.929) + 2.15,
        HbA1c
        ),
      Hba1c2 = ifelse(
        Hba1c2 > 20,
        (Hba1c2/10.929) + 2.15,
        Hba1c2
        ),
      IFCCHBA1C = ifelse(
        IFCCHBA1C > 20,
        (IFCCHBA1C/10.929) + 2.15,
        IFCCHBA1C
        )
      )
  
  print("This warning is okay.")
  

  
  
  ### Need to choosen between HbA1c, Hba1c2, IFCCHBA1C (make sure they are all %)
  if (investigate == TRUE) {
    ## low values
    dataset_investigate <- dataset_investigate %>%
      left_join(
        dataset_formatted %>%
          select(MODYNo, HbA1c, Hba1c2, IFCCHBA1C) %>%
          mutate(
            HbA1c_check = ifelse(
              HbA1c < 4.5 | HbA1c > 20,
              TRUE,
              NA
            ),
            Hba1c2_check = ifelse(
              Hba1c2 < 4.5 | Hba1c2 > 20,
              TRUE,
              NA
            ),
            IFCCHBA1C_check = ifelse(
              IFCCHBA1C < 4.5 | IFCCHBA1C > 20,
              TRUE,
              NA
            )
          ) %>%
          select(-HbA1c, -Hba1c2, -IFCCHBA1C),
        by = c("MODYNo")
      )
  }
  
  dataset_formatted <- dataset_formatted %>%
    ## apply our criteria for plausible values first
    mutate(
      HbA1c_plausible = ifelse(
        HbA1c < 3 | HbA1c > 20,
        NA,
        HbA1c
      ),
      Hba1c2_plausible = ifelse(
        Hba1c2 < 3 | Hba1c2 > 20,
        NA,
        Hba1c2
      ),
      IFCCHBA1C_plausible = ifelse(
        IFCCHBA1C < 3 | IFCCHBA1C > 20,
        NA,
        IFCCHBA1C
      )
    ) %>%
    ## decide on which value we are going to use for the final variable
    mutate(
      hba1c = ifelse(
        # if the variable IFCCHBA1C isn't missing, take this value
        !is.na(IFCCHBA1C_plausible),
        IFCCHBA1C_plausible,
        # if the variable HbA1c isn't missing, take this value
        ifelse(
          !is.na(HbA1c_plausible),
          HbA1c_plausible,
          # if the variable Hba1c2 isn't missing, take this value
          ifelse(
            !is.na(Hba1c2_plausible),
            Hba1c2_plausible,
            # if all variable are missing
            NA
          )
        )
      )
    )
    
  #:----- Sex (male 1, female 2)
  dataset_formatted <- dataset_formatted %>%
    mutate(sex = ifelse(
      # If male
      Sex == "Male",
      1,
      # If female
      ifelse(
        Sex == "Female",
        2,
        # If unknown
        NA
      )
    ))
  
  #:----- Treatment (insulin or tablets vs no insulin or tablets)
  
  #####
  # Just a check for whether there is a new type of reported gene that we haven't seen yet
  # This is being added because we will be rerunning this on newer versions of the dataset which may have new gene
  insoroha_test <- dataset_formatted$`Current Trtmnt` %in% c("br", "CSII", "Diet", "DIET", "Eithroxin", "Hexathyroxinelevothyroxine", "INS", 
                                                             "insulin", "Insulin", "INSULIN", "Insulin during pregnancy", 
                                                             "INSULIN during pregnancy", "insulin pump", "Insulin pump", "Insulin Pump", 
                                                             "Lantus", "Lantus & Humalog", "Levemir", "Loop, DIET", "metformin", "Metformin", 
                                                             "METFORMIN", "Nifedipine + Sulphonylurea", "non insulin treated", "non specified",
                                                             "NONE", "not insulin treated", "not known", "not Known", "Not Known", 
                                                             "not provided", "not stated", "Not stated", "Not Stated", "Novonorm", 
                                                             "Novorapid", "Octreotide", "oha", "OHA", "OHA & injectable GLP1", 
                                                             "OHA & INS", "OHA and diet", "Oral", "SCII, DIET", "Semaglutide", "unknown", 
                                                             "Unknown", "Unsure of text on form", NA)
  
  # If there are other gene, stop the function so that it can be fixed
  if (sum(insoroha_test) != nrow(dataset_formatted)) {
    print("There is new gene which were not considered:")
    print(unique(dataset_formatted$`Current Trtmnt`[!insoroha_test]))
    stop()
  }
  ####
  
  if (investigate == TRUE) {
    dataset_investigate <- dataset_investigate %>%
      left_join(
        dataset_formatted %>%
          select(MODYNo, `Current Trtmnt`) %>%
          mutate(
            `Current Trtmnt_check` = ifelse(
              `Current Trtmnt` %in% c("Unsure of text on form", "br"),
              TRUE,
              NA
            )
          ) %>%
          select(-`Current Trtmnt`),
        by = c("MODYNo")
      )
  }
  
  #:-------------
  ### Still need to figure out what to do with Insulin during pregnancy
  #:-------------
  
  dataset_formatted <- dataset_formatted %>%
    mutate(insoroha = ifelse(
      # 1 (on insulin or oha)
      `Current Trtmnt` %in% c("CSII", "INS", "insulin", "Insulin", "INSULIN", "insulin pump", "Insulin pump", 
                              "Insulin Pump", "Lantus", "Lantus & Humalog", "Levemir", "Loop, DIET", 
                              "metformin", "Metformin", "METFORMIN", "Nifedipine + Sulphonylurea",
                              "Novonorm", "Novorapid", "Octreotide", "oha", "OHA", "OHA & injectable GLP1",
                              "OHA & INS", "OHA and diet", "Oral", "SCII, DIET", "Semaglutide"),
      1,
      # 0 (not on insulin or oha)
      ifelse(
        `Current Trtmnt` %in% c("Diet", "DIET", "Eithroxin", "Hexathyroxinelevothyroxine", 
                                "non insulin treated", "NONE", "not insulin treated"),
        0,
        NA
      )
    ))
  
  

  #:----- C-peptide
  
  ## Only do this for Type 1
  if (type == "Type 1") {
    
    # cpeptide > 200 or UCPCR > 0.2
    
    dataset_formatted <- dataset_formatted %>%
      # convert units (from ng/ml to pmol/L)
      mutate(`C-Peptide` = ifelse(
        `C-peptide units` %in% c("ng/ml", "ug/ul"),
        `C-Peptide`/0.003,
        `C-Peptide`
      )) 
    
    if (investigate == TRUE) {
      dataset_investigate <- dataset_investigate %>%
        left_join(
          dataset_formatted %>%
            select(MODYNo, `UCPCR Value`, `C-peptide units`, `C-Peptide`) %>%
            mutate(
              `UCPCR Value_check` = ifelse(
                `UCPCR Value` < 0.01 | `UCPCR Value` > 12,
                TRUE,
                NA
                ),
              `C-peptide units_check` = ifelse(
                `C-peptide units` %in% c("nmol/L", "pml", "nmol/L", "ug/ul"),
                TRUE,
                NA
              ),
              `C-Peptide_check` = ifelse(
                `C-Peptide` < 20 | `C-Peptide` > 5000,
                TRUE,
                NA
              )
              ) %>%
            select(-`UCPCR Value`, -`C-peptide units`, -`C-Peptide`),
          by = c("MODYNo")
        )
    }
    
    
    dataset_formatted <- dataset_formatted %>%
      mutate(
        C = ifelse(
          ## if UCPCR isn't missing
          !is.na(`UCPCR Value`),
          ifelse(
            `UCPCR Value` > 0.2,
            1,
            0
            ),
          ## if C-peptide isn't missing
          ifelse(
            !is.na(`C-Peptide`),
            ifelse(
              `C-Peptide` > 200,
              1,
              0
              ),
            NA
            )
          )
        )
    
    
  }
  
  #:----- Antibody
  
  ## Only do this for Type 1
  if (type == "Type 1") {
    
    ## three variables, one for each each ab, 0 or 1 or NA
    
    if (investigate == TRUE) {
      dataset_investigate <- dataset_investigate %>%
        left_join(
          dataset_formatted %>%
            select(MODYNo, `GAD -ve`, `GAD +ve`, `GAD -ve Exeter`, `GAD +ve Exeter`, `ZnT8 -ve`, `ZnT8 +ve`, `ZnT8 -ve Exeter`, `ZnT8 +ve Exeter`, `IA2 -ve`, `IA2 +ve`) %>%
            mutate(
              GAD_non_Exeter = ifelse(
                `GAD -ve` == TRUE & `GAD +ve` == TRUE,
                TRUE,
                NA
              ),
              GAD_Exeter = ifelse(
                `GAD -ve Exeter` == TRUE & `GAD +ve Exeter` == TRUE,
                TRUE,
                NA
              ),
              ZnT8_non_Exeter = ifelse(
                `ZnT8 -ve` == TRUE & `ZnT8 +ve` == TRUE,
                TRUE,
                NA
              ),
              ZnT8_Exeter = ifelse(
                `ZnT8 -ve Exeter` == TRUE & `ZnT8 +ve Exeter` == TRUE,
                TRUE,
                NA
              ),
              IA2_non_Exeter = ifelse(
                `IA2 -ve` == TRUE & `IA2 +ve` == TRUE,
                TRUE,
                NA
              )
            ) %>%
            select(MODYNo, GAD_non_Exeter, GAD_Exeter, ZnT8_non_Exeter, ZnT8_Exeter, IA2_non_Exeter),
          by = c("MODYNo")
        )
    }
    
    dataset_formatted <- dataset_formatted %>%
      ## Create variables on each antibody and location
      mutate(
        GAD_non_Exeter_test_result = ifelse(
          `GAD -ve` == TRUE & `GAD +ve` == FALSE,
          "Negative",
          ifelse(
            `GAD -ve` == FALSE & `GAD +ve` == TRUE,
            "Positive",
            NA
          )
        ),
        GAD_Exeter_test_result = ifelse(
          `GAD -ve Exeter` == TRUE & `GAD +ve Exeter` == FALSE,
          "Negative",
          ifelse(
            `GAD -ve Exeter` == FALSE & `GAD +ve Exeter` == TRUE,
            "Positive",
            NA
          )
        ),
        ZnT8_non_Exeter_test_result = ifelse(
          `ZnT8 -ve` == TRUE & `ZnT8 +ve` == FALSE,
          "Negative",
          ifelse(
            `ZnT8 -ve` == FALSE & `ZnT8 +ve` == TRUE,
            "Positive",
            NA
          )
        ),
        ZnT8_Exeter_test_result = ifelse(
          `ZnT8 -ve Exeter` == TRUE & `ZnT8 +ve Exeter` == FALSE,
          "Negative",
          ifelse(
            `ZnT8 -ve Exeter` == FALSE & `ZnT8 +ve Exeter` == TRUE,
            "Positive",
            NA
          )
        ),
        IA2_non_Exeter_test_result = ifelse(
          `IA2 -ve` == TRUE & `IA2 +ve` == FALSE,
          "Negative",
          ifelse(
            `IA2 -ve` == FALSE & `IA2 +ve` == TRUE,
            "Positive",
            NA
          )
        )
      ) %>%
      ## Create general variable result
      mutate(
        ### GAD
        GAD_final = ifelse(
          #### If Exeter isn't missing, check result in Exeter, otherwise check result in non-Exeter
          !is.na(GAD_Exeter_test_result),
          ifelse(
            GAD_Exeter_test_result == "Positive",
            1,
            0
          ),
          #### If non-Exeter isn't missing, check result in non-Exeter, other NA
          ifelse(
            !is.na(GAD_non_Exeter_test_result),
            ifelse(
              GAD_non_Exeter_test_result == "Positive",
              1,
              0
            ),
            NA
          )
        ),
        ZnT8_final = ifelse(
          #### If Exeter isn't missing, check result in Exeter, otherwise check result in non-Exeter
          !is.na(ZnT8_Exeter_test_result),
          ifelse(
            ZnT8_Exeter_test_result == "Positive",
            1,
            0
          ),
          #### If non-Exeter isn't missing, check result in non-Exeter, other NA
          ifelse(
            !is.na(ZnT8_non_Exeter_test_result),
            ifelse(
              ZnT8_non_Exeter_test_result == "Positive",
              1,
              0
            ),
            NA
          )
        ),
        IA2_final = ifelse(
          #### If non-Exeter isn't missing, check result in non-Exeter, other NA
          !is.na(IA2_non_Exeter_test_result),
          ifelse(
            IA2_non_Exeter_test_result == "Positive",
            1,
            0
          ),
          NA
        )
      ) %>%
      # add variable with how many tests were done
      mutate(
        ab_tested = ifelse(
          !is.na(GAD_final) & !is.na(ZnT8_final) & !is.na(IA2_final),
          3,
          ifelse(
            (!is.na(GAD_final) & !is.na(ZnT8_final) & is.na(IA2_final)) | (!is.na(GAD_final) & is.na(ZnT8_final) & !is.na(IA2_final)) | (is.na(GAD_final) & !is.na(ZnT8_final) & !is.na(IA2_final)),
            2,
            ifelse(
              (!is.na(GAD_final) & is.na(ZnT8_final) & is.na(IA2_final)) | (is.na(GAD_final) & !is.na(ZnT8_final) & is.na(IA2_final)) | (is.na(GAD_final) & is.na(ZnT8_final) & !is.na(IA2_final)),
              1,
              ifelse(
                is.na(GAD_final) & is.na(ZnT8_final) & is.na(IA2_final),
                0,
                NA
                )
              )
            )
          )
      )
    # add variable with how many positives you had
    dataset_formatted$abcat = rowSums(dataset_formatted %>% select(GAD_final, ZnT8_final, IA2_final), na.rm = TRUE)
    dataset_formatted <- dataset_formatted %>%
      mutate(abcat = ifelse(
        ab_tested == 0,
        NA,
        abcat
      ))
    
    
    ## create main antibody variable
    dataset_formatted <- dataset_formatted %>%
      mutate(
        A = ifelse(
          ab_tested == 0,
          NA,
          ifelse(
            abcat > 0,
            1,
            0
          )
        )
      )
    
  }
  
  #:----- MODY outcome
  
  dataset_formatted <- dataset_formatted %>%
    mutate(
      tested_1 = ifelse(
        P291fsinsCResult %in% c(NA),
        0,
        1
      ),
      tested_2 = ifelse(
        HNF1aTestResult %in% c(NA, "No Test"),
        0,
        1
      ),
      tested_3 = ifelse(
        HNF4aTestResult %in% c(NA),
        0,
        1
      ),
      tested_4 = ifelse(
        GlucokinaseResult %in% c(NA),
        0,
        1
      )
    ) %>%
    mutate(
      MODY_tests = tested_1 + tested_2 + tested_3 + tested_4,
      MODY_tested = ifelse(
        MODY_tests > 0,
        TRUE,
        FALSE
      )
    )
  
  
  
  #:------------------------------------------------------------------------
  ####
  ####    Final formatting and output dataset
  ####
  
  

    
  
  
  
  
  
  
}

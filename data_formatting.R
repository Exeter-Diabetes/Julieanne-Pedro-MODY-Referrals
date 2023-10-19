#' Formatting Referral data
#'
#' This file contains the function needed for formatting the referral data.
#'
#' @param dataset original unformatted referral dataset
#' @param diagnosis TRUE or FALSE flag on whether excluded patient numbers should be printed
#' @param type 'Type 1' or 'Type 2' variable to help define the dataset
#' @param ethnicity 'White' or 'Non-White' variable to help define the ethnicity
#'
#' @return dataset ready for analysis
#'
#' @import coda
#'
#'
#' @export
formatting <- function(dataset, diagnosis = FALSE, type = NULL, ethnicity = NULL) {
  
  ### Function checks
  ## Ensure type of diabetes is chosen
  if (is.null(type)) {stop("'type' of diabetes must be defined; 'Type 1' or 'Type 2'")}
  if (!(type %in% c("Type 1", "Type 2"))) {("'type' of diabetes must be defined; 'Type 1' or 'Type 2'.")}
  ## Ensure ethnicity is chosen 
  if (is.null(ethnicity)) {stop("'ethnicity' must be defined; 'White' or 'Non-White'")}
  if (!(ethnicity %in% c("White", "Non-White"))) {("'ethnicity' must be defined; 'White' or 'Non-White'.")}
  
  ### Libraries needed
  require(tidyverse)
  
  
  #:------------------------------------------------------------------------
  ####
  ####    Exclusion procedure
  ####
  
  ### set up dataset for change
  dataset_formatted <- dataset
  
  ###
  # Make sure there are no repeated rows
  if (length(unique(dataset_formatted$MODYNo)) != nrow(dataset_formatted)) {
    stop("There are repeated individuals based on the MODY number")
  }
  
  
  ###
  
  #:----- Age range
  
  ## Nothing needs to be done here as the dataset is already in the range needed
  
  if (diagnosis == TRUE) {
    print("###### Age of diagnosis #######")
    print("######## Range 1 - 35 #########")
    print(summary(dataset_formatted$`Age Diag`))
  }
  
  
  #:----- Split between Type 1 or Type 2 people
  
  #####
  # Just a check for whether there is a new type of reported initial treatments that we haven't seen yet
  # This is being added because we will be rerunning this on newer versions of the dataset which may have new initial treatments
 initial_treatment_test <- dataset_formatted$`Initial Trtmnt` %in% c("?", "Diazoxide", "diazoxide + nifedipine", "diet", "Diet", "DIET", "DIET & INS", "DIET & OHA", 
                                                                     "Gliclazide", "Glucosae", "glucose", "Glucose", "Humulin", "Ins dur preg", "ins during preg", 
                                                                     "Ins during preg", "Ins in preg", "insulin", "Insulin", "INSULIN", "insulin during preg", 
                                                                     "Insulin during pregnancy", "insulin pump", "Keto diet", "MDF", "MDI", "metformin", "Metformin", 
                                                                     "Metformin 500mg + diet", "Mixtard BD", "Nifedipine", "non specified", "NONE", "not known", 
                                                                     "not Known", "Not known", "Not Known", "not kown", "not provided", "Not stated", "Not Stated", 
                                                                     "Novonorm", "Ocreotide", "Octreotide", "oha", "OHA", "OHA & INS", "SCII", "SU sent", "unknown", 
                                                                     "Unknown", "Unsure", NA)
  
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
      # rules for Type 1: insulin treated within 6 months TRUE, OR, insulin treated within 6 months FALSE but initial treatment is insulin
      InsulinTreatedWithin6Months == "TRUE" | (InsulinTreatedWithin6Months == "FALSE" & (`Initial Trtmnt` == "Insulin" | `Initial Trtmnt` == "Humulin" | `Initial Trtmnt` == "INSULIN" | `Initial Trtmnt` == "insulin pump" | `Initial Trtmnt` == "OHA & INS")),
      "Type 1",
      # rules for Type 2: not specified
      stop("Rules for defining 'Type 2' not defined yet.")
    ))
  
  if (diagnosis == TRUE) {
    print("###### Type 1 vs Type 2 #######")
    print(table(dataset_formatted$`Initial Trtmnt`, dataset_formatted$progressed_to_insulin, useNA = "ifany"))
  }
  
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
  
  ## Define ethnicity types from subtypes
  # MODYno for White ethnicity
  modyno_white <- dataset_formatted %>%
    filter(str_detect(EthnicOrigin, "White") | str_detect(EthnicOrigin, "European") | EthnicOrigin == "Caucasian" | EthnicOrigin == "British" | EthnicOrigin == "Czech" | EthnicOrigin == "Dutch" | EthnicOrigin == "English" | EthnicOrigin == "Finnish" | EthnicOrigin == "French Canadian" | EthnicOrigin == "German" | EthnicOrigin == "Greek" | EthnicOrigin == "Hungarian" | EthnicOrigin == "Icelandic" | EthnicOrigin == "Irish" | EthnicOrigin == "Italian" | EthnicOrigin == "Maltese" | EthnicOrigin == "N Irish" | EthnicOrigin == "Polish" | EthnicOrigin == "Portuguese" | EthnicOrigin == "Romanian" | EthnicOrigin == "Spanish" | EthnicOrigin == "Swedish" | EthnicOrigin == "UK" | EthnicOrigin == "Ukranian" | EthnicOrigin == "Ukrainian" | EthnicOrigin == "whtie" | EthnicOrigin == "Whtie" | EthnicOrigin == "white") %>%
    filter(str_detect(EthnicOrigin, "Black", negate = TRUE), str_detect(EthnicOrigin, "black", negate = TRUE), str_detect(EthnicOrigin, "Asian", negate = TRUE), str_detect(EthnicOrigin, "Mixed", negate = TRUE), str_detect(EthnicOrigin, "mixed", negate = TRUE)) %>%
    select(MODYNo) %>%
    unlist()
  
  # MODYno for Black ethnicity
  modyno_black <- dataset_formatted %>%
    filter(str_detect(EthnicOrigin, "Black") | str_detect(EthnicOrigin, "black")) %>%
    filter(str_detect(EthnicOrigin, "White", negate = TRUE), str_detect(EthnicOrigin, "European", negate = TRUE), str_detect(EthnicOrigin, "Asian", negate = TRUE), str_detect(EthnicOrigin, "Mixed", negate = TRUE), str_detect(EthnicOrigin, "mixed", negate = TRUE)) %>%
    select(MODYNo) %>%
    unlist()
  
  # MODYno for Asian ethnicity
  modyno_asian <- dataset_formatted %>%
    filter(str_detect(EthnicOrigin, "Asian")) %>%
    filter(str_detect(EthnicOrigin, "White", negate = TRUE), str_detect(EthnicOrigin, "European", negate = TRUE), str_detect(EthnicOrigin, "Black", negate = TRUE), str_detect(EthnicOrigin, "black", negate = TRUE), str_detect(EthnicOrigin, "Mixed", negate = TRUE), str_detect(EthnicOrigin, "mixed", negate = TRUE)) %>%
    select(MODYNo) %>%
    unlist()
  
  # MODYno for Mixed ethnicity
  modyno_mixed <- dataset_formatted %>%
    filter(str_detect(EthnicOrigin, "Mixed") | str_detect(EthnicOrigin, "mixed") | str_detect(EthnicOrigin, "&")) %>%
    select(MODYNo) %>%
    unlist()
  
  ###
  # Just a check to make sure no rows are being considered more than once
  if (sum(duplicated(c(modyno_white, modyno_black, modyno_asian, modyno_mixed))) != 0) {
    stop("One or more rows are being identified as several Ethnicity types.")
  } 
  ###
  
  ## Define ethnicity types from subtypes
  dataset_formatted$ethnicity_clean <- "Not used"
  dataset_formatted$ethnicity_clean[dataset_formatted$MODYNo %in% modyno_white] <- "White"
  dataset_formatted$ethnicity_clean[dataset_formatted$MODYNo %in% modyno_black] <- "Black"
  dataset_formatted$ethnicity_clean[dataset_formatted$MODYNo %in% modyno_asian] <- "Asian"
  dataset_formatted$ethnicity_clean[dataset_formatted$MODYNo %in% modyno_mixed] <- "Mixed"
  
  
  if (diagnosis == TRUE) {
    print("###### White or Non-White #######")
    print(table(dataset_formatted$EthnicOrigin ,dataset_formatted$ethnicity_clean, useNA = "ifany"))
  }
  
  
  ## Select the specific Ethnicity and discard the variable created to define them
  if (ethnicity == "White") {
    dataset_formatted <- dataset_formatted %>%
      filter(ethnicity_clean == "White")
  } else if(ethnicity == "Non-White") {
    dataset_formatted <- dataset_formatted %>%
      filter(ethnicity_clean == "Black" | ethnicity_clean == "Asian" | ethnicity_clean == "Mixed")
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
                                                   "Renal Cysts & Diabetes", "Diabetes & Renal Cysts", "Hyperinsulinism progressed to diabetes", "hyperinsulinism progressed to diabetes")), "Remove", "Keep"))
  
  
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
  
  
  #:----- Gene specification
  
  #####
  # Just a check for whether there is a new type of reported gene that we haven't seen yet
  # This is being added because we will be rerunning this on newer versions of the dataset which may have new gene
  gene_test <- dataset_formatted$Gene %in% c("?UQCRH", "3243", "6q24", "9pdel", "ABCC8", "ABCC8 & HNF4A", "ADA", "AIRE", 
                                             "BWS", "CEL", "CISD2", "CTLA4", "DNAJC3", "DNAJC3/DNAJC3", "EIF2AK3", "FOXP3",
                                             "GATA4", "GATA6", "GCK", "GLUD1", "HK1", "HNF1a", "HNF1A", "HNF1A & GCK", 
                                             "HNF1A & HNF4A", "HNF1b", "HNF1B", "HNF4a", "HNF4A", "HNF4A & INSR", "IL2RA", 
                                             "INS", "INSR", "KCNJ11", "KMT2D", "LMNA", "LRBA", "MAFA", "MANF", "MEN1", 
                                             "Monogenic unlikely", "NeuroD1", "NEUROD1", "NEUROG3", "NGN3", "PAX6", "PDX1", 
                                             "PIK3R1", "PMM2", "POLD1", "PPARG", "PTF1A", "RFX6", "SLC19A2", "SLC29A3", 
                                             "STAT1", "STAT3", "TRMT10A", "Turner's syndrome", "WFS1", "ZBTB20", "ZFP57", 
                                             "ZNF808", NA)
  
  # If there are other gene, stop the function so that it can be fixed
  if (sum(gene_test) != nrow(dataset_formatted)) {
    print("There is new gene which were not considered:")
    print(unique(dataset_formatted$Gene[!gene_test]))
    stop()
  }
  ####
  
  # Set 'Keep' or 'Remove' to those with the gene we want/don't, respectively
  dataset_formatted <- dataset_formatted %>%
    mutate(gene_interim = ifelse(!(Gene %in% c("GCK", "HNF1a", "HNF1A", "HNF4a", "HNF4A", NA)), "Remove", "Keep"))
  
  
  if (diagnosis == TRUE) {
    print("### Only include specific genes ###")
    print(c("", "HNF1a", "HNF1A", "HNF4a", "HNF4A", "GCK"))
    print("###############################")
    print(table(dataset_formatted$Gene, dataset_formatted$gene_interim, useNA = "ifany"))
    print("###############################")
    print(table(dataset_formatted$gene_interim))
  }
  
  dataset_formatted <- dataset_formatted %>%
    filter(gene_interim == "Keep") %>%
    select(-gene_interim)
  
  
  
  #:----- Proband
  
  ### Needs to be done, but require the specific variable
  
  
  
  
  
  #:------------------------------------------------------------------------
  ####
  ####    Set up variables
  ####
  
  
  #:----- Ethnicity
  dataset_formatted <- dataset_formatted %>%
    mutate(Ethnicity = ethnicity_clean) %>%
    select(-ethnicity_clean)
  
  #:----- Parent history
  dataset_formatted <- dataset_formatted %>%
    mutate(pardm = ifelse(
      # If parents have history
      MotherDM == "Yes" | FatherDM == "Yes",
      1,
      ifelse(
        # If parents do not have history: combinations: N/N, N/DK, N/NA, DK/N, NA/N
        (MotherDM == "No" & FatherDM == "No") | (MotherDM == "No" & FatherDM == "Don't Know") | (MotherDM == "No" & is.na(FatherDM)) | (MotherDM == "Don't Know" & FatherDM == "No") | (is.na(MotherDM) & FatherDM == "No"),
        0,
        NA
      )
    ))
  
  #:----- HbA1c (%)
  
  ### Need to choosen between HbA1c, Hba1c2, IFCCHBA1C (make sure they are all %)
  
  #:----- Current age
  dataset_formatted <- dataset_formatted %>%
    mutate(agerec = as.numeric(`Age Collected`))
  
  #:----- Age at diagnosis
  dataset_formatted <- dataset_formatted %>%
    mutate(agedx = as.numeric(`Age Diag`))
  
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
  
  #:----- BMI
  
  ### Need to make sure BMI values are within sensible ranges
  
  dataset_formatted <- dataset_formatted %>%
    mutate(bmi = as.numeric(BMI)) %>%
    select(-BMI)
  
  #:----- Treatment (insulin or tablets vs no insulin or tablets)
  
  ### Need to choose which are in treatment and which are not
  
  #:----- C-peptide
  
  ## Only do this for Type 1
  if (type == "Type 1") {
    
    # cpeptide > 200 or UCPCR > 0.2
    
    
  }
  
  #:----- Antibody
  
  ## Only do this for Type 1
  if (type == "Type 1") {
    
    
    
  }
  
  #:----- MODY outcome
  
  
  
  
  
  #:------------------------------------------------------------------------
  ####
  ####    Final formatting and output dataset
  ####
  
  
  
  
  
  
  
  
  
  
}

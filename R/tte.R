#' Prepare Time-to-Event data
#' 
#' Prepares time-to-event data from raw  UK Biobank \url{https://www.ukbiobank.ac.uk/} electronic medical record data.
#'
#' @param combined_data Optional. A data frame containing all specified variables. If left \code{NULL}, \code{tte} assumes individual data sets have been correctly generated in a user specified working directory.   
#' @param cancer_of_interest_ICD10 Character list specifying the ICD-10 code(s) of the cancer(s) or disease(s) of interest. A value must be specified.
#' @param prevalent_cancer_list Character list specifying the ICD-10 code(s) of prevalent cancer(s) or disease(s) to identify. Can be left empty ex. "c()".
#' @param prevalent_C_cancers Logical. If TRUE, will automatically include all ICD-10 codes that begin with 'C' to the \code{prevalent_cancer_list}. If FALSE (the default), will not include ICD-10 codes that begin with 'C' to the \code{prevalent_cancer_list}.  
#' @param incident_cancer_list Character list specifying the ICD10 code(s) of incident cancer(s) or disease(s) to identify. Can be left empty ex. "c()".
#' @param remove_prevalent_cancer Logical. If TRUE, will exclude cancer(s) or disease(s) specified in \code{prevalent_cancer_list} and/or \code{prevalent_C_cancers}. If FALSE (the default), will not exclude prevalent cancer(s) or disease(s).
#'  @param remove_self_reported_cancer Logical. If TRUE, will remove individuals that self reported as having cancer. If FALSE (the default), will not exclude individuals with self reported cancer. 
#'
#' @details This function will read-in raw UK Biobank data in 'RDS' format and generate time-to-event status and time based on user specified parameters. 
#' 
#' @return An object of class "list". This is a named list with the following columns:
#' 
#' \describe{
#' \item{\code{f.eid}}{UK Biobank generated ID.}
#' \item{\code{enrollment_date}}{Individual level UK Biobank enrollment date extracted during data cleaning.}
#' \item{\code{prevalent_cancer_date}}{Individual level UK Biobank prevalent cancer date extracted during data cleaning. Prevalent cancer codes are user specified within \code{prevalent_cancer_list} and \code{prevalent_C_cancers}.}
#' \item{\code{prevalent_cancer_type}}{Individual level UK Biobank prevalent cancer ICD10-10 code extracted during data cleaning. Prevalent cancer codes are user specified within \code{prevalent_cancer_list} and \code{prevalent_C_cancers}.}
#' \item{\code{prevalent_cancer}}{Prevalent cancer or disease status. Prevalent cancer codes are user specified within \code{prevalent_cancer_list} and \code{prevalent_C_cancers}.}
#' \item{\code{self_reported_cancer}}{Individual self reported as having cancer to UK Biobank.}
#' \item{\code{death_date}}{Individual level UK Biobank death date extracted during data cleaning.}
#' \item{\code{attrition_date}}{Individual level UK Biobank attrition (lost to follow-up) date extracted during data cleaning.}
#' \item{\code{case_date}}{Date of first cancer or disease of interest ICD-10 code. Codes are user specified within \code{cancer_of_interest_ICD10}.}
#' \item{\code{case_type}}{Individual level UK Biobank cancer or disease of interest ICD10-10 code extracted during data cleaning. Codes are user specified within \code{cancer_of_interest_ICD10}.}
#' \item{\code{case_control}}{Cancer or disease of interest status. Incident cancer codes are user specified within \code{cancer_of_interest_ICD10}.}
#' \item{\code{incident_date}}{Individual level UK Biobank incident cancer date extracted during data cleaning. Incident cancer codes are user specified within \code{incident_cancer_list}.}
#' \item{\code{incident_type}}{Individual level UK Biobank incident cancer ICD10-10 code extracted during data cleaning. Incident cancer codes are user specified within \code{incident_cancer_list}.}
#' \item{\code{incident_cancer}}{Incident cancer or disease status. Incident cancer codes are user specified within \code{incident_cancer_list}.}
#' \item{\code{censor_type_cancer_control}}{Study endpoint utilized for final time-to-event date. This end point controls for user specified incident cancer(s) or disease(s). Incident cancer codes are user specified within \code{incident_cancer_list}.}
#' \item{\code{censor_date_cancer_control}}{Study endpoint date corresponding to \code{censor_type_cancer_control} endpoint. This end point controls for user specified incident cancer(s) or disease(s). Incident cancer codes are user specified within \code{incident_cancer_list}.}
#' }
#' \item{\code{censor_days_cancer_control}}{Final time-to-event time in days. Calculated as the difference between \code{censor_date_cancer_control} and \code{enrollment_date}. This end point controls for user specified incident cancer(s) or disease(s). Incident cancer codes are user specified within \code{incident_cancer_list}.}
#' \item{\code{case_control_cancer_control}}{Final time-to-event status variable. This end point controls for user specified incident cancer(s) or disease(s). Incident cancer codes are user specified within \code{incident_cancer_list}.}
#' \item{\code{censor_type_cancer_ignore}}{Study endpoint utilized for final time-to-event date. This end point ignores any user specified incident cancer(s) or disease(s).}
#' \item{\code{censor_date_cancer_ignore}}{Study endpoint date corresponding to \code{censor_type_cancer_ignore} endpoint. This end point ignores any user specified incident cancer(s) or disease(s).}
#' \item{\code{censor_days_cancer_ignore}}{Final time-to-event time in days. Calculated as the difference between \code{censor_date_cancer_ignore} and \code{enrollment_date}. This end point ignores any user specified incident cancer(s) or disease(s).}
#' \item{\code{case_control_cancer_ignore}}{Final time-to-event status variable. This end point ignores any user specified incident cancer(s) or disease(s).}
#' }
#' 
#' @import tidyverse
#' @import data.table
#' @import lubridate
#' @export
#' 
#' @examples
#' # Load combined data from UKBBcleanR
#' mydata <- as.data.frame(combined_data)
#' # Set function parameters
#' cancer_outcome <- c("C911") # ICD 10 code of the cancer of interest
#' prevalent_cancers <- c("D37", "D38") # Prevalent ICD10 codes to exclude
#' incident_cancers <- c("C900") # Incident cancer to control for in your analysis
#' example_output<-  tte(combined_data= mydata, 
#'                      cancer_of_interest_ICD10= cancer_outcome, 
#'                      prevalent_cancer_list= prevalent_cancers, 
#'                      prevalent_C_cancers= TRUE, 
#'                      incident_cancer_list= incident_cancers, 
#'                      remove_prevalent_cancer=FALSE, 
#'                      remove_self_reported_cancer= FALSE
#'                      )
#' 
tte <- function(combined_data=NULL, cancer_of_interest_ICD10=c(), prevalent_cancer_list=c(), prevalent_C_cancers= TRUE, incident_cancer_list=c(), remove_prevalent_cancer=FALSE, remove_self_reported_cancer= FALSE){
  
  begin <- proc.time()
  
  #####################
  # Warnings Messages #
  #####################
  
  # User Cancer of interest
  if(length(cancer_of_interest_ICD10) == 0){
    stop("UKBBcleanR Error: Please specify the main cancer or outcome (ICD 10 code) of interest.")
  }
  
  
  #################
  # Load datasets #
  #################
  
  
  #################
  # Combined Data #
  #################
  if(!is.null(combined_data)){
    
    # User message
    message("UKBBcleanR Message: Loading data from user defined combined data set.")
    
  }  
  
  if(!is.null(combined_data)){
    
    # Split combined data into individual data sets
    
    # Enrollment
    enroll<- combined_data %>% dplyr::select(f.eid,f.53.0.0)
    
    # Cancer Registry Records
    cancer<- combined_data %>% dplyr::select(f.eid, starts_with("f.40005"), starts_with("f.40006"))
    
    # Inpatient Records
    inpatient<- combined_data %>% dplyr::select(f.eid, starts_with("f.41270"), starts_with("f.41280"))
    
    # Self-report Cancers
    self_report_cancer<- combined_data %>% dplyr::select(f.eid, starts_with("f.20001"))
    
    # Death
    death<- combined_data %>% dplyr::select(f.eid, starts_with("f.40000"))
    
    # Attrition
    attrition<- combined_data %>% dplyr::select(f.eid, f.190.0.0, f.191.0.0)
    
    # User message
    message("UKBBcleanR Message: Data successfully loaded from user defined combined data set.")
  }
  
  
  ########################
  # Individual Data sets #
  ########################
  if(is.null(combined_data)){
    
    # User message
    message("UKBBcleanR Message: Loading data from individual data sets.")
  }
  
  if(is.null(combined_data)){
    
    # Enrollment
    enroll<-readRDS("date_enroll.rds")
    
    # Cancer Registry Records
    cancer<-readRDS("cancer_reg.rds")
    
    # Inpatient Records
    inpatient<-readRDS("inpatient_data.rds")
    
    # Self-report Cancers
    self_report_cancer <- readRDS("self_report_cancer.rds")
    
    # Death
    death <- readRDS("death_reg.rds")
    
    # Attrition
    attrition<-readRDS("attrition.rds")
    
    
    # User message
    message("UKBBcleanR Message: Data successfully loaded from individual data sets.")
  }
  
  
  
  #######################
  # Clean each data set #
  #######################
  
  # User message
  message("UKBBcleanR Message: Beginning data cleaning:")
  
  ##############
  # Enrollment #
  ##############
  
  # Dataset with enrollment dates
  # This is the start time of their "time to event" calculation
  # Multiple dates select the earliest
  
  # Make all columns character variables 
  enroll[] <- lapply(enroll, as.character) # converts to character
  
  # creates a dataframe with earliest enrollment date
  enrollment_date <- enroll %>%
    mutate(enrollment_date = f.53.0.0) %>% # creates a new column with the earliest date
    dplyr::select(f.eid, enrollment_date) %>% # selects for id and enrollment date
    filter(!is.na(enrollment_date)) # removes na from the enrollment date column
  
  enrollment_date$enrollment_date <- as.Date(enrollment_date$enrollment_date) # converting character back to dates
  enrollment_date$f.eid <- as.integer(enrollment_date$f.eid) # converting character back to integer
  remove(enroll)
  
  # User message
  message("                    - Enrollment data completed.")
  
  
  ###########################
  # Cancer Registry Records #
  ###########################
  
  # manipulating cancer data 
  cancer[] <- lapply(cancer, as.character) # converts to character
  
  # function to remove ".0" from end of columns and remove "." from columns to elongate
  colchange <- function(x){
    colnames(x) <- gsub(".0$", "", colnames(x));x
    colnames(x) <- gsub("^f.4", "f4", colnames(x));x
  }
  cancer <- colchange(cancer) # use colchange function to reformat data
  
  # converts from wide to long
  cancer_long <- melt(setDT(cancer), measure.vars = list(grep("f40005", colnames(cancer), value = T), grep("f40006", colnames(cancer), value = T)),
                      variable.name = "var", value.name = c("cancer_date", "cancer_type")) 
  
  cancer_long2 <- cancer_long %>% filter(!is.na(cancer_type))
  
  names(cancer_long2) <- c("f.eid", "var", "disease_date", "disease_type") # matching column names to inpatient
  remove(cancer, cancer_long)
  
  # User message
  message("                    - Cancer Registry data completed.")
  
  
  ##################
  # Inpatient Data #
  ##################
  
  # reformatting
  inpatient[] <- lapply(inpatient, as.character) # converts to character
  colnames(inpatient) <- gsub("\\.0.\\b", ".", colnames(inpatient)) # removes ".0." from columns and replaces with "."
  
  # converts from wide to long
  inpatient_long <- melt(setDT(inpatient), measure.vars = list(grep("f.41280", colnames(inpatient), value = T), grep("f.41270", colnames(inpatient), value = T)),
                         variable.name = "var", value.name = c("disease_date", "disease_type")) 
  inpatient_long2 <- inpatient_long %>% filter(!is.na(disease_type))
  remove(inpatient, inpatient_long)
  
  # User message
  message("                    - Inpatient data completed.")
  
  
  #########################
  # Merge Cancer datasets #
  #########################
  
  # User message
  message("                    - Combining and cleaning cancer registry and inpatient data.")
  
  disease_long <- rbind(cancer_long2, inpatient_long2) # bind the long cancer and inpatient data
  
  # checking class
  sapply(disease_long, class)
  
  # manipulating data
  disease_long$disease_date <- as.Date(disease_long$disease_date) # converting character back to dates
  disease_long$f.eid <- as.integer(disease_long$f.eid) # converting character back to integer
  
  # joining data
  enrolled_disease <- left_join(enrollment_date, disease_long, by = "f.eid")
  sapply(enrolled_disease, class)
  remove(disease_long)
  
  
  ########################
  # Finding prior cancer #
  ########################
  
  if(prevalent_C_cancers== TRUE){  # identifies ICD 10 codes that begin with 'C'
    if(length(prevalent_cancer_list) == 0){ 
      # prior cancer #
      
      enrolled_disease_prior <- enrolled_disease %>% 
        mutate(prevalent_cancer =
                 ifelse(disease_date < enrollment_date & str_detect(disease_type, fixed("C", ignore_case = TRUE)) & !is.na(disease_type), 1, 0)
               
               
        )  # creates a logical variable if any f.eid had prior cancer
    }
    
    # if the excluded prevalent cancer has values
    if(length(prevalent_cancer_list) != 0){ 
      # prior cancer #
      
      enrolled_disease_prior <- enrolled_disease %>% 
        mutate(prevalent_cancer =
                 ifelse(disease_date < enrollment_date & str_detect(disease_type, fixed("C", ignore_case = TRUE)) & !is.na(disease_type) | 
                          disease_date < enrollment_date & str_detect(disease_type, str_c(prevalent_cancer_list, collapse = "|")) & !is.na(disease_type), 1, 0)
               
               
        )  # creates a logical variable if any f.eid had prior cancer
    }
  }
  
  
  if(prevalent_C_cancers== FALSE){  # Ignores ICD 10 codes that begin with 'C'
    if(length(prevalent_cancer_list) == 0){ 
      # prior cancer #
      
      enrolled_disease_prior <- enrolled_disease %>% 
        mutate(prevalent_cancer = 0
               
        )  # creates a logical variable if any f.eid had prior cancer
    }
    
    # if the excluded prevalent cancer has values
    if(length(prevalent_cancer_list) != 0){ 
      # prior cancer #
      
      enrolled_disease_prior <- enrolled_disease %>% 
        mutate(prevalent_cancer =
                 ifelse(disease_date < enrollment_date & str_detect(disease_type, str_c(prevalent_cancer_list, collapse = "|")) & !is.na(disease_type), 1, 0)
               
        )  # creates a logical variable if any f.eid had prior cancer
    }
  }
  
  prevalent_cancer <- enrolled_disease_prior[which(enrolled_disease_prior$prevalent_cancer==1),] # data set of prior cancer
  prevalent_cancer_person_level <- prevalent_cancer %>% group_by(f.eid) %>% filter(row_number()==1) # selects for only one ID - person level
  names(prevalent_cancer_person_level) <- c("f.eid", "enrollment_date", "var", "prevalent_cancer_date", "prevalent_cancer_type", "prevalent_cancer") # renames the data
  
  
  #######################
  # Self-report cancers # 
  #######################
  
  # Clean self-reported cancer data
  self_report_cancer <- self_report_cancer %>% filter_if(startsWith(names(.), "f.2"), any_vars(!is.na(.))) # filters for a report of cancer
  self_report_cancer$self_reported_cancer <- 1
  self_report_cancer_final <- self_report_cancer %>% dplyr::select(c(f.eid, self_reported_cancer))
  
  # User message
  message("                    - Self-reported data completed.")  
  
  
  #################
  # Death Records #
  ################# 
  
  # select for just death date
  death_date <- death %>% dplyr::select(f.eid, starts_with("f.40000"))
  
  # reformat dates
  death_date[-1] <- lapply(death_date[-1], as.Date, format = "%Y-%m-%d") # converts to date
  
  # selects for only one column of date of death 
  # used for joining
  death_date2 <- death_date %>%
    mutate(death_date = pmin(f.40000.0.0, f.40000.1.0, na.rm = TRUE)) %>% # creates a new column with the earliest date
    dplyr::select(f.eid, death_date)
  death_date2$death_date <-  as.Date(death_date2$death_date)
  remove(death, death_date)
  
  # User message
  message("                    - Death data completed.")    
  
  
  #############
  # Attrition #
  #############
  
  # lost to follow up data
  # selecting for just f.eid and the date 
  # used for join
  attrition_date <- attrition %>%
    dplyr::rename(attrition_date = f.191.0.0) %>%
    mutate(attrition_date = as.Date(attrition_date)) %>%
    dplyr::select(f.eid, attrition_date)
  remove(attrition)
  
  # User message
  message("                    - Attrition data completed.")    
  
  
  
  ###########################
  # Case control assignment #
  ###########################
  
  # User message
  message("UKBBcleanR Message: Generating Final time-to-event data.")
  
  # creates a new column for cases and controls  
  case_control <- enrolled_disease %>% filter(str_detect(disease_type, str_c(cancer_of_interest_ICD10, collapse = "|"))) %>% mutate(case_control = 1) %>% dplyr::select(f.eid, case_control)
  enrolled_disease_assign <- left_join(enrolled_disease, case_control, by = "f.eid") %>% mutate(case_control = ifelse(case_control == 1 & !is.na(case_control), 1, 0))
  
  # cancer of interest cases
  cancer_of_interest <- enrolled_disease_assign[which(enrolled_disease_assign$case_control == 1),] # creates cancer data set
  cancer_of_interest_person_level <- cancer_of_interest %>% group_by(f.eid) %>% filter(str_detect(disease_type, str_c(cancer_of_interest_ICD10, collapse = "|")) ) %>% filter(row_number() == 1) # cancer person level
  names(cancer_of_interest_person_level) <- c("f.eid", "enrollment_date", "var", "case_date", "case_type", "case_control")
  
  
  # Specified incident cancers 
  if(length(incident_cancer_list) != 0){
    incident_cancer <- enrolled_disease %>% mutate(incident_cancer = 
                                                     ifelse(disease_date > enrollment_date & str_detect(disease_type, str_c(incident_cancer_list, collapse = "|")) & !is.na(disease_type), 1, 0), # finds cases where the cancer of interest was diagnosed after a cancer was diagnosed
                                                   
    ) %>% filter(incident_cancer == 1)
    
    incident_cancer_person_level <- incident_cancer %>% group_by(f.eid) %>% arrange(f.eid, disease_date) %>% filter(row_number()==1)
    names(incident_cancer_person_level) <- c("f.eid", "enrollment_date", "var", "incident_date", "incident_type", "incident_cancer")
    
  }
  
  
  
  ################
  # Joining data #
  ################
  
  if(remove_prevalent_cancer == TRUE & remove_self_reported_cancer == TRUE){
    e <- left_join(enrollment_date, prevalent_cancer_person_level[, c("f.eid", "prevalent_cancer_date", "prevalent_cancer_type", "prevalent_cancer")], by = c("f.eid"))
    e1 <- left_join(e, self_report_cancer_final[, c("f.eid","self_reported_cancer")], by = c("f.eid"))
    
    e3 <- left_join(e1, death_date2, by = c("f.eid"))
    e4 <- left_join(e3, attrition_date, by = c("f.eid"))
    e5 <- left_join(e4, cancer_of_interest_person_level[, c("f.eid", "case_date", "case_type", "case_control")], by = c("f.eid"))
    
    if(length(incident_cancer_list) != 0){  
      e6 <- left_join(e5, incident_cancer_person_level[,c("f.eid", "incident_date", "incident_type", "incident_cancer")], by = c("f.eid"))
    } else{
      e6 <- e5 %>% mutate(incident_date = as.Date(NA))
    }
    
    e7 <- subset(e6, prevalent_cancer != 1 |  is.na(prevalent_cancer)) 
    e8 <- subset(e7, self_reported_cancer != 1 | is.na(self_reported_cancer)) 
    
    max_study_date <- max(enrolled_disease$disease_date, na.rm = TRUE) # date of most recent diagnosis
    
    
    e10 <- e8 %>% mutate(
      censor_type_cancer_control =
        ifelse(death_date == pmin(death_date, attrition_date, incident_date, case_date, na.rm = TRUE) & !is.na(death_date), "death",
               ifelse(attrition_date == pmin(death_date, attrition_date, incident_date, case_date, na.rm = TRUE) & !is.na(attrition_date), "attrition",
                      ifelse(incident_date == pmin(death_date, attrition_date, incident_date, case_date, na.rm = TRUE) & !is.na(incident_date), "other_cancer", 
                             ifelse(case_date == pmin(death_date, attrition_date, incident_date, case_date, na.rm = TRUE) & !is.na(case_date), paste0(cancer_of_interest_ICD10, collapse = " & "),  "study_end")))),
      
      censor_date_cancer_control =
        ifelse(death_date == pmin(death_date, attrition_date, incident_date, case_date, na.rm = TRUE) & !is.na(death_date), paste0(death_date),
               ifelse(attrition_date == pmin(death_date, attrition_date, incident_date, case_date, na.rm = TRUE) & !is.na(attrition_date), paste0(attrition_date),
                      ifelse(incident_date == pmin(death_date, attrition_date, incident_date, case_date, na.rm = TRUE) & !is.na(incident_date), paste0(incident_date), 
                             ifelse(case_date == pmin(death_date, attrition_date, incident_date, case_date, na.rm = TRUE) & !is.na(case_date), paste0(case_date),  paste0(max_study_date))))),
      
      
      
      censor_days_cancer_control = as.Date(censor_date_cancer_control) - enrollment_date, 
      
      case_control_cancer_control =
        ifelse(censor_type_cancer_control == paste0(cancer_of_interest_ICD10, collapse = " & "), 1,  0),
      
      censor_type_cancer_ignore =
        ifelse(death_date == pmin(death_date, attrition_date, case_date, na.rm = TRUE) & !is.na(death_date), "death",
               ifelse(attrition_date == pmin(death_date, attrition_date, case_date, na.rm = TRUE) & !is.na(attrition_date), "attrition",
                      ifelse(case_date == pmin(death_date, attrition_date, case_date, na.rm = TRUE) & !is.na(case_date), paste0(cancer_of_interest_ICD10, collapse = " & "),  "study_end"))),
      
      censor_date_cancer_ignore =
        ifelse(death_date == pmin(death_date, attrition_date, case_date, na.rm = TRUE) & !is.na(death_date), paste0(death_date),
               ifelse(attrition_date == pmin(death_date, attrition_date, case_date, na.rm = TRUE) & !is.na(attrition_date), paste0(attrition_date),
                      ifelse(case_date == pmin(death_date, attrition_date, case_date, na.rm = TRUE) & !is.na(case_date), paste0(case_date),  paste0(max_study_date)))),
      
      
      censor_days_cancer_ignore = as.Date(censor_date_cancer_ignore) - enrollment_date,
      
      case_control_cancer_ignore =
        ifelse(censor_type_cancer_ignore == paste0(cancer_of_interest_ICD10, collapse = " & "), 1,  0)
      
    ) 
    
  }
  
  if(remove_prevalent_cancer == TRUE & remove_self_reported_cancer == FALSE){
    e <- left_join(enrollment_date, prevalent_cancer_person_level[, c("f.eid", "prevalent_cancer_date", "prevalent_cancer_type", "prevalent_cancer")], by = c("f.eid"))
    e1 <- left_join(e, self_report_cancer_final[, c("f.eid","self_reported_cancer")], by = c("f.eid"))
    
    e3 <- left_join(e1, death_date2, by = c("f.eid"))
    e4 <- left_join(e3, attrition_date , by = c("f.eid"))
    e5 <- left_join(e4, cancer_of_interest_person_level[, c("f.eid", "case_date", "case_type", "case_control")], by = c("f.eid"))  
    
    if(length(incident_cancer_list) != 0){  
      e6 <- left_join(e5, incident_cancer_person_level[,c("f.eid", "incident_date", "incident_type", "incident_cancer")], by = c("f.eid"))
    } else{
      e6 <- e5 %>% mutate(incident_date = as.Date(NA))
    }
    
    e7 <- subset(e6, prevalent_cancer != 1 |  is.na(prevalent_cancer)) 
    
    
    max_study_date <- max(enrolled_disease$disease_date, na.rm = TRUE) # date of most recent diagnosis
    
    
    e10 <- e7 %>% mutate(
      censor_type_cancer_control =
        ifelse(death_date == pmin(death_date, attrition_date, incident_date, case_date, na.rm = TRUE) & !is.na(death_date), "death",
               ifelse(attrition_date == pmin(death_date, attrition_date, incident_date, case_date, na.rm = TRUE) & !is.na(attrition_date), "attrition",
                      ifelse(incident_date == pmin(death_date, attrition_date, incident_date, case_date, na.rm = TRUE) & !is.na(incident_date), "other_cancer", 
                             ifelse(case_date == pmin(death_date, attrition_date, incident_date, case_date, na.rm = TRUE) & !is.na(case_date), paste0(cancer_of_interest_ICD10, collapse = " & "),  "study_end")))),
      
      censor_date_cancer_control =
        ifelse(death_date == pmin(death_date, attrition_date, incident_date, case_date, na.rm = TRUE) & !is.na(death_date), paste0(death_date),
               ifelse(attrition_date == pmin(death_date, attrition_date, incident_date, case_date, na.rm = TRUE) & !is.na(attrition_date), paste0(attrition_date),
                      ifelse(incident_date == pmin(death_date, attrition_date, incident_date, case_date, na.rm = TRUE) & !is.na(incident_date), paste0(incident_date), 
                             ifelse(case_date == pmin(death_date, attrition_date, incident_date, case_date, na.rm = TRUE) & !is.na(case_date), paste0(case_date),  paste0(max_study_date))))),
      
      
      
      censor_days_cancer_control = as.Date(censor_date_cancer_control) - enrollment_date, 
      
      case_control_cancer_control =
        ifelse(censor_type_cancer_control == paste0(cancer_of_interest_ICD10, collapse = " & "), 1,  0),
      
      censor_type_cancer_ignore =
        ifelse(death_date == pmin(death_date, attrition_date, case_date, na.rm = TRUE) & !is.na(death_date), "death",
               ifelse(attrition_date == pmin(death_date, attrition_date, case_date, na.rm = TRUE) & !is.na(attrition_date), "attrition",
                      ifelse(case_date == pmin(death_date, attrition_date, case_date, na.rm = TRUE) & !is.na(case_date), paste0(cancer_of_interest_ICD10, collapse = " & "),  "study_end"))),
      
      censor_date_cancer_ignore =
        ifelse(death_date == pmin(death_date, attrition_date, case_date, na.rm = TRUE) & !is.na(death_date), paste0(death_date),
               ifelse(attrition_date == pmin(death_date, attrition_date, case_date, na.rm = TRUE) & !is.na(attrition_date), paste0(attrition_date),
                      ifelse(case_date == pmin(death_date, attrition_date, case_date, na.rm = TRUE) & !is.na(case_date), paste0(case_date),  paste0(max_study_date)))),
      
      
      censor_days_cancer_ignore = as.Date(censor_date_cancer_ignore) - enrollment_date,
      
      case_control_cancer_ignore =
        ifelse(censor_type_cancer_ignore == paste0(cancer_of_interest_ICD10, collapse = " & "), 1,  0)
      
    ) 
    
  }
  
  if(remove_prevalent_cancer == FALSE & remove_self_reported_cancer == TRUE){
    e <- left_join(enrollment_date, prevalent_cancer_person_level[, c("f.eid", "prevalent_cancer_date", "prevalent_cancer_type", "prevalent_cancer")], by = c("f.eid"))
    e1 <- left_join(e, self_report_cancer_final[, c("f.eid","self_reported_cancer")], by = c("f.eid"))
    
    e3 <- left_join(e1, death_date2, by = c("f.eid"))
    e4 <- left_join(e3, attrition_date , by = c("f.eid"))
    e5 <- left_join(e4, cancer_of_interest_person_level[, c("f.eid", "case_date", "case_type", "case_control")], by = c("f.eid"))  
    
    if(length(incident_cancer_list) != 0){  
      e6 <- left_join(e5, incident_cancer_person_level[,c("f.eid", "incident_date", "incident_type", "incident_cancer")], by = c("f.eid"))
    } else{
      e6 <- e5 %>% mutate(incident_date = as.Date(NA))
    } 
    
    e8 <- subset(e6, self_reported_cancer != 1 | is.na(self_reported_cancer)) 
    
    max_study_date <- max(enrolled_disease$disease_date, na.rm = TRUE) # date of most recent diagnosis
    
    
    e10 <- e8 %>% mutate(
      censor_type_cancer_control =
        ifelse(death_date == pmin(death_date, attrition_date, incident_date, case_date, na.rm = TRUE) & !is.na(death_date), "death",
               ifelse(attrition_date == pmin(death_date, attrition_date, incident_date, case_date, na.rm = TRUE) & !is.na(attrition_date), "attrition",
                      ifelse(incident_date == pmin(death_date, attrition_date, incident_date, case_date, na.rm = TRUE) & !is.na(incident_date), "other_cancer", 
                             ifelse(case_date == pmin(death_date, attrition_date, incident_date, case_date, na.rm = TRUE) & !is.na(case_date), paste0(cancer_of_interest_ICD10, collapse = " & "),  "study_end")))),
      
      censor_date_cancer_control =
        ifelse(death_date == pmin(death_date, attrition_date, incident_date, case_date, na.rm = TRUE) & !is.na(death_date), paste0(death_date),
               ifelse(attrition_date == pmin(death_date, attrition_date, incident_date, case_date, na.rm = TRUE) & !is.na(attrition_date), paste0(attrition_date),
                      ifelse(incident_date == pmin(death_date, attrition_date, incident_date, case_date, na.rm = TRUE) & !is.na(incident_date), paste0(incident_date), 
                             ifelse(case_date == pmin(death_date, attrition_date, incident_date, case_date, na.rm = TRUE) & !is.na(case_date), paste0(case_date),  paste0(max_study_date))))),
      
      
      
      censor_days_cancer_control = as.Date(censor_date_cancer_control) - enrollment_date, 
      
      case_control_cancer_control =
        ifelse(censor_type_cancer_control == paste0(cancer_of_interest_ICD10, collapse = " & "), 1,  0),
      
      censor_type_cancer_ignore =
        ifelse(death_date == pmin(death_date, attrition_date, case_date, na.rm = TRUE) & !is.na(death_date), "death",
               ifelse(attrition_date == pmin(death_date, attrition_date, case_date, na.rm = TRUE) & !is.na(attrition_date), "attrition",
                      ifelse(case_date == pmin(death_date, attrition_date, case_date, na.rm = TRUE) & !is.na(case_date), paste0(cancer_of_interest_ICD10, collapse = " & "),  "study_end"))),
      
      censor_date_cancer_ignore =
        ifelse(death_date == pmin(death_date, attrition_date, case_date, na.rm = TRUE) & !is.na(death_date), paste0(death_date),
               ifelse(attrition_date == pmin(death_date, attrition_date, case_date, na.rm = TRUE) & !is.na(attrition_date), paste0(attrition_date),
                      ifelse(case_date == pmin(death_date, attrition_date, case_date, na.rm = TRUE) & !is.na(case_date), paste0(case_date),  paste0(max_study_date)))),
      
      
      censor_days_cancer_ignore = as.Date(censor_date_cancer_ignore) - enrollment_date,
      
      case_control_cancer_ignore =
        ifelse(censor_type_cancer_ignore == paste0(cancer_of_interest_ICD10, collapse = " & "), 1,  0)
      
    ) 
    
  }
  
  if(remove_prevalent_cancer == FALSE & remove_self_reported_cancer == FALSE){
    e <- left_join(enrollment_date, prevalent_cancer_person_level[, c("f.eid", "prevalent_cancer_date", "prevalent_cancer_type", "prevalent_cancer")], by = c("f.eid"))
    e1 <- left_join(e, self_report_cancer_final[, c("f.eid","self_reported_cancer")], by = c("f.eid"))
    
    e3 <- left_join(e1, death_date2, by = c("f.eid"))
    e4 <- left_join(e3, attrition_date , by = c("f.eid"))
    e5 <- left_join(e4, cancer_of_interest_person_level[, c("f.eid", "case_date", "case_type", "case_control")], by = c("f.eid")) 
    
    if(length(incident_cancer_list) != 0){  
      e6 <- left_join(e5, incident_cancer_person_level[,c("f.eid", "incident_date", "incident_type", "incident_cancer")], by = c("f.eid"))
    } else{
      e6 <- e5 %>% mutate(incident_date = as.Date(NA))
    }
    
    max_study_date <- max(enrolled_disease$disease_date, na.rm = TRUE) # date of most recent diagnosis
    
    
    e10 <- e6 %>% mutate(
      censor_type_cancer_control =
        ifelse(death_date == pmin(death_date, attrition_date, incident_date, case_date, na.rm = TRUE) & !is.na(death_date), "death",
               ifelse(attrition_date == pmin(death_date, attrition_date, incident_date, case_date, na.rm = TRUE) & !is.na(attrition_date), "attrition",
                      ifelse(incident_date == pmin(death_date, attrition_date, incident_date, case_date, na.rm = TRUE) & !is.na(incident_date), "other_cancer", 
                             ifelse(case_date == pmin(death_date, attrition_date, incident_date, case_date, na.rm = TRUE) & !is.na(case_date), paste0(cancer_of_interest_ICD10, collapse = " & "),  "study_end")))),
      
      censor_date_cancer_control =
        ifelse(death_date == pmin(death_date, attrition_date, incident_date, case_date, na.rm = TRUE) & !is.na(death_date), paste0(death_date),
               ifelse(attrition_date == pmin(death_date, attrition_date, incident_date, case_date, na.rm = TRUE) & !is.na(attrition_date), paste0(attrition_date),
                      ifelse(incident_date == pmin(death_date, attrition_date, incident_date, case_date, na.rm = TRUE) & !is.na(incident_date), paste0(incident_date), 
                             ifelse(case_date == pmin(death_date, attrition_date, incident_date, case_date, na.rm = TRUE) & !is.na(case_date), paste0(case_date),  paste0(max_study_date))))),
      
      
      
      censor_days_cancer_control = as.Date(censor_date_cancer_control) - enrollment_date, 
      
      case_control_cancer_control =
        ifelse(censor_type_cancer_control == paste0(cancer_of_interest_ICD10, collapse = " & "), 1,  0),
      
      censor_type_cancer_ignore =
        ifelse(death_date == pmin(death_date, attrition_date, case_date, na.rm = TRUE) & !is.na(death_date), "death",
               ifelse(attrition_date == pmin(death_date, attrition_date, case_date, na.rm = TRUE) & !is.na(attrition_date), "attrition",
                      ifelse(case_date == pmin(death_date, attrition_date, case_date, na.rm = TRUE) & !is.na(case_date), paste0(cancer_of_interest_ICD10, collapse = " & "),  "study_end"))),
      
      censor_date_cancer_ignore =
        ifelse(death_date == pmin(death_date, attrition_date, case_date, na.rm = TRUE) & !is.na(death_date), paste0(death_date),
               ifelse(attrition_date == pmin(death_date, attrition_date, case_date, na.rm = TRUE) & !is.na(attrition_date), paste0(attrition_date),
                      ifelse(case_date == pmin(death_date, attrition_date, case_date, na.rm = TRUE) & !is.na(case_date), paste0(case_date),  paste0(max_study_date)))),
      
      
      censor_days_cancer_ignore = as.Date(censor_date_cancer_ignore) - enrollment_date,
      
      case_control_cancer_ignore =
        ifelse(censor_type_cancer_ignore == paste0(cancer_of_interest_ICD10, collapse = " & "), 1,  0)
      
    ) 
    
  }
  
  
  
  ##################################
  # Creat final time-to-event data #
  ##################################
  Time_to_Event_final <- e10 #  creating final dataset 
  
  
  
  ######################### 
  # Return Generated Data #
  #########################
  end <- proc.time()
  time.elapse <- round((end - begin)/60, 2)
  
  # User message
  message(paste0("UKBBcleanR Message: Cleaning completed. Total time: ", time.elapse[3], " min."))
  
  return(Time_to_Event_final)
}

####################
# End tte Function #
#################### 

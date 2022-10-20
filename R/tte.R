#' Prepare Time-to-Event data
#' 
#' Prepares time-to-event data from raw  UK Biobank \url{https://www.ukbiobank.ac.uk/} electronic medical record data.
#'
#' @param cancer_of_interest_ICD10 Character list specifying the ICD-10 code(s) of the cancer(s) or disease(s) of interest. A value must be specified.
#' @param prevalent_cancer_list Character list specifying the ICD10 code(s) of prevalent cancer(s) or disease(s) to identify. Can be left empty ex. "c()".
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
#' 
tte <- function(...) { 
  
  }

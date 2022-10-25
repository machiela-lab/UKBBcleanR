#' The UKBBcleanR Package
#'
#' Prepare electronic medical record data from the UK Biobank for time-to-event analyses
#'
#' @details Prepares time-to-event data from raw UK Biobank \url{https://www.ukbiobank.ac.uk/} electronic medical record data. The prepared data can be used for cancer outcomes, but could be modified for other health outcomes.
#' 
#' Key content of the 'UKBBcleanR' package include:\cr
#' 
#' \code{\link{tte}} Prepares time-to-event data from raw UK Biobank \url{https://www.ukbiobank.ac.uk/} electronic medical record data.
#' 
#' @name UKBBcleanR-package
#' @aliases UKBBcleanR-package UKBBcleanR
#' @docType package
#' 
#' @section Dependencies: The 'UKBBcleanR' package relies heavily upon \code{\link{tidyverse}} and \code{\link{data.table}} to clean raw UK Biobank data \url{https://www.ukbiobank.ac.uk/} and output a time-to-event data set. 
#' 
#' @author Alexander Depaulis\cr \emph{Integrative Tumor Epidemiology Branch (ITEB), Division of Cancer Epidemiology and Genetics (DCEG), National Cancer Institute (NCI), National Institutes of Health (NIH), Rockville, Maryland, USA} \cr
#' @author Derek W. Brown\cr \emph{ITEB, DCEG, NCI, NIH, Rockville, Maryland, USA} \cr
#' 
#' Maintainer: D.W.B. \email{derek.brown@@nih.gov}
#'
#' @keywords package
NULL

#' @import tidyverse
#' @import data.table
NULL

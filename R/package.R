#' The UKBBcleanR Package
#'
#' Prepare electronic medical record data from the UK Biobank for time-to-event analyses
#'
#' @details Prepares time-to-event data from raw UK Biobank \url{https://www.ukbiobank.ac.uk/} electronic medical record data. The prepared data can be used for cancer outcomes but could be modified for other health outcomes.
#' 
#' Key content of the 'UKBBcleanR' package include:\cr
#' 
#' \code{\link{tte}} Prepares time-to-event data from raw UK Biobank \url{https://www.ukbiobank.ac.uk/} electronic medical record data.
#' 
#' @name UKBBcleanR-package
#' @aliases UKBBcleanR-package UKBBcleanR
#' @docType package
#' 
#' @section Dependencies: The 'UKBBcleanR' package relies heavily upon \code{\link{data.table}}, \code{\link{dplyr}}, and \code{\link{stringr}} to clean raw UK Biobank data \url{https://www.ukbiobank.ac.uk/} and output a time-to-event data set. 
#' 
#' @author Alexander Depaulis\cr \emph{Integrative Tumor Epidemiology Branch (ITEB), Division of Cancer Epidemiology and Genetics (DCEG), National Cancer Institute (NCI), National Institutes of Health (NIH), Rockville, Maryland (MD), USA} \cr
#' @author Derek W. Brown\cr \emph{ITEB, DCEG, NCI, NIH, Rockville, MD, USA} \cr
#' @author Aubrey K. Hubbard\cr \emph{ITEB, DCEG, NCI, NIH, Rockville, MD, USA} \cr
#' 
#' Maintainer: D.W.B. \email{derek9@@gwmail.gwu.edu}
#'
#' @keywords package
NULL

#' @import dplyr
#' @importFrom data.table melt setDT
#' @importFrom stringr fixed str_c str_detect
NULL

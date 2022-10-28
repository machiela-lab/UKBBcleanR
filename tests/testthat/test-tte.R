context("tte")

cancer_outcome <- c("C911") # ICD-10 code of the cancer of interest
prevalent_cancers <- c("D37", "D38") # Prevalent ICD-10 codes to exclude
incident_cancers <- c("C900") # Incident cancer to control for in your analysis

################
# tte testthat #
################

test_that("tte works", {  
  
  # Describe Test
  expect_message(tte(combined_data = combined_data, 
                       cancer_of_interest_ICD10 = cancer_outcome, 
                       prevalent_cancer_list = prevalent_cancers, 
                       prevalent_C_cancers = TRUE, 
                       incident_cancer_list = incident_cancers, 
                       remove_prevalent_cancer = FALSE, 
                       remove_self_reported_cancer = FALSE)) 
}
)  

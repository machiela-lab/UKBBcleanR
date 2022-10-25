context("tte")

################
# tte testthat #
################

test_that("tte throws error with invalid arguments", {
  
  # Describe Test
  expect_error(tte(...))

}
)   

test_that("tte works", {  

  # Describe Test
  expect_message(tte(...)) 
  
  # Describe Test
  expect_silent(tte(...))
  
}
)  

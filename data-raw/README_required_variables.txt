## The following variables are required to run the 'tte' function within 'UKBBcleanR'
## Data can be loaded as indiviudal data sets or in a combined data frame. See Vignette for further details. 


###################
# Enrollment date #
###################

## UK Biobank Variables of interest: 

### eid= UK Biobank sample ID
### 53= Date of attending assessment centre

##Extracted fields into new file name='date_enroll.rds'



########################
# Cancer registry data #
########################

## UK Biobank Variables of interest: 

### eid= UK Biobank sample ID
### 40005= Date of cancer diagnosis
### 40006= Type of cancer: ICD10 Cancer register

## Note, there are multiple instances of these variables (multiple entries for participants). You need to use all instances. 

##Extracted fields into new file name= 'cancer_reg.rds'



##################
# Inpatient data #
##################

## UK Biobank Variables of interest: 

### eid= UK Biobank sample ID
### 41270= Diagnoses - ICD10 Summary Diagnoses
### 41280= Date of first in-patient diagnosis - ICD0 Summary Diagnoses

## Note, there are multiple instances of these variables (multiple entries for participants). You need to use all instances. 

##Extracted fields into new file name = 'inpatient_data.rds'



##################################
# Self-Reported Prevalent Cancer #
##################################

## UK Biobank Variables of interest: 

### eid= UK Biobank sample ID
### 20001 (0.0-0.5, 1.0-1.5, 2.0-2.5, 3.0-3.5)= 

##Extracted fields into new file name='self_report_cancer.rds'



##################
# Death Registry #
##################

## UK Biobank Variables of interest: 

### eid= UK Biobank sample ID
### 40000= Date of death

##Extracted fields into new file name= 'death_reg.rds'



#############
# Attrition #
#############

## UK Biobank Variables of interest: 

### eid= UK Biobank sample ID
### 190= Reason lost to follow-up
### 191= Date lost to follow-up

##Extracted fields into new file name='attrition.rds'



#######
# END #
#######
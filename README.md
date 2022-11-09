UKBBcleanR: Prepare electronic medical record data from the UK Biobank for time-to-event analyses <img src="man/figures/UKBBcleanR.png" width="120" align="right" />
===================================================

<!-- badges: start -->
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
![GitHub last commit](https://img.shields.io/github/last-commit/machiela-lab/UKBBcleanR)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7301712.svg)](https://doi.org/10.5281/zenodo.7301712)
<!-- badges: end -->

**Date repository last updated**: November 07, 2022

### Overview

The `UKBBcleanR` package contains an `R` function that prepares time-to-event data from raw [UK Biobank](https://www.ukbiobank.ac.uk/) electronic medical record data. The prepared data can be used for cancer outcomes, but could be modified for other health outcomes. This package is not available on [CRAN](https://cran.r-project.org/).

### Installation

<!-- 
To install the release version from CRAN:

    install.packages("UKBBcleanR")
-->

To install the development version from GitHub:

    devtools::install_github("machiela-lab/UKBBcleanR")

### Available function(s)

<table>
<colgroup>
<col width="30%" />
<col width="70%" />
</colgroup>
<thead>
<tr class="header">
<th>Function</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<td><code>tte</code></td>
<td>Prepares time-to-event data from raw <a href="https://www.ukbiobank.ac.uk/">UK Biobank</a> electronic medical record data.</td>
</tr>
</tbody>
<table>

The repository also includes the resources and code to create the project hex sticker.

### Authors

* **Alexander Depaulis** - *Integrative Tumor Epidemiology Branch (ITEB), Division of Cancer Epidemiology and Genetics (DCEG), National Cancer Institute (NCI), National Institutes of Health (NIH), Rockville, Maryland (MD), USA* - [GitHub](https://github.com/adepaulis1)

* **Derek W. Brown** - *ITEB, DCEG, NCI, NIH, Rockville, MD, USA* - [GitHub](https://github.com/derekbrown12) - [ORCID](https://orcid.org/0000-0001-8393-1713)

* **Aubrey K. Hubbard** - *ITEB, DCEG, NCI, NIH, Rockville, MD, USA* - [ORCID](https://orcid.org/0000-0003-4052-1110)

See also the list of [contributors](https://github.com/machiela-lab/UKBBcleanR/graphs/contributors) who participated in this package, including:

* **Ian D. Buller** - *Occupational and Environmental Epidemiology Branch, DCEG, NCI, NIH, Rockville, MD, USA* - [GitHub](https://github.com/idblr) - [ORCID](https://orcid.org/0000-0001-9477-8582)

* **Mitchell J. Machiela** - *ITEB, DCEG, NCI, NIH, Rockville, MD, USA* - [GitHub](https://github.com/machiela) - [ORCID](https://orcid.org/0000-0001-6538-9705)

### Getting Started

The `tte` function requires several raw [UK Biobank](https://www.ukbiobank.ac.uk/) variables to run correctly. A detailed list of required variables are provided in the [README_required_variables.txt](https://github.com/machiela-lab/UKBBcleanR/blob/main/data-raw/README_required_variables.txt) file.   

Data can be loaded in the `tte` function in two ways: 

* The user can specify a working directory using `setwd()` to where each individual data set is stored.  
    + NOTE: These individual data sets must contain the specific variables and have names which match the [README_required_variables.txt](https://github.com/machiela-lab/UKBBcleanR/blob/main/data-raw/README_required_variables.txt) file. Example data is available within the [package](https://github.com/machiela-lab/UKBBcleanR/tree/main/inst/extdata).

* The user can generate a single data set containing all the variables of interest. This data set can then be loaded into the `tte` function using the `combined_data` argument. Example data is available within the [package](https://github.com/machiela-lab/UKBBcleanR/tree/main/inst/extdata).

### Usage

``` r
# ------------------ #
# Necessary packages #
# ------------------ #

library(UKBBcleanR)

# -------- #
# Settings #
# -------- #

##### Input UKBBcleanR sample data

 # Use combined data set
 testdata <- as.data.frame(combined_data)
 
 # Set ICD-10 outcome of interest
 cancer_outcome <- c("C911") 
 
 # Set prevalent cancers to identify in data cleaning
 prevalent_cancers <- c("D37", "D38", "D39", "D40", "D41", "D42",
                        "D43", "D44", "D45", "D46", "D47", "D48") 
 
 # Set incident cancers to identify in data cleaning
 incident_cancers <- c("C900") 
 
# ------- #
# Run tte #
# ------- #

# Run without removing prevalent cancers from analysis
test1 <- tte(combined_data = testdata, 
             cancer_of_interest_ICD10 = cancer_outcome,
             prevalent_cancer_list = prevalent_cancers, 
             prevalent_C_cancers = TRUE, 
             incident_cancer_list = incident_cancers, 
             remove_prevalent_cancer = FALSE, 
             remove_self_reported_cancer = FALSE)
            
table(test1$case_control_cancer_ignore)  # tte outcome ignoring other incident cancers
table(test1$case_control_cancer_control) # tte outcome controlling for other incident cancers


# Run with removing prevalent cancers from analysis
test2 <- tte(combined_data = testdata, 
             cancer_of_interest_ICD10 = cancer_outcome,
             prevalent_cancer_list = prevalent_cancers, 
             prevalent_C_cancers = TRUE, 
             incident_cancer_list = incident_cancers, 
             remove_prevalent_cancer = TRUE, 
             remove_self_reported_cancer = TRUE)
table(test2$case_control_cancer_ignore)  # tte outcome ignoring other incident cancers
table(test2$case_control_cancer_control) # tte outcome controlling for other incident cancers
```

### Vignette

We provide a [vignette](https://htmlpreview.github.io/?https://github.com/machiela-lab/UKBBcleanR/blob/main/vignettes/vignette.html) with a practical example and work through of the provided [example data](https://github.com/machiela-lab/UKBBcleanR/tree/main/inst/extdata).  
    
### Funding

Package was developed while the first author was a participant of the 2022 [National Institutes of Health](https://www.nih.gov/) [Summer Internship Program in Biomedical Research](https://www.training.nih.gov/programs/sip) and while the second author was a postdoctoral fellow supported by the [Cancer Prevention Fellowship Program](https://cpfp.cancer.gov/) at the [National Cancer Institute](https://www.cancer.gov/) (NCI) and the third author was a postdoctoral fellow in the NCI [Division of Cancer Epidemiology and Genetics](https://dceg.cancer.gov/).

### Acknowledgments

When citing this package for publication, please cite follow:

    citation("UKBBcleanR")

### Questions? Feedback?

For questions about the package please contact the maintainer [Dr. Derek Brown](mailto:derek.brown@nih.gov) or [submit a new issue](https://github.com/machiela-lab/UKBBcleanR/issues).

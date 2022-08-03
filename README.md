UKBBcleanR: Prepare electronic medical record data from the UK Biobank for time-to-event analyses <img src="man/figures/UKBBcleanR.png" width="120" align="right" />
===================================================

<!-- badges: start -->
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

**Date repository last updated**: August 02, 2022

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
<td>Prepares time-to-event data from raw [UK Biobank](https://www.ukbiobank.ac.uk/) electronic medical record data.</td>
</tr>
</tbody>
<table>

The repository also includes the code to create the project hexsticker.

### Authors

* **Alexander Depaulis** - *Integrative Tumor Epidemiology Branch (ITEB), Division of Cancer Epidemiology and Genetics (DCEG), National Cancer Institute (NCI), National Institutes of Health (NIH), Rockville, Maryland, USA*

* **Derek W. Brown** - *ITEB, DCEG, NCI, NIH, Rockville, Maryland, USA* - [GitHub](https://github.com/derekbrown12) - [ORCID](https://orcid.org/0000-0001-8393-1713)

See also the list of [contributors](https://github.com/machiela-lab/UKBBcleanR/graphs/contributors) who participated in this package, including:

* **Ian D. Buller** - *Occupational and Environmental Epidemiology Branch, DCEG, NCI, NIH, , Rockville, Maryland, USA* - [GitHub](https://github.com/idblr) - [ORCID](https://orcid.org/0000-0001-9477-8582)

* **Mitchell J. Machiela** - *ITEB, DCEG, NCI, NIH, Rockville, Maryland, USA* - [GitHub](https://github.com/machiela) - [ORCID](https://orcid.org/0000-0001-6538-9705)

### Getting Started

* [INSERT ANY PREPARATION STEPS NECESSARY BEFORE RUNNING THE CODE HERE]

### Usage

``` r
# ------------------ #
# Necessary packages #
# ------------------ #

library(UKBBcleanR)

# -------- #
# Settings #
# -------- #

```

### Funding

Package was developed while the first author was a participant of the 2022 [National Institutes of Health](https://www.nih.gov/) [Summer Internship Program in Biomedical Research](https://www.training.nih.gov/programs/sip) and while the second author was a postdoctoral fellow supported by the [Cancer Prevention Fellowship Program](https://cpfp.cancer.gov/) at the [National Cancer Institute](https://www.cancer.gov/).

### Acknowledgments

When citing this package for publication, please cite follow:

    citation("UKBBcleanR")

### Questions? Feedback?

For questions about the package please contact the maintainer [Dr. Derek Brown](mailto:derek.brown@nih.gov) or [submit a new issue](https://github.com/machiela-lab/UKBBcleanR/issues).

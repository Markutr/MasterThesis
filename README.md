# Education-Specific Differences in Time Trends of Age-Stratified Back Pain Data of US Adults from 1997 to 2018

## Overview

This repository contains the code used in my master thesis, and will reproduce all figures used in the thesis. As publicly available data was used, it is also included in the repository.

In this thesis, complex survey data on chronic back pain in United States adults aged 25 to 84 in the period 1997 to 2018, acquired from the National Health Interview Survey was used to investigate temporal trends in back pain over age groups, periods, and birth cohorts. The data is acquired through the IPUMS (Blewett et al., 2023). As educational attainment is a powerful determinant of health, we wish to investigate these trends stratified by educational attainment, and look at differences between the educational groups. To this end, the Bayesian multivariate age-period-cohort model was used, with latent field priors and hyperpriors elicited using expert knowledge provided by a sociologist familiar with temporal analyses in the field of chronic pain. The complex survey design of the NHIS is also accounted for in the analysis using a recently-proposed two-step approach (Mercer et al., 2014). For completeness, the analysis also includes a prior sensitivity analysis and model selection procedures.

Since the priors used here takes a long time to generate (4-6 hours for each variant), they are included in the folder `./Code/Priors`.

## Features

- **Integrated Nested Laplace Approximation (INLA):** Utilizing the INLA package in R, this project implements the INLA framework for efficient and approximate Bayesian inference.

- **makemyprior:** The makemymrior package is employed to customize and fine-tune prior distributions, allowing for a flexible and tailored approach in the Bayesian modeling process.

- **survey:** The survey package in R was used to account for the complex survey design of the provided data. 

## Getting Started

1. Clone this repository: `git clone https://github.com/Markutr/MasterThesis.git`
2. Install the required R packages (check the imported packages in the provided guide "guide.qmd"): `install.packages(c("makemyprior", ...))`

For a guide on how to use the code to specify age-period-cohort models using a user specified combination of shared and education-specific effects, see "guide.qmd".

To reproduce all figures of the thesis, first set the working directory to be in the same folder as the scripts `setwd(".../Code")`. Then, do `source("Generate_all_figures.R")`

## Acknowledgments

I extend my greatest thanks to my supervisor Andrea Riebler, as well as my co-supervisor Ingeborg Hem Sørmoen for their excellent guidance, support and encouragement. 

I thank Anna Zajacova (Professor of Sociology, Western University) and Snehalata V. Huzurbazar (Acting Professor of Statistics, Emory University) for starting this project and letting me be part of it. Thank you for the fruitful discussions and comments. Thank you, Anna, for providing me with your sociological expert knowledge regarding backpain which allowed me to investigate prior elicitation as part of this thesis. 

## References

Blewett, L. A., Drew, J. A. R., King, M. L., Williams, K. C., Chen, A., Richards, S. and Westberry, M. (2023). IPUMS Health Surveys: National Health Interview Survey, Version 7.3 [dataset].

Mercer, L., Wakeﬁeld, J., Chen, C. and Lumley, T. (2014). A comparison of spatial smoothing methods for small area estimation with sampling weights, Spatial Statistics 8: 69–85. Spatial Statistics Miami.




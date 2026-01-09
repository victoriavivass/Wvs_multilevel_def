# Multilevel modeling of life satisfaction (WVS)

This repository contains the code and documentation for an applied multilevel modeling exercise conducted as part of the *Methods in Quantitative Sociology* course.

The analysis uses data from the World Values Survey (Wave 7) to study individual and contextual determinants of life satisfaction, with a particular focus on income, age, gender, and income inequality.

## Project structure

- `00_setup.R`  

- `01_empty_model.R`  
  Three-level empty multilevel model to decompose variance in life satisfaction across individuals, regions, and countries.

- `02_random_intercept_models.R`  
  Random-intercept models assessing the association between age, gender, and life satisfaction, and country rankings based on BLUPs.

- `03_random_slope_models.R`  
  Random-slope models examining the relationship between income and life satisfaction and its moderation by income inequality.

- `WVS-Multilevel-modeling.Rmd`  
  Main report containing the statistical analysis, tables, figures, and interpretation of results.

## Data

The data used in this project come from the World Values Survey (Wave 7) in csv. 

## Required packages

The analysis relies on the following R packages. They can be installed by running:
```
install.packages(c(
  "dplyr",
  "tibble",
  "knitr",
  "stringr",
  "readr",
  "lme4",
  "ggplot2",
  "performance",
  "ggeffects"
))
```
After installation, the packages can be loaded as follows:
```
library(dplyr)
library(tibble)
library(knitr)
library(stringr)
library(readr)
library(lme4)
library(ggplot2)
library(performance)
library(ggeffects)
```
## Author

María Victoria Vivas Gutiérrez

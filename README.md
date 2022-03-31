# A Dynamic Baseline Calibration Procedure for CGE models

Supplementary code for the paper:
Johannes Ziesmer & Ding Jin & Sneha Thube & Christian Henning. (2022). A Dynamic Baseline Calibration Procedure for CGE models. Computational Economics. https://doi.org/10.1007/s10614-022-10248-4

## Calibration Example

Install R (4.1), Rtools4 (if on Windows), RStudio (optional) and GAMS (37, most older and newer versions should also be fine). Upon opening the R project in the R-subdirectory renv will ask to restore the packages used, enter:

    renv::restore()

to automatically restore all used packages. If you are using Windows, the Code should automatically pick up your latest installed GAMS version. If you have installed GAMS to a non-standard location or are using another operating system please adapt R/steps/gams_path.R . 
Afterwards the full example can be run by the following two steps:

1. R/sample_generation.R
2. R/metamodels.R

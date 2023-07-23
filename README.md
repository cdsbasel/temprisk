# Comparing the Temporal Stability and Convergent Validity of Risk Preference Measures: A Meta-Analytic Approach

This repository contains the estimated test-retest correlations and inter-correlations from the primary data sources as well as all analysis scripts to replicate the analyses described in the manuscript.

### Overview of folders and files

* **var_info:** xlsx. and .csv files with the list of variables and risk preference measures for each panel/sample that are used for pre-processing and data analysis *(refer to var_info_file_codebook.csv for a description of the data columns)*. Includes codebook of risk preference measures (risk_measure_codebook.csv).

* **pre_processing**
  + code: scripts to select and pre-process the variables of interest from the raw data of each panel/sample.

* **processing**
  + code:
    + temp_stability: scripts to compute test-retest correlations using the pre-processed data of each panel/sample.
    + convergent_val: scripts to compute inter-correlations using the pre-processed data of each panel/sample.
  + output:
    + temp_stability: .csv files with the test-retest correlations of each panel/sample.
    + convergent_val: .csv files with the inter-correlations of each panel/sample.
  + processed_data_codebook.xlsx: description of the columns in the different .csv files.

* **analysis**
  + code:
    + temp_stability: scripts to conduct the analyses on the temporal stability of risk preference measures.
    + convergent_val: scripts to conduct the analyses on the convergent validity of risk preference measures.
  + output:
    + temp_stability: output of the analyses on the temporal stability of risk preference measures.
    + convergent_val: output of the analyses on the convergent validity of risk preference measures.


* **plotting**
  + code:
    + temp_stability: scripts to plot the test-retest correlations and analysis results.
    + convergent_val: scripts to plot the inter-correlations and analysis results.
  + output:
    + temp_stability: .png files of figures included in the manuscript.
    + convergent_val: .png files of figures included in the manuscript.



* **docs:** files for the companion website

### Running scripts

The details on how to run the scripts, the output obtained, and replicate the analyses reported in the manuscript can be found on the [companion website](https://cdsbasel.github.io/temprisk/workflow_overview.html).

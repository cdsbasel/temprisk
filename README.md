# Comparing the Temporal Stability and Convergent Validity of Risk Preference Measures: A Meta-Analytic Approach

### Overview

* **var_info:** xlsx. and .csv files with the list of variables and risk preference measures for each panel/sample that are used for pre-processing and data analysis. Includes codebook of risk preference measures.

* **pre_processing**
  + code: scripts that selects and pre-processes the variables of interest from the raw data of each panel/sample selected,

* **processing**
  + code:
    + temp_stability: scripts compute test-retest correlations for each panel/sample using the pre-processed data.
    + convergent_val: scripts compute inter-correlations for each panel/sample using the pre-processed data.
  + output:
    + temp_stability: .csv files with test-retest correlations for each panel/sample.
    + convergent_val: .csv files with inter-correlations for each panel/sample.


* **analysis**
  + code:
    + temp_stability: scripts to conduct the analyses on temporal stability,
    + convergent_val: scripts to conduct the analyses on convergent validity.
  + output:
    + temp_stability: output of the analyses on temporal stability,
    + convergent_val: output of the analyses on convergent validity.


* **plotting**
  + code:
    + temp_stability: scripts to plot test-retest correlations and analysis results.
    + convergent_val: scripts to plot inter-correlations and analysis results.
  + output:
    + temp_stability: .png files of figures included in the manuscript.
    + convergent_val: .png files of figures included in the manuscript.



### Running scripts

Details on how to run the scripts and replicate the analyses reported in the manuscript can be found on the [companion website](https://cdsbasel.github.io/temprisk/workflow_overview.html).

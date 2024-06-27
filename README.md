# Meta-Analyses of the Temporal Stability and Convergent Validity of Risk Preference Measures

This repository contains the processed data (i.e., test-retest correlations and inter-correlations from the primary data sources) and code to replicate the analyses reported in the manuscript _Meta-Analyses of the Temporal Stability and Convergent Validity of Risk Preference Measures_.

Below we briefly describe the study (see **[Abstract](#abstract)**), how this repository is organised (see **[Organization](#organization)**), how to replicate the analyses (see **[Scripts](#scripts)**), and provide a description of the structure and content of the different data files created/used within the processing and analysis steps  (see **[Codebook](#codebook)**).


Additional information on the data sets, analyses, and results is available on the **[companion website](https://cdsbasel.github.io/temprisk/index.html)**.


## Abstract

Understanding whether risk preference represents a stable, coherent trait is central to efforts aimed at explaining, predicting, and preventing risk-related behaviours. We help characterise the nature of the construct by adopting an individual participant data meta-analytic approach to summarise the temporal stability of over 350 risk preference measures (33 panels, 57 samples, >575,000 respondents). Our findings reveal significant heterogeneity across and within measure categories (propensity, frequency, behaviour), domains (e.g., investment, occupational, alcohol consumption), and sample characteristics (e.g., age). Specifically, while self-reported propensity and frequency measures of risk preference show a higher degree of stability relative to behavioural measures, these patterns are moderated by domain and age. Crucially, an analysis of convergent validity reveals a low agreement across measures, questioning the idea that they capture the same underlying phenomena. Our results raise concerns about the coherence and measurement of the risk preference construct.

## Organization

* **var_info:** 
  + indv_panel_var_info:
    + PANEL_risk_var_info.csv:  information on the variables from each panel/sample that are used for pre-processing.
    + PANEL_VariableInfo.xlsx:  information on the risk preference measures from each panel/sample that are used for data analysis.
    _Refer to the **[Codebook](#codebook)** section for a description of the structure of the different files._
  + risk_measure_codebook.csv: description (incl. full wording of questions/items and options) of risk preference measures considered for analysis
  + panel_risk_info.rds: complete list of information on the risk preference measures for each panel/sample
  + panel_variable_info.rds: complete list of information on the variables for each panel/sample
  + code: scripts that combine all .xlsx or .csv files from the _indv_panel_var_info_ folder to create the .rds files
 

* **pre_processing**
  + code: scripts to select and pre-process the variables of interest from the raw data of each panel/sample.

* **processing**
  + code:
    + temp_stability: scripts to compute test-retest correlations using the pre-processed data of each panel/sample.
    + convergent_val: scripts to compute inter-correlations using the pre-processed data of each panel/sample.
  + output:
    + temp_stability: .csv files with the test-retest correlations of all panels/samples combined as well as separate.
    + convergent_val: .csv files with the inter-correlations of of all panels/samples combined as well as separate.
    
    _Refer to the **[Codebook](#codebook)** section for a description of the columns in the different files._

* **analysis**
  + code:
    + temp_stability: scripts to conduct the analyses on the temporal stability of risk preference measures.
    + convergent_val: scripts to conduct the analyses on the convergent validity of risk preference measures.
  + output:
    + temp_stability: output of the analyses on the temporal stability of risk preference measures.
    + convergent_val: output of the analyses on the convergent validity of risk preference measures.
    
    _Refer to the **[Codebook](#codebook)** section for a description of the columns in the different output (.csv) files._


* **plotting**
  + code:
    + temp_stability: scripts to plot the test-retest correlations and analysis results.
    + convergent_val: scripts to plot the inter-correlations and analysis results.
  + output:
    + temp_stability: .png files of figures included in the manuscript.
    + convergent_val: .png files of figures included in the manuscript.


* **docs:** files (.rmd and imagaes) for the companion website

* main_data_files_codebook.xlsx: separate sheets containing the information on the structure and contents of main data files (_Information also displayed in the **[Codebook](#codebook)** section_)
* helper_functions.R: set of custom functions used for the processing and analysis of the data.
* temprisk_info_session.txt: Information on the R environment, installed packages and their versions used for the analyses.

## Scripts

A detailed description on how to run the scripts, on the output obtained, and on how to replicate the analyses reported in the manuscript can be found on the [companion website](https://cdsbasel.github.io/temprisk/workflow_overview.html). Additionally, each script contains a "Description" section. Lastly,  refer to the *temprisk_info_session.txt* file for information on the packages required for the analyses.


## Codebook

Description of the structure and content of the different data files created/used within the processing and analysis steps. Information also found in _main_data_files_codebook.xlsx_.


##### Analysis
- [cor_mat_convergent](#cor_mat_convergent)
- [summary_shapley_values_boot](#summary_shapley_values_boot)
- [summary_shapley_values](#summary_shapley_values)
- [shapley_values_check](#shapley_values_check)
- [shapley_values](#shapley_values)
- [shapley_values_boot](#shapley_values_boot)
- [convergent_val-masc_nlpar_pred](#convergent_val-masc_nlpar_pred)
- [temp_stability-masc_nlpar_pred](#temp_stability-masc_nlpar_pred)

##### Processing
- [complete_intercor_info](#complete_intercor_info)
- [complete_retest_info](#complete_retest_info)
- [agg_retest](#agg_retest)
- [retest_data](#retest_data)
- [agg_intercorr](#agg_intercorr)
- [intercor_data](#intercor_data)

##### Variable Info
- [risk_measure_codebook](#risk_measure_codebook)
- [VariableInfo_AND_risk_var_info](#variableinfo_and_risk_var_info)


<br>

#### VariableInfo_AND_risk_var_info                                                                       


- **filename(s): PANEL_VariableInfo.xlsx and PANEL_risk_var_info.csv**


- file description: Files containing information on the variables used for pre-processing and the risk preference measures included in the analyses.


- location: var_info/indv_panel_var_info/


|column             |description                                                                                                                                                                                                                                                           |type      |
|:------------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:---------|
|`panel`            |Name of the panel                                                                                                                                                                                                                                                     |character |
|`wave_id`          |ID of the wave                                                                                                                                                                                                                                                        |character |
|`varfile`          |Name of the file containing the data (original name when the file was downloaded)                                                                                                                                                                                     |character |
|`origin_varcode`   |Variable code in the original data                                                                                                                                                                                                                                    |character |
|`varcode`          |Standardized variable code                                                                                                                                                                                                                                            |character |
|`measure_category` |Measure category of the variable (pro, fre, beh)                                                                                                                                                                                                                      |character |
|`general_domain`   |Domain-general or domain-specific  variable (gen or dom)                                                                                                                                                                                                              |character |
|`domain_name`      |Abbreviated mame of the domain of the variable (e.g., smo, alc, inv, gen)                                                                                                                                                                                             |character |
|`scale_type`       |Type of response scale: ordinal (categorical variable with options that can be ranked), discrete (counts with clear range of possible responses, e.g., days in a month 0-30), open-ended (counts with no clear range), composite measure (sum of scores, proportions) |character |
|`scale_length`     |If ordinal or discrete, the number of options/possible responses                                                                                                                                                                                                      |numeric   |
|`time_frame`       |For frequency measures, the number of days the measure enquires about.                                                                                                                                                                                                |numeric   |
|`behav_type`       |For behavioural measures, the format of the task: lotteries, multiple price lists, willingness to pay/sell, allocation, dynamic                                                                                                                                       |character |
|`behav_paid`       |For behavioural measures, if it was incentivized or hypothetical                                                                                                                                                                                                      |character |
|`check_var`        |Has value 1 if it is a filter question in the survey                                                                                                                                                                                                                  |numeric   |
|`item_num`         |Number of items included in the measure                                                                                                                                                                                                                               |numeric   |
|`comment`          |comment                                                                                                                                                                                                                                                               |character |

 <br><br> 

#### risk_measure_codebook                                                                                


- **filename(s): risk_measure_codebook.csv**


- file description: Main file containing the detailed description of the risk preference measures included in the analyses.


- location: var_info/


|column             |description                                                                                                                               |type      |
|:------------------|:-----------------------------------------------------------------------------------------------------------------------------------------|:---------|
|`panel`            |Name of the panel                                                                                                                         |character |
|`measure_category` |Name of measure category (Propensity, Behavioural, Frequency)                                                                             |character |
|`general_domain`   |Whether it is general or domain-specific item                                                                                             |character |
|`domain_name`      |Name of the domain (e.g., smoking, alcohol, investment)                                                                                   |character |
|`scale_type`       |Type of scale (e.g., ordinal, open-ended)                                                                                                 |character |
|`scale_length`     |Number of response options (except for open-ended or composite measures)                                                                  |numeric   |
|`item_num`         |Number of items included in the measure                                                                                                   |numeric   |
|`time_frame`       |For frequency measures, the number of days the measure enquires about.                                                                    |numeric   |
|`survey_item`      |How was the item phrased (copy-pasted from the panel questionnaire/codebook)                                                              |character |
|`response_options` |Reponse options (copy-pasted from the panel questionnaire/codebook)                                                                       |character |
|`original_varcode` |Code of the variable in the panel codebook                                                                                                |character |
|`varcode`          |Standardized varcode label                                                                                                                |character |
|`info_source`      |Where was information on the survey item collected from                                                                                   |character |
|`comment`          |Additional comment (e.g., on which waves was this item included)                                                                          |character |
|`reverse_coding`   |In the pre-processing stage, do we need to reverse code the responses such that higher scores indicate more risk taking?  (Y(es) or N(o)) |character |
|`behav_type`       |For behavioural measures, the format of the task: lotteries, multiple price lists, willingness to pay/sell, allocation, dynamic           |character |
|`behav_paid`       |For behavioural measures, if it was incentivized or hypothetical                                                                          |character |
|`dependencies`     |Other variables to account for when pre-processing the data                                                                               |character |

 <br><br> 

#### intercor_data                                                                                        


- **filename(s): PANEL/complete_intercor_data.csv**


- file description: Files containing the intercorrelations between risk preference measures


- location: processing/output/convergent_val/


|column               |description                                                                                                                                                                                                                                                                         |type      |
|:--------------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:---------|
|`panel`              |name of panel                                                                                                                                                                                                                                                                       |character |
|`sample`             |name of sample                                                                                                                                                                                                                                                                      |character |
|`wave_id`            |ID of wave                                                                                                                                                                                                                                                                          |character |
|`wave_year`          |Year in wich data collection took place                                                                                                                                                                                                                                             |numeric   |
|`year_age_group`     |Size of age bins (i.e., 5, 10 or 20)                                                                                                                                                                                                                                                |numeric   |
|`age_group`          |Age group of respondents (e.g., "10-19")                                                                                                                                                                                                                                            |character |
|`age_mean`           |Mean age of respondents                                                                                                                                                                                                                                                             |numeric   |
|`age_median`         |Median age of respondents                                                                                                                                                                                                                                                           |numeric   |
|`age_min`            |Minimum age of respondents                                                                                                                                                                                                                                                          |numeric   |
|`age_max`            |Maximum age of respondents                                                                                                                                                                                                                                                          |numeric   |
|`age_sd`             |Standard deviation of age of respondents                                                                                                                                                                                                                                            |numeric   |
|`gender_group`       |Gender of respondents (i.e., female, male, all)                                                                                                                                                                                                                                     |character |
|`prop_female`        |Proportion of female respondents                                                                                                                                                                                                                                                    |numeric   |
|`n`                  |Sample size                                                                                                                                                                                                                                                                         |numeric   |
|`varcode_a`          |Name of variable A                                                                                                                                                                                                                                                                  |character |
|`varcode_b`          |Name of variable B                                                                                                                                                                                                                                                                  |character |
|`cor_pearson`        |Pearson correlation between variable A and B                                                                                                                                                                                                                                        |numeric   |
|`cor_spearman`       |Spearman correlation between variable A and B                                                                                                                                                                                                                                       |numeric   |
|`icc2_1`             |ICC between variable A and B                                                                                                                                                                                                                                                        |numeric   |
|`cor_pearson_log`    |Pearson correlation between variable A and B for log-transformed responses                                                                                                                                                                                                          |numeric   |
|`cor_spearman_log`   |Spearman correlation between variable A and B for log-transformed responses                                                                                                                                                                                                         |numeric   |
|`icc2_1_log`         |ICC between variable A and B for log-transformed responses                                                                                                                                                                                                                          |numeric   |
|`coeff_var_a`        |Coefficient of variation for variable A responses                                                                                                                                                                                                                                   |numeric   |
|`coeff_var_b`        |Coefficient of variation for variable B responses                                                                                                                                                                                                                                   |numeric   |
|`skewness_a`         |Skewness of variable A responses                                                                                                                                                                                                                                                    |numeric   |
|`skewness_b`         |Skewness of variable B responses                                                                                                                                                                                                                                                    |numeric   |
|`measure_category_a` |Measure category of variable A (pro, fre, beh)                                                                                                                                                                                                                                      |character |
|`general_domain_a`   |Whether variable A is a domain general or specific measure                                                                                                                                                                                                                          |character |
|`domain_name_a`      |Domain of variable A (e.g., smo, alc)                                                                                                                                                                                                                                               |character |
|`scale_type_a`       |Type of response scale of variable A: ordinal (categorical variable with options that can be ranked), discrete (counts with clear range of possible responses, e.g., days in a month 0-30), open-ended (counts with no clear range), composite measure (sum of scores, proportions) |character |
|`scale_length_a`     |If variable A is ordinal or discrete, the number of options/possible responses                                                                                                                                                                                                      |numeric   |
|`time_frame_a`       |If variable A is a frequency measure, the number of days the measure enquires about.                                                                                                                                                                                                |numeric   |
|`behav_type_a`       |If variable A is a behavioural measure, the format of the task: lotteries, multiple price lists, willingness to pay/sell, allocation, dynamic                                                                                                                                       |character |
|`behav_paid_a`       |If variable A is a behavioural measure, if it was incentivized or hypothetical                                                                                                                                                                                                      |character |
|`item_num_a`         |Number of items included in variable A                                                                                                                                                                                                                                              |numeric   |
|`measure_category_b` |Measure category of variable B (pro, fre, beh)                                                                                                                                                                                                                                      |character |
|`general_domain_b`   |Whether variable B is a domain general or specific measure                                                                                                                                                                                                                          |character |
|`domain_name_b`      |Domain of variable B (e.g., smo, alc)                                                                                                                                                                                                                                               |character |
|`scale_type_b`       |Type of response scale of variable B: ordinal (categorical variable with options that can be ranked), discrete (counts with clear range of possible responses, e.g., days in a month 0-30), open-ended (counts with no clear range), composite measure (sum of scores, proportions) |character |
|`scale_length_b`     |If variable B is ordinal or discrete, the number of options/possible responses                                                                                                                                                                                                      |numeric   |
|`time_frame_b`       |If variable B is a frequency measure, the number of days the measure enquires about.                                                                                                                                                                                                |numeric   |
|`behav_type_b`       |If variable B is a behavioural measure, the format of the task: lotteries, multiple price lists, willingness to pay/sell, allocation, dynamic                                                                                                                                       |character |
|`behav_paid_b`       |If variable B is a behavioural measure, if it was incentivized or hypothetical                                                                                                                                                                                                      |character |
|`item_num_b`         |Number of items included in variable B                                                                                                                                                                                                                                              |numeric   |
|`continent`          |Continent where data collection took place                                                                                                                                                                                                                                          |character |
|`country`            |Country where data collection took place                                                                                                                                                                                                                                            |character |
|`language`           |Language of survey                                                                                                                                                                                                                                                                  |character |
|`data_collect_mode`  |Mode of data collection used across most waves: interview, online, laboratory, self-administered (e.g., survey sent via post)                                                                                                                                                      |character |
|`sample_type`        |Type of population who partakes in the survey: adolescents, adults, older adults, lifespan                                                                                                                                                                                          |character |

 <br><br> 

#### agg_intercorr                                                                                        


- **filename(s): agg_intercor_data.csv**


- file description: Files containing the aggregated intercorrelations between risk preference measures


- location: processing/output/convergent_val/


|column              |description                                                                                                                    |type      |
|:-------------------|:------------------------------------------------------------------------------------------------------------------------------|:---------|
|`panel`             |Name of panel                                                                                                                  |character |
|`sample`            |Name of sample                                                                                                                 |character |
|`continent`         |Continent where data collection took place                                                                                     |character |
|`country`           |Country where data collection took place                                                                                       |character |
|`language`          |Language of survey                                                                                                             |character |
|`data_collect_mode` |Mode of data collection used across most waves: interview, online, laboratory, self-administered (e.g., survey sent via post) |character |
|`sample_type`       |Type of population who partakes in the survey: adolescents, adults, older adults, lifespan                                     |character |
|`age_group`         |Age group of respondents (e.g., "10-19")                                                                                       |character |
|`gender_group`      |Gender of respondents (i.e., female, male, all)                                                                                |character |
|`meas_pair_lbl`     |Measure category pair label (e.g., propensity-frequency)                                                                       |character |
|`domain_pair_lbl`   |Measure category-domain pair label (e.g., Propensity-General_Frequency-Smoking)                                                |character |
|`n_mean`            |Mean sample size of correlations                                                                                               |numeric   |
|`n_sd`              |Standard deviation of sample sizes of the correlations                                                                         |numeric   |
|`mean_age`          |Mean age of the respondents (i.e., mean of the mean age of respondents )                                                       |numeric   |
|`sd_age`            |Standard deviation of the mean age of the respondents                                                                          |numeric   |
|`cor_num`           |Number of correlations included to calculate the aggregate estimate                                                            |numeric   |
|`wcor_z`            |Fisher's z values of the aggregated inter-correlation                                                                          |numeric   |
|`vi_z`              |Sampling variance of Fisher's z aggregated estimate                                                                            |numeric   |
|`sei_z`             |Square root of vi_z                                                                                                            |numeric   |
|`ci_lb_z`           |Lower 95% bound of Fisher's z aggregate                                                                                        |numeric   |
|`ci_ub_z`           |Upper 95% bound of Fisher's z aggregate                                                                                        |numeric   |
|`wcor`              |z-to-r transformed aggregated inter-correlation                                                                                |numeric   |
|`ci_lb`             |Lower 95% bound of r inter-correlation aggregate                                                                               |numeric   |
|`ci_ub`             |Upper 95% bound of r inter-correlation aggregate                                                                               |numeric   |
|`sei`               |Square root of vi                                                                                                              |numeric   |
|`vi`                |Sampling variance of r aggregated estimate                                                                                     |numeric   |
|`es_id`             |ID of effect size                                                                                                              |numeric   |
|`age_bin`           |Age binning (5, 10, or 20-year bins)                                                                                           |numeric   |
|`min_n`             |Minimum sample size of correlations included to compute the aggregated estimates                                               |numeric   |
|`rho_val`           |Correlation between sampling errors of effect sizes being aggregated                                                           |numeric   |
|`data_transform`    |Whether correlations were computed from the non or log-transformed responses                                                   |character |
|`cor_metric`        |Correlation metric: pearson, spearman, ICC                                                                                     |character |

 <br><br> 

#### retest_data                                                                                          


- **filename(s): PANEL/complete_retest_data.csv**


- file description: Files containing the retest correlation for each risk preference measure


- location: processing/output/temp_stability/


|column              |description                                                                                                                                                                                                                                                           |type      |
|:-------------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:---------|
|`panel`             |name of panel                                                                                                                                                                                                                                                         |character |
|`sample`            |name of sample                                                                                                                                                                                                                                                        |character |
|`wave_id_t1`        |ID of wave at T1                                                                                                                                                                                                                                                      |character |
|`wave_year_t1`      |Year in wich data collection took place at T1                                                                                                                                                                                                                         |numeric   |
|`wave_id_t2`        |ID of wave at T2                                                                                                                                                                                                                                                      |character |
|`wave_year_t2`      |Year in wich data collection took place at T2                                                                                                                                                                                                                         |numeric   |
|`time_diff_mean`    |Mean time difference (in years) between T1 and T2                                                                                                                                                                                                                     |numeric   |
|`time_diff_median`  |Median time difference (in years) between T1 and T2                                                                                                                                                                                                                   |numeric   |
|`time_diff_min`     |Minimum time difference (in years) between T1 and T2                                                                                                                                                                                                                  |numeric   |
|`time_diff_max`     |Maximum time difference (in years) between T1 and T2                                                                                                                                                                                                                  |numeric   |
|`time_diff_sd`      |Standard deviaton of  time difference (in years) between T1 and T2                                                                                                                                                                                                    |numeric   |
|`year_age_group`    |Size of age bins (i.e., 5, 10 or 20)                                                                                                                                                                                                                                  |numeric   |
|`age_group`         |Age group of respondents (e.g., "10-19")                                                                                                                                                                                                                              |character |
|`age_mean`          |Mean age of respondents                                                                                                                                                                                                                                               |numeric   |
|`age_median`        |Median age of respondents                                                                                                                                                                                                                                             |numeric   |
|`age_min`           |Minimum age of respondets                                                                                                                                                                                                                                             |numeric   |
|`age_max`           |Maximum age of respondenrts                                                                                                                                                                                                                                           |numeric   |
|`age_sd`            |Standard deviation of age of respondents                                                                                                                                                                                                                              |numeric   |
|`gender_group`      |Gender of respondents (i.e., female, male, all)                                                                                                                                                                                                                       |character |
|`prop_female`       |Proportion of female respondents                                                                                                                                                                                                                                      |numeric   |
|`n`                 |Number of respondents                                                                                                                                                                                                                                                 |numeric   |
|`attrition_rate`    |Proportion of T1 respondents missing at T2                                                                                                                                                                                                                            |numeric   |
|`varcode`           |Variable code of measure                                                                                                                                                                                                                                              |character |
|`cor_pearson`       |Pearson correlation between responses at T1 and T2                                                                                                                                                                                                                    |numeric   |
|`cor_spearman`      |Spearman correlation between responses at T1 and T2                                                                                                                                                                                                                   |numeric   |
|`icc2_1`            |ICC between responses at T1 and T2                                                                                                                                                                                                                                    |numeric   |
|`cor_pearson_log`   |Pearson correlation between responses at T1 and T2 for log-transformed responses                                                                                                                                                                                      |numeric   |
|`cor_spearman_log`  |Spearman correlation between responses at T1 and T2 for log-transformed responses                                                                                                                                                                                     |numeric   |
|`icc2_1_log`        |ICC between responses at T1 and T2 for log-transformed responses                                                                                                                                                                                                      |numeric   |
|`coeff_var_t1`      |Coefficient of variation for variable T1 responses                                                                                                                                                                                                                    |numeric   |
|`coeff_var_t2`      |Coefficient of variation for variable T2 responses                                                                                                                                                                                                                    |numeric   |
|`skewness_t1`       |Skewness of variable T1 responses                                                                                                                                                                                                                                     |numeric   |
|`skewness_t2`       |Skewness of variable T2 responses                                                                                                                                                                                                                                     |numeric   |
|`measure_category`  |Measure category of the variable (pro, fre, beh)                                                                                                                                                                                                                      |character |
|`general_domain`    |Domain-general or domain-specific variable (gen or dom)                                                                                                                                                                                                               |character |
|`domain_name`       |Name of domain of variable (e.g., smo, alc)                                                                                                                                                                                                                           |character |
|`scale_type`        |Type of response scale: ordinal (categorical variable with options that can be ranked), discrete (counts with clear range of possible responses, e.g., days in a month 0-30), open-ended (counts with no clear range), composite measure (sum of scores, proportions) |character |
|`scale_length`      |If ordinal or discrete, the number of options/possible responses                                                                                                                                                                                                      |numeric   |
|`time_frame`        |For frequency measures, the number of days the measure enquires about.                                                                                                                                                                                                |numeric   |
|`behav_type`        |For behavioural measures, the format of the task: lotteries, multiple price lists, willingness to pay/sell, allocation, dynamic                                                                                                                                       |character |
|`behav_paid`        |For behavioural measures, if it was incentivized or hypothetical                                                                                                                                                                                                      |character |
|`item_num`          |Number of items included in the measure                                                                                                                                                                                                                               |numeric   |
|`continent`         |Continent where data collection took place                                                                                                                                                                                                                            |character |
|`country`           |Country where data collection took place                                                                                                                                                                                                                              |character |
|`language`          |Language of survey                                                                                                                                                                                                                                                    |character |
|`data_collect_mode` |Mode of data collection used across most waves: interview, online, laboratory, self-administered (e.g., survey sent via post)                                                                                                                                        |character |
|`sample_type`       |Type of population who partakes in the survey: adolescents, adults, older adults, lifespan                                                                                                                                                                            |character |

 <br><br> 

#### agg_retest                                                                                           


- **filename(s): agg_retest_data.csv**


- file description: Files containing aggregated retest correlations


- location: processing/output/temp_stability/


|column              |description                                                                                                                    |type      |
|:-------------------|:------------------------------------------------------------------------------------------------------------------------------|:---------|
|`panel`             |Name of panel                                                                                                                  |character |
|`sample`            |Name of sample                                                                                                                 |character |
|`continent`         |Continent where data collection took place                                                                                     |character |
|`country`           |Country where data collection took place                                                                                       |character |
|`language`          |Language of survey                                                                                                             |character |
|`data_collect_mode` |Mode of data collection used across most waves: interview, online, laboratory, self-administered (e.g., survey sent via post) |character |
|`sample_type`       |Type of population who partakes in the survey: adolescents, adults, older adults, lifespan                                     |character |
|`time_diff_bin`     |Rounded mean time difference between T1 and T2                                                                                 |numeric   |
|`age_group`         |Age group of respondents (e.g., "10-19")                                                                                       |character |
|`gender_group`      |Gender of respondents (i.e., female, male, all)                                                                                |character |
|`measure_category`  |Measure category of the variable (pro, fre, beh)                                                                               |character |
|`domain_name`       |Name of domain of variable (e.g., smo, alc)                                                                                    |character |
|`item_num`          |Whether measures are "single item" or "multi item" measures                                                                    |character |
|`n_mean`            |Mean sample size of correlations                                                                                               |numeric   |
|`n_sd`              |Standard deviation of sample sizes of the correlations                                                                         |numeric   |
|`mean_age`          |Mean age of the respondents (i.e., mean of the mean age of respondents )                                                       |numeric   |
|`sd_age`            |Standard deviation of the mean age of the respondents                                                                          |numeric   |
|`mean_attrition`    |Mean attrition rate between T1 and T2                                                                                          |numeric   |
|`sd_attrition`      |Standard deviation of attrition rate between T1 and T2                                                                         |numeric   |
|`cor_num`           |Number of correlations included to calculate the aggregate estimate                                                            |numeric   |
|`wcor_z`            |Fisher's z value of the aggregated retest correlation                                                                          |numeric   |
|`vi_z`              |Sampling variance of Fisher's z aggregated estimate                                                                            |numeric   |
|`sei_z`             |Square root of vi_z                                                                                                            |numeric   |
|`ci_lb_z`           |Lower 95% bound of Fisher's z aggregate                                                                                        |numeric   |
|`ci_ub_z`           |Upper 95% bound of Fisher's z aggregate                                                                                        |numeric   |
|`wcor`              |z-to-r transformed aggregated retest correlation                                                                               |numeric   |
|`ci_lb`             |Lower 95% bound of r retest aggregate                                                                                          |numeric   |
|`ci_ub`             |Upper 95% bound of r retest aggregate                                                                                          |numeric   |
|`sei`               |Square root of vi                                                                                                              |numeric   |
|`vi`                |Sampling variance of aggregated estimate                                                                                       |numeric   |
|`es_id`             |Effect size id                                                                                                                 |numeric   |
|`age_bin`           |Age binning (5, 10, or 20-year bins)                                                                                           |numeric   |
|`min_n`             |Minimum sample size of correlations included to compute the aggregated estimates (30, 100, or 250)                             |numeric   |
|`month_bin`         |Binning of time difference (3, 6, or 12-month bins)                                                                            |numeric   |
|`rho_val`           |Correlation between sampling errors of effect sizes being aggregated                                                           |numeric   |
|`data_transform`    |Whether correlations were computed from the non or log-transformed responses                                                   |character |
|`cor_metric`        |Correlation metric: pearson, spearman, ICC                                                                                     |character |

 <br><br> 

#### complete_retest_info                                                                                 


- **filename(s): complete_retest_info.csv**


- file description: File containing summary information on the amount of data and number of retest correlations analysed in each sample (used to create the flowchart)


- location: processing/output/temp_stability/


|column              |description                                                                                                                    |type      |
|:-------------------|:------------------------------------------------------------------------------------------------------------------------------|:---------|
|`panel`             |Name of panel                                                                                                                  |character |
|`sample`            |Name of sample                                                                                                                 |character |
|`continent`         |Continent where data collection took place                                                                                     |character |
|`country`           |Country where data collection took place                                                                                       |character |
|`sample_type`       |Type of population who partakes in the survey: adolescents, adults, older adults, lifespan                                     |character |
|`collect_mode`      |Mode of data collection used across most waves: interview, online, laboratory, self-administered (e.g., survey sent via post) |character |
|`measure_categ`     |List of the categories of the measures anaylsed (pro, fre, beh)                                                                |character |
|`domain_name`       |List of the domains of the measures anaylsed (e.g., smo, alc)                                                                  |character |
|`unique_meas`       |Number of unique measures                                                                                                      |numeric   |
|`unique_waves`      |Number of waves                                                                                                                |numeric   |
|`retest_int_min`    |Minimum retest interval (years)                                                                                                |numeric   |
|`retest_int_median` |Median retest interval (years)                                                                                                 |numeric   |
|`retest_int_mean`   |Mean retest interval (years)                                                                                                   |numeric   |
|`retest_int_max`    |Maximum retest interval (years)                                                                                                |numeric   |
|`cor_num`           |Number of correlations analysed                                                                                                |numeric   |
|`unique_id`         |Number of unique respondents                                                                                                   |numeric   |
|`unique_resp`       |Number of unique responses                                                                                                     |numeric   |

 <br><br> 

#### complete_intercor_info                                                                               


- **filename(s): complete_intercor_info.csv**


- file description: File containing summary information on the amount of data and number of intercorrelations analysed in each sample (used to create the flowchart)


- location: processing/output/convergent_val/


|column          |description                                                                                                                    |type      |
|:---------------|:------------------------------------------------------------------------------------------------------------------------------|:---------|
|`panel`         |Name of panel                                                                                                                  |character |
|`sample`        |Name of sample                                                                                                                 |character |
|`continent`     |Continent where data collection took place                                                                                     |character |
|`country`       |Country where data collection took place                                                                                       |character |
|`collect_mode`  |Mode of data collection used across most waves: interview, online, laboratory, self-administered (e.g., survey sent via post) |character |
|`measure_categ` |List of the categories of the measures analysed (pro, fre, beh)                                                                |character |
|`domain_name`   |List of the domains of the measures analysed (e.g., smo, alc)                                                                  |character |
|`unique_meas`   |Number of unique measures                                                                                                      |numeric   |
|`cor_num`       |Number of correlations analysed                                                                                                |numeric   |
|`unique_id`     |Number of unique respondents                                                                                                   |numeric   |
|`unique_resp`   |Number of unique responses                                                                                                     |numeric   |

 <br><br> 

#### temp_stability-masc_nlpar_pred                                                                       


- **filename(s): temp_stability/masc_nlpar_pred.csv**


- file description:  Summary values of  MASC parameter estimates for risk preference and other psych constructs (plot to compare constructs, predictor of interest is domain, all other predictors are set to 0 )


- location: analysis/output/temp_stability/


|column          |description                                          |type      |
|:---------------|:----------------------------------------------------|:---------|
|`categ`         |Name of the predictor (e.g., domain,)                |character |
|`x`             |Name of the predictor level (e.g., smo, inv)         |character |
|`measure`       |Measure category (Propensity, Behaviour, Frequency)  |character |
|`nlpar`         |Name of the non linear parameter                     |character |
|`.epred`        |Mean value of the paramter estimate                  |numeric   |
|`.lower_0.95`   |Value of the lower 95% HDI of the parameter estimate |numeric   |
|`.lower_0.8`    |Value of the lower 80% HDI of the parameter estimate |numeric   |
|`.lower_0.5`    |Value of the lower 50% HDI of the parameter estimate |numeric   |
|`.upper_0.95`   |Value of the upper 95% HDI of the parameter estimate |numeric   |
|`.upper_0.8`    |Value of the upper 80% HDI of the parameter estimate |numeric   |
|`.upper_0.5`    |Value of the upper 50% HDI of the parameter estimate |numeric   |
|`sub_component` |Relabeled x variable                                 |character |

 <br><br> 

#### convergent_val-masc_nlpar_pred                                                                       


- **filename(s): convergent_val/masc_nlpar_pred.csv**


- file description: Summary ofMASC parameter estimates for risk preference for different predictor values (used for variance decomp. analysis)


- location: analysis/output/convergent_val/


|column          |description                                          |type      |
|:---------------|:----------------------------------------------------|:---------|
|`categ`         |Name of the predictor (e.g., domain,)                |character |
|`x`             |Name of the predictor level (e.g., smo, inv)         |character |
|`measure`       |Measure category (Propensity, Behaviour, Frequency)  |character |
|`age_group`     |Age group (e.g., "20-30")                            |character |
|`gender_group`  |Gender group (male, female)                          |character |
|`nlpar`         |Name of the non-linear parameter                     |character |
|`.epred`        |Mean value of the parameter estimate                  |numeric   |
|`.lower_0.95`   |Value of the lower 95% HDI of the parameter estimate |numeric   |
|`.lower_0.8`    |Value of the lower 80% HDI of the parameter estimate |numeric   |
|`.lower_0.5`    |Value of the lower 50% HDI of the parameter estimate |numeric   |
|`.upper_0.95`   |Value of the upper 95% HDI of the parameter estimate |numeric   |
|`.upper_0.8`    |Value of the upper 80% HDI of the parameter estimate |numeric   |
|`.upper_0.5`    |Value of the upper 50% HDI of the parameter estimate |numeric   |
|`sub_component` |Relabeled x variable                                 |character |

 <br><br> 

#### shapley_values_boot                                                                                  


- **filename(s): shapley_values_measure_retest_boot.csv AND  shapley_values_intercor_boot.csv**


- file description: Variance Decomposition output for bootstrapped samples including the R2 values of including and excluding specific predictors


- location: analysis/output/temp_stability/ AND analysis/output/covergent_val/


|column            |description                                                 |type      |
|:-----------------|:-----------------------------------------------------------|:---------|
|`r2_increment`    |Difference between of r2_with and r2_without                |numeric   |
|`r2_with`         |Value of R2 by including the predictor of interest          |numeric   |
|`r2_without`      |Value of R2 by excluding the predictor of interest          |numeric   |
|`r2adj_increment` |Difference between of r2adj_with and r2adj_without          |numeric   |
|`r2adj_with`      |Value of adjusted R2 by including the predictor of interest |numeric   |
|`r2adj_without`   |Value of adjusted R2 by excluding the predictor of interest |numeric   |
|`x`               |Predictor of interest (e.g., age, domain)                   |character |
|`boot_num`        |Bootstrapped sample number                                  |numeric   |
|`n_reg_with`      |Number of predictors in the model                           |numeric   |
|`supple`          |List of predictors in the model                             |character |

 <br><br> 

#### shapley_values                                                                                       


- **filename(s): shapley_values_measure_retest.csv and shapley_values_intercor.csv**


- file description: Variance Decomposition output for the dataset including the R2 values of including and excluding specific predictors


- location: analysis/output/temp_stability/ and analysis/output/convergent_val/


|column            |description                                                 |type      |
|:-----------------|:-----------------------------------------------------------|:---------|
|`row_id`          |Row number                                                  |numeric   |
|`r2_increment`    |Difference between of r2_with and r2_without                |numeric   |
|`r2_with`         |Value of R2 by including the predictor of interest          |numeric   |
|`r2_without`      |Value of R2 by excluding the predictor of interest          |numeric   |
|`r2adj_increment` |Difference between of r2adj_with and r2adj_without          |numeric   |
|`r2adj_with`      |Value of adjusted R2 by including the predictor of interest |numeric   |
|`r2adj_without`   |Value of adjusted R2 by excluding the predictor of interest |numeric   |
|`x`               |Predictor of interest (e.g., age, domain)                   |character |
|`n_reg_with`      |Number of predictors in the model                           |numeric   |
|`supple`          |List of predictors in the model                             |character |

 <br><br> 

#### shapley_values_check                                                                                 


- **filename(s): shapley_values_check.csv**


- file description: Checking for issues of singularity in the variance decomposition analysis


- location: analysis/output/temp_stability/ and analysis/output/convergent_val/


|column  |description                         |type    |
|:-------|:-----------------------------------|:-------|
|`check` |Issue of singularity in regression? |logical |

 <br><br> 

#### summary_shapley_values                                                                               


- **filename(s): summary_shapley_values_retest.csv AND summary_shapley_values_intercor.csv**


- file description: Summarised Variance Decomposition output for plotting


- location: analysis/output/temp_stability/ AND  analysis/output/convergent_val/


|column             |description                                                                               |type      |
|:------------------|:-----------------------------------------------------------------------------------------|:---------|
|`x`                |Predictor of interest                                                                     |character |
|`measure_category` |Name of measure category (Behaviour, Frequency, Propensity, Omnibus)                      |character |
|`m`                |Shapley Value (i.e., weighted adjusted R2 increment)                                      |numeric   |
|`x_lbl`            |Relabeled predictor name for plotting                                                     |character |
|`categ_lbl`        |Category/Family of predictors the predictor belongs to (i.e., panel, respondent, measure) |character |

 <br><br> 

#### summary_shapley_values_boot                                                                          


- **filename(s): summary_shapley_values_retest_boot.csv AND summary_shapley_values_intercor_boot.csv**


- file description: Summarised Variance Decomposition (boostrapped) output for plotting


- location: analysis/output/temp_stability/ AND  analysis/output/convergent_val/


|column             |description                                                                                                   |type      |
|:------------------|:-------------------------------------------------------------------------------------------------------------|:---------|
|`x`                |Predictor of interest                                                                                         |character |
|`measure_category` |Name of measure category (Behaviour, Frequency, Propensity, Omnibus)                                          |character |
|`m`                |Overall mean of Shapley Values (i.e., weighted adjusted R2 increment) accoss all boostrapped samples          |numeric   |
|`.point`           |Mean                                                                                                          |character |
|`.interval`        |Quantiles                                                                                                     |character |
|`.lower_0.5`       |Lower 50th quantile of  Shapley Values (i.e., weighted adjusted R2 increment)  across all boostrapped samples |numeric   |
|`.lower_0.8`       |Lower 80th quantile of  Shapley Values (i.e., weighted adjusted R2 increment)  across all boostrapped samples |numeric   |
|`.lower_0.95`      |Lower 95th quantile of  Shapley Values (i.e., weighted adjusted R2 increment)  across all boostrapped samples |numeric   |
|`.upper_0.5`       |Upper 50th quantile of  Shapley Values (i.e., weighted adjusted R2 increment)  across all boostrapped samples |numeric   |
|`.upper_0.8`       |Upper 80th quantile of  Shapley Values (i.e., weighted adjusted R2 increment)  across all boostrapped samples |numeric   |
|`.upper_0.95`      |Upper 95th quantile of  Shapley Values (i.e., weighted adjusted R2 increment)  across all boostrapped samples |numeric   |
|`x_lbl`            |Relabeled predictor name for plotting                                                                         |character |
|`categ_lbl`        |Category/Family of predictors the predictor belongs to (i.e., panel, respondent, measure)                     |character |

 <br><br> 

#### cor_mat_convergent                                                                                   


- **filename(s): cor_mat_convergent_.csv**


- file description: Convergent Validity data for plotting


- location: analysis/output/convergent_val/


|column           |description                                          |type      |
|:----------------|:----------------------------------------------------|:---------|
|`param`          |Name of model regression from the regression         |character |
|`estimate`       |Meta-Analytic estimate for the intercorrelation      |numeric   |
|`.lower`         |Lower 95% HDI of estimate                            |numeric   |
|`.upper`         |Upper 95% HDI of estimate                            |numeric   |
|`.width`         |95                                                   |numeric   |
|`.point`         |mean                                                 |character |
|`.interval`      |hdci                                                 |character |
|`meas_pair_id`   |ID of measure pairs                                  |numeric   |
|`meas_pair_lbl`  |Label of measure pairs                               |character |
|`x`              |x-axis label/text for plotting                       |character |
|`y`              |y-axis label/text for plotting                       |character |
|`n_cor`          |Number of "raw" correlations                         |numeric   |
|`n_wcor`         |Number of aggregated correlations that were analysed |numeric   |
|`pooled_est_lbl` |Label of pooled estimate for plotting                |character |
|`cred_int_lbl`   |Label of upper and lower HDCI for plotting           |character |
|`k_lbl`          |Label of n_wcor for plotting                         |character |
|`lbl_color`      |Color of the text label for the plotting             |character |

 <br><br> 

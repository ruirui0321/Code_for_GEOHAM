Code Repository for Benefits for non-Article 5 countries under the Montreal Protocol from supporting the phase-out of ozone-depleting substances in Article 5 countries
This repository contains all R scripts used in the study: Benefits for non-Article 5 countries under the Montreal Protocol from supporting the phase-out of ozone-depleting substances in Article 5 countries. The scripts are organized according to the general workflow of the analysis, from data preprocessing to final visualization.

Code Structure and Workflow
The R scripts follow the sequential order below:

calculate the ozone depletion.R
Estimates ozone depletion based on modeled scenarios or input data.

calculate the population.R
Processes population data, including age-group stratification and temporal harmonization.

compute the radiation.R
Computes surface ultraviolet (UV) radiation levels under different ozone conditions.

Read and integrate the radiation from a single file.R
Reads and aggregates radiation data from individual output files for downstream use.

drawing the distribution of radiation and sum the TUV results.R
Visualizes the spatial distribution of UV radiation and summarizes results from the TUV radiative transfer model.

calculate the b.R
Estimates the exposure-response coefficient (b).

calculate Y in any special year.R
Calculates health-related outcome variables (Y) for specific target years.

calculate the cost.R
Estimates the associated economic costs based on avoided health impacts.

plot the results.R
Generates figures summarizing the main findings.

Citation
If you use this code, please cite the following publication:
Benefits for non-Article 5 countries under the Montreal Protocol from supporting the phase-out of ozone-depleting substances in Article 5 countries

Contact
For questions or collaboration inquiries, please contact:
Mingrui Ji
Zhejiang University, China
mingruiji@zju.edu.cn

# ReportingBias

This repository contains the source codes 
- to estimate temporal and regional trends in reported GLOFs, 
- to estimate GLOF trends from the predictors 'annual air temperatures' and 'research activity', and
- to hind- and forecast GLOF counts, based on these two predictors.

The codes are written in the statistical programming language R (https://www.r-project.org/) and run
using the Graphical User Interface RStudio (https://rstudio.com) from a MS Windows 10 OS. 
Please install both softwares on your machine.
The comments within the scripts provide further details on model dependencies
and usage of functions. 


## Scripts

### Name.R

Bayesian piecewise regression model to learn peak discharge Qp from eta, the product of flood volume and the breach rate. Written in R.

## Input data

### regional_glof_stats.rds

R-Data object (a list with 8 entries) containing regional annual statistics of GLOF occurrences, temperatures, and research activity.
Description of the column names:
- 'year': Year;
- 'freq': Total number of reported GLOFs per year, including GLOFs from volcanic eruptions;
- 'moraine': Number of moraine-dam failures per year;
- 'ice': Number of ice-dam failures per year;
- 'other': Number of GLOFs from other (bedrock, water pockets, supraglacial) or unknown sources;
- 'volc': Number of GLOFs from subglacial lakes beneath ice-covered volcanoes;
- 'mb_meas'
- 'front_meas'
- 'dch_meas'
- 'all_meas': Annual sum of mb_meas, front_meas, and dch_meas;
- 'mb_and_front': Annual sum of mb_meas and front_meas;
- 'region': Name of the study region;
- 'year_scale': Standardised years (zero mean and unit standard deviation)
- 'temp_mean': Mean annual air temperature extracted from the CRU TS 4.05 dataset (https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.05/) from all lakes that produced at least one GLOF in a given region;
- 'temp_q25': 25th percentile of annual air temperatures in a given region;
- 'temp_q75': 75th percentile of annual air temperatures in a given region;
- 'pre_sum': total amount of precipitation in a given region.

## References

Veh, G., Korup, O. & A. Walz: Hazard from Himalayan glacier lake outburst floods. PNAS *(accepted)*.

## Contact

Georg Veh

Working group on natural hazards
University of Potsdam
georg.veh@uni-potsdam.de

https://www.uni-potsdam.de/de/umwelt/forschung/ag-naturgefahren.html

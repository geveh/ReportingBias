# ReportingBias

This repository contains the source codes 
- to estimate temporal and regional trends in reported GLOFs, 
- to estimate GLOF trends from the predictors 'annual air temperatures' and 'research activity', and
- to hind- and forecast GLOF counts, based on these two predictors.

The codes are written in the statistical programming language R (https://www.r-project.org/) and run
using the Graphical User Interface RStudio (https://rstudio.com) from a MS Windows 10 OS. 
Please install both softwares on your machine.

The R codes depend on a number of packages. Please install those before running the codes. The comments within the scripts provide further 
details on model dependencies and usage of functions. 


## Scripts

### Preprocessing.R

Script to obtain annual statistics of
- GLOF counts by dam type;
- glaciological research activity;
- temperature; and
- precipitation
from the original OpenOffice spreadsheet 'Global_GLOF_database_2021_06_09.ods', which is available in the Section 'Input data'.
The scripts produces the R-Data object 'regional_glof_stats.rds', which is available in the same folder.

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
- 'mb_meas': Annual number of glacier surveys measuring in-situ mass balances from the WGMS database;
- 'front_meas': Annual number of glacier surveys measuring in-situ front variations;
- 'dch_meas': Annual number of glacier surveys measuring geodetic mass balances (includes also remote sensing studies);
- 'all_meas': Annual sum of mb_meas, front_meas, and dch_meas;
- 'mb_and_front': Annual sum of mb_meas and front_meas;
- 'region': Name of the study region;
- 'year_scale': Standardised years (zero mean and unit standard deviation);
- 'temp_mean': Mean annual air temperature extracted from the CRU TS 4.05 dataset from all lakes that produced at least one GLOF in a given region;
- 'temp_q25': 25th percentile of annual air temperatures in a given region;
- 'temp_q75': 75th percentile of annual air temperatures in a given region;
- 'pre_sum': total amount of precipitation in a given region.

## References

Veh, G., Lützow, N., Kharlamova; V., Petrakov, D., Hugonnet, R. & Korup, O.: Reporting bias in historic glacier lake outburst floods. (in preparation)

## See also

https://glofs.geoecology.uni-potsdam.de

## Contact

Georg Veh

Working group on natural hazards

University of Potsdam

georg.veh@uni-potsdam.de

https://www.uni-potsdam.de/de/umwelt/forschung/ag-naturgefahren.html

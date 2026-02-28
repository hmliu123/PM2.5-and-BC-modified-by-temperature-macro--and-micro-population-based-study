# Example Codes for "High Temperature Amplifies Particulate Air Pollution-related Coronary Heart Disease Risks: A Study Combining Macro- and Micro-Population Evidence"

## Overview
Example codes and test data for the main analysis of the paper "High Temperature Amplifies Particulate Air Pollution-related Coronary Heart Disease Risks: A Study Combining Macro- and Micro-Population Evidence". Macro-population level study was a nationwide time-series study based on the health insurance data from two basic medical insurance datasets. For the reasons that the health insurance data is regulated by governmental policies and cannot be made available to the public due to privacy reasons. Thus, we generate a fake dataset as an example, the results are not consistent with the main findings of the paper. For the micro-population level study, a multi-center panel study, an example dataset was also generate due to the privacy reason.

## Software details
R Software, version 4.3.0

## Instructions to use data
1."Time_serie_test_data.csv" is the fake dataset contains hospital admissions for total major cardiovascular events, daily mean temperautre and ambient air polllution at lag01 from 5 cities during 2017-01-01-2017-12-31.
2."Example_panel_data.csv" is the fake dataset contains repeated personal monitoring data of both black carbon and all lead ST-segment depression events recorded at 5-minute interval.

## Instructions to use code
Necessary packages are provided in the codes, please first install the packages.
The shared codes are used to generate the main analysis of the paper, including the modification by temperature on the associations between short-term exposure to black carbon and hospital admissions in both time-series study and panel study, and the code for calculating attributable number and fracion of black carbon exposure in each temperature stratum.
The codes are generated to estimate the hosptial admission risks for coronary heart disease and ST-segment depression event associated with short-term exposure to BC in different temperature stratum. The same code can be used (by changing the dataset of other cause-specific hospital admissions and other cardiac health indicators) to develop the results for other outcomes.

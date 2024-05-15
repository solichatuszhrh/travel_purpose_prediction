# About
- This study aims to predict trip purpose prediction using GPS data from a smartphone. Innovation in smart surveys can reduce the burden on respondents in filling out daily travel diaries of each stop and track.
- The GPS data was collected by Statistics Netherlands (CBS), therefore, the data is not available publicly without permission. GPS data was linked to administrative data (sociodemographic variables) from CBS.
- Information from OSM is then combined with the GPS and administrative data.
- To reproduce this study, one can run the [R script](./r_annotated_script.R) in RStudio. However, all data files cannot be included in this archive.  
- Information from OSM can be gathered using [Python script](./python_script.py) for each province using a specific radius for the bounding box. Input data to run this script is also not publicly available because it contains a person's location. 


# Requirements
All analysis was conducted in `RStudio 2023.12.1+402` with `R 4.4.0`. Packages used during the analysis are:
- `gtsummary 1.7.2` 
- `dplyr 1.1.0`
- `tidyverse 2.0.0`
- `stringr 1.5.0`
- `geosphere 1.5-18`
- `lubridate 1.9.2`
- `ggplot2 3.4.1`
- `scales 1.2.1`
- `data.table 1.14.8`
- `caret 6.0-94`
- `randomForest 4.7-1.1`
- `xgboost 1.7.7.1`
- `plyr 1.8.8`
- `e1071 1.7-13`
- `naivebayes 1.0.0`

Libraries used in `Python 3.7.0` are:
- `pandas 2.2.2`
- `pyrosm 0.6.2`
- `geopandas 0.14.4`
- `shapely 2.0.4`
- `multiprocessing 2.6.2.1`


# Data Availability
- The data is **available in a secure environment**.
- Researchers who want to conduct the same study should contact a methodologist (from CBS) to get the data.
- Researchers must work in a secure environment by going to the CBS office or working as interns. To get the data, researchers can contact methodologist from CBS:
  -	Yvonne Gootzen (y.a.p.m.gootzen@cbs.nl)
  -	Jonas Klingwort (j.klingwort@cbs.nl)


# Output
Output files **cannot be included** in the archive because of **disclosure problems** (contains individual information or groups of less than 5 persons).


# Ethics/Privacy/Concern
This study is **approved** by the **FETC** with approval number **23-1774**.  
**CBS manages informed consent and methods of anonymization**. To get more information about this, researchers should contact:  
-	Yvonne Gootzen (y.a.p.m.gootzen@cbs.nl)
-	Jonas Klingwort (j.klingwort@cbs.nl)
  
The **security of personal data is guaranteed by CBS** (which has some ISO certifications of security)


# Permission and Access
The archive can be accessed in the [author’s personal GitHub](https://github.com/solichatuszhrh/travel_purpose_prediction).  
However, it contains **only the scripts**, therefore, it is not completely reproducible. The script is openly available for an unlimited time.  
In addition, the data can be obtained by contacting one of CBS's employees. The result cannot also be included because of disclosure problems.  
Author’s contact information:  
- Solichatus Zahroh (s.zahroh@uu.nl)
  
CBS’s employees:
-	Yvonne Gootzen (y.a.p.m.gootzen@cbs.nl)
-	Jonas Klingwort (j.klingwort@cbs.nl)

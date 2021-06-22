# Topological Data Analysis on Upper Mississippi River data

### Organization (proposed)

* LTRM references: (currently existing) a folder for PDF documents about the LTRM data (namely, QF code) and the research proposal
* LTRM data: (new) folder for the LTRM water data, LTRM veg data, and both meta data docs
  * the cleaned data should be saved to this folder
  * code in the github can be relative to here 
* pool-specific EDA: (currently existing) folder for week 2 EDA of pool
  * rename from  "pools EDA"
  * rename the folders inside pools EDA to the actual pool name (Pool 4 Lower as opposed to AL)?
  * remove the pools data folder (since the LTRM data exists). make sure to change relative paths of code referencing pools data
* water data cleaning: (new) folder for all the work we've been doing in week 3 and week 4
  * water data cleaning.Rmd (rename to initial filtering.Rmd?)
  * Data_collapse.ipynb
  * K_Nearest_Neighbors.ipynb
  * where should Correlations.ipynb and DataInterpolation.ipynb go? maybe make another folder for scratch scripts
  * **note that** cleaned data should be written to the LTRM data folder, NOT this folder!

_your zenodo badge here_

# lafferty-etal_inprep_tbg

**A stylized but rich multi-sector model that serves as a testbed for validating uncertainty quantification methods** (tentative)

David Lafferty<sup>1\*</sup>, Katerina Tang<sup>2</sup>, Vivek Srikrishnan<sup>2</sup>, Ryan Sriver<sup>1</sup>, Klaus Keller<sup>3</sup>, Jonathan Lamontagne<sup>4</sup> 

<sup>1</sup> University of Illinois <br>
<sup>2</sup> Cornell University <br>
<sup>3</sup> Dartmouth College <br>
<sup>4</sup> Tufts University

\* corresponding author: davidcl2@illinois.edu

## Abstract

_Paper abstract goes here._

## Journal reference

_TBD_

## Code reference

This section will be updated with finalized model code.

## Data reference

### Input data

All fully processed input data:

_Input data reference goes here._

Note that *all* input data can be downloaded and pre-processed programatically by following the instructions in the [How to reproduce the experiment: Model construction](#model-construction) section. This section includes a list of all source locations. 

| Data Description | Source | URL | Additonal Instructions to Access | Date Accessed |
| --- | --- | --- | --- | --- |
| 1979-2020 gridded meteorological observations | gridMET (also known/cited as METDATA) | [https://www.climatologylab.org/gridmet.html](https://www.climatologylab.org/gridmet.html) | *Download* tab | 09/14/2022 |
| 1948-2016 gridded meteorological observations | GMFD | [http://hydrology.princeton.edu/data.pgf.php](http://hydrology.princeton.edu/data.pgf.php) | *Download data locally* link | 09/14/2022 |
| Shapefile of US counties and states | US Census Bureau | [https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html](https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html) | *cb_2018_us_county_20m.zip* and *cb_2018_us_state_20m.zip* | 09/14/2022 |
| 1979-2020 US county-level maize yields | USDA National Agricultural Statistics Service (NASS) | [https://quickstats.nass.usda.gov/](https://quickstats.nass.usda.gov/) | *SURVEY - CROPS - FIELD CROPS - CORN - YIELD - CORN, GRAIN - YIELD, MEASURED IN BU/ACRE - COUNTY* | 09/14/2022 |
| 1979-2020 US county-level soybean yields | USDA NASS | [https://quickstats.nass.usda.gov/](https://quickstats.nass.usda.gov/) | *SURVEY - CROPS - FIELD CROPS - SOYBEANS - YIELD - SOYBEANS - YIELD, MEASURED IN BU/ACRE - COUNTY* | 09/14/2022 |
| 1950-2016 US national-level maize prices | USDA NASS | [https://quickstats.nass.usda.gov/](https://quickstats.nass.usda.gov/) | *SURVEY - CROPS - FIELD CROPS - CORN - PRICE RECEIVED - CORN, GRAIN - PRICE RECEIVED, MEASURED IN $/BU - NATIONAL* | 09/14/2022 |
| 1950-2016 US national-level soybean prices | USDA NASS | [https://quickstats.nass.usda.gov/](https://quickstats.nass.usda.gov/) | *SURVEY - CROPS - FIELD CROPS - SOYBEANS - PRICE RECEIVED - SOYBEANS - PRICE RECEIVED, MEASURED IN $/BU - NATIONAL* | 09/14/2022 |
| Producer Price Index by Commodity: Farm Products | St. Louis FRED | [https://fred.stlouisfed.org/series/WPU01](https://fred.stlouisfed.org/series/WPU01) | *DOWNLOAD* button | 09/14/2022 |
| 1997, 2003, 2007, 2012, 2017 US county-level maize irrigared acres | USDA NASS | [https://quickstats.nass.usda.gov/](https://quickstats.nass.usda.gov/) | *CENSUS - CROPS - FIELD CROPS - CORN - AREA HARVESTED - CORN, GRAIN, IRRIGATED - ACRES HARVESTED - STATE* | 09/14/2022 |
| 1997, 2003, 2007, 2012, 2017 US county-level soybean irrigared acres | USDA NASS | [https://quickstats.nass.usda.gov/](https://quickstats.nass.usda.gov/) | *CENSUS - CROPS - FIELD CROPS - SOYBEANS - AREA HARVESTED - SOYBEANS, IRRIGATED - ACRES HARVESTED - STATE* | 09/14/2022 |
| 2013, 2018 US state-level water applied to maize | USDA NASS Irrigation and Water Management Survey | [https://quickstats.nass.usda.gov/](https://quickstats.nass.usda.gov/) | *CENSUS - CROPS - FIELD CROPS - CORN - WATER APPLIED - CORN, GRAIN, IRRIGATED - WATER APPLIED, MEASURED IN ACRE FEET / ACRE - TOTAL - STATE* | 09/14/2022 |
| 2013, 2018 US state-level water applied to soybean | USDA NASS Irrigation and Water Management Survey | [https://quickstats.nass.usda.gov/](https://quickstats.nass.usda.gov/) | *CENSUS - CROPS - FIELD CROPS - SOYBEANS - WATER APPLIED - SOYBEANS, IRRIGATED - WATER APPLIED, MEASURED IN ACRE FEET / ACRE - TOTAL - STATE* | 09/14/2022 |
| 2013, 2018 US state-level irrigation expansion costs | USDA NASS Irrigation and Water Management Survey | [https://quickstats.nass.usda.gov/](https://quickstats.nass.usda.gov/) | *CENSUS - ECONOMICS - IRRIGATION - FACILITIES & EQUIPMENT - STATE* | 09/14/2022 |
| 2013, 2018 US state-level irrigation application costs | USDA NASS Irrigation and Water Management Survey | [https://quickstats.nass.usda.gov/](https://quickstats.nass.usda.gov/) | *CENSUS - ECONOMICS - IRRIGATION - ENERGY - EXPENSE - STATE* | 09/14/2022 |

### Output data

All fully processed input data:

_Output data reference goes here._

Note that all output data can be generated by following the instructions in the [How to reproduce the experiment: Sensitivity analysis](#sensitivity-analysis) section.

## Dependencies

- **Python**: include `environment.yml` file
- **R**: include ``sessionInfo()`` output

## How to reproduce the experiment

### Model construction

All relevant scripts/notebooks for this section are found in `workflow/model_construction/code/`

** 0) Downloading and pre-processing input data **

To skip these steps, download the processed input data from [*Input data reference*], rename the directory to `input_data` and place in `workflow/model_construction/`

0. Acquire an API key for both USDA NASS (https://quickstats.nass.usda.gov/api) and St. Louis FRED (https://fred.stlouisfed.org/). In `00e_usda_data_processing.ipynb` you will need to update the lines `usda_api_key = ...` and `fred_api_key = ...`
1. Run the bash script `00a_gridmet_wget.sh`. This will download all relevant gridMET meteorological data (total size: **~150 GB**)
2. Run the bash script `00b_gmfd_wget.sh`. This will download all relevant GMFD meteorological data (total size: **~50 GB**)
3. Run the bash script `00c_us_shapefiles_wget.sh`. This will download all relevant shapefiles
4. Execute all cells (in order) in `00d_weather_data_processing.ipynb`
5. Execute all cells (in order) in `00e_usda_data_processing.ipynb`. Remember to update the lines `usda_api_key = ...` and `fred_api_key = ...`

** 1) Performing the regressions leading to model empirical equations **

1. 

### Sensitivity analysis

## How to reproduce the figures

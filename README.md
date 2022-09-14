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

Note that *almost* all input data can be downloaded and pre-processed programatically by following the instructions in the [How to reproduce the experiment: Model construction](#model-construction) section. This section will list all input data sources and describe how to generate input data that requires some manual processing. 

| Index | Data Description | Source | URL | Additonal Instructions to Access | Date Accessed |
| --- | --- | --- | --- | --- | --- |
| 1 | 1979-2020 gridded meteorological observations | gridMET (also known/cited as METDATA) | [https://www.climatologylab.org/gridmet.html](https://www.climatologylab.org/gridmet.html) | *Download* tab | 09/14/2022 |
| 2 | Shapefile of US counties and states | US Census Bureau | [https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html](https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html) | *cb_2018_us_county_20m.zip* and *cb_2018_us_state_20m.zip* | 09/14/2022 |
| 3 | 1979-2020 US county-level maize yields | USDA National Agricultural Statistics Service (NASS) | [https://quickstats.nass.usda.gov/](https://quickstats.nass.usda.gov/) | *SURVEY - CROPS - FIELD CROPS - CORN - YIELD - CORN, GRAIN - YIELD, MEASURED IN BU/ACRE - COUNTY* | 09/14/2022 |
| 4 | 1979-2020 US county-level soybean yields | USDA NASS | [https://quickstats.nass.usda.gov/](https://quickstats.nass.usda.gov/) | *SURVEY - CROPS - FIELD CROPS - SOYBEANS - YIELD - SOYBEANS - YIELD, MEASURED IN BU/ACRE - COUNTY* | 09/14/2022 |
| 5 | 1950-2016 US national-level maize prices | USDA NASS | [https://quickstats.nass.usda.gov/](https://quickstats.nass.usda.gov/) | *SURVEY - CROPS - FIELD CROPS - CORN - PRICE RECEIVED - CORN, GRAIN - PRICE RECEIVED, MEASURED IN $/BU - NATIONAL* | 09/14/2022 |
| 6 | 1950-2016 US national-level soybean prices | USDA NASS | [https://quickstats.nass.usda.gov/](https://quickstats.nass.usda.gov/) | *SURVEY - CROPS - FIELD CROPS - SOYBEANS - PRICE RECEIVED - SOYBEANS - PRICE RECEIVED, MEASURED IN $/BU - NATIONAL* | 09/14/2022 |
| 7 | Producer Price Index by Commodity: Farm Products | St. Louis FRED | [https://fred.stlouisfed.org/series/WPU01](https://fred.stlouisfed.org/series/WPU01) | None | 09/14/2022 |
| 8 | 1997, 2003, 2007, 2012, 2017 US county-level maize irrigared acres | USDA NASS | [https://quickstats.nass.usda.gov/](https://quickstats.nass.usda.gov/) | *CENSUS - CROPS - FIELD CROPS - CORN - AREA HARVESTED - CORN, GRAIN, IRRIGATED - ACRES HARVESTED - STATE* | 09/14/2022 |
| 9 | 1997, 2003, 2007, 2012, 2017 US county-level soybean irrigared acres | USDA NASS | [https://quickstats.nass.usda.gov/](https://quickstats.nass.usda.gov/) | *CENSUS - CROPS - FIELD CROPS - SOYBEANS - AREA HARVESTED - SOYBEANS, IRRIGATED - ACRES HARVESTED - STATE* | 09/14/2022 |
| 10 | 2013, 2018 US state-level water applied to maize | USDA NASS Irrigation and Water Management Survey | [https://quickstats.nass.usda.gov/](https://quickstats.nass.usda.gov/) | *CENSUS - CROPS - FIELD CROPS - CORN - WATER APPLIED - CORN, GRAIN, IRRIGATED - WATER APPLIED, MEASURED IN ACRE FEET / ACRE - TOTAL - STATE* | 09/14/2022 |
| 11 | 2013, 2018 US state-level water applied to soybean | USDA NASS Irrigation and Water Management Survey | [https://quickstats.nass.usda.gov/](https://quickstats.nass.usda.gov/) | *CENSUS - CROPS - FIELD CROPS - SOYBEANS - WATER APPLIED - SOYBEANS, IRRIGATED - WATER APPLIED, MEASURED IN ACRE FEET / ACRE - TOTAL - STATE* | 09/14/2022 |
| 12 | 2013, 2018 US state-level irrigation expansion costs | USDA NASS Irrigation and Water Management Survey | [https://quickstats.nass.usda.gov/](https://quickstats.nass.usda.gov/) | *CENSUS - ECONOMICS - IRRIGATION - FACILITIES & EQUIPMENT - STATE* | 09/14/2022 |
| 13 | 2013, 2018 US state-level irrigation application costs | USDA NASS Irrigation and Water Management Survey | [https://quickstats.nass.usda.gov/](https://quickstats.nass.usda.gov/) | *CENSUS - ECONOMICS - IRRIGATION - ENERGY - ........ - STATE* | 09/14/2022 |

n. List steps to download and pre-process non-machine-readable USDA data

### Output data

All fully processed input data:

_Output data reference goes here._

Note that all output data can be generated by following the instructions in the [How to reproduce the experiment: Sensitivity analysis](#sensitivity-analysis) section.

## Dependencies

- **Python**: include `environment.yml` file
- **R**: include ``sessionInfo()`` output

## How to reproduce the experiment

### Model construction

### Sensitivity analysis

## How to reproduce the figures

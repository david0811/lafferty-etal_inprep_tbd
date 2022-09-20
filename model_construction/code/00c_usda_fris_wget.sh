#!/bin/bash

cd ../input_data

# 2018
wget https://www.nass.usda.gov/Publications/AgCensus/2017/Online_Resources/Farm_and_Ranch_Irrigation_Survey/fris_csv.zip
unzip fris_csv.zip
rm fris_csv.zip
mv fris_p020_t0007.csv usda_water_applied_2018.csv
rm fris_*

# 2013
wget https://agcensus.library.cornell.edu/wp-content/uploads/2012-Farm-and-Ranch-Irrigation-Survey-fris_csv-1.zip
unzip 2012-Farm-and-Ranch-Irrigation-Survey-fris_csv-1.zip
rm 2012-Farm-and-Ranch-Irrigation-Survey-fris_csv-1.zip
mv fris_p020_t0007.csv usda_water_applied_2013.csv
rm fris_*
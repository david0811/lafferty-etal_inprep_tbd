#!/bin/bash

cd ../input_data

# 20m county shapefile
mkdir us_county_shp
cd us_county_shp
wget https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_county_20m.zip
unzip cb_2018_us_county_20m.zip
rm cb_2018_us_county_20m.zip

# 20m state shapefile
mkdir ../us_state_shp
cd ../us_state_shp
wget https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_state_20m.zip
unzip cb_2018_us_state_20m.zip
rm cb_2018_us_state_20m.zip
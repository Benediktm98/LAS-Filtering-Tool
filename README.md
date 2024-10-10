# LAS Filtering Tool for High Resolution DTM Generation #
A high-performance R tool designed to filter ground data from LAS point clouds obtained from under-canopy laser scanning and photogrammetric surveys. Specifically tailored for densely vegetated areas, this tool is built upon the [LidR](https://github.com/r-lidar/lidR) and [TreeLS](https://github.com/tiagodc/TreeLS) packages and leverages chunk processing to efficiently handle large datasets.

## Description ##
This tool aims to optimize the extraction of high-resolution ground data from dense vegetation, allowing for precise Digital Terrain Model (DTM) generation. By integrating advanced filtering techniques and surface refinement processes, it ensures the highest quality ground point data from complex forest environments.

## Main functionalities ##
- Point cloud clipping
- Voxel downsampling
- Point cloud denoising
- Reflectance and deviation filtering (only for TLS/ALS data)
- Cloth simulation filtering
- Tree detection and forest inventory
- Generalized additive modeling for surface refinement
- Point cloud smoothing
- Point cloud rasterization

![picture alt](https://1drv.ms/i/s!Aq_N410_TZ4rhsQex76-n_0UN4tjZw?e=j1Gvxw "Title is optional")

## Input data ##
1. Georeferenced LAS point cloud
2. Shapefile for the study area
   
*Both input datasets must share the same coordinate reference system (CRS).*


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
- Generalized Additive Modeling for surface refinement
- Point cloud smoothing
- Point cloud rasterization

<hr>
<img src="https://github.com/Benediktm98/LAS-Filtering-Tool/blob/main/LAS_Filtering_Tool_GAM.png" alt="GAM processing steps">
Basic processing of the Generalized Additive Modeling (GAM). 1) Dynamically adjusted cutout around detected tree stems. 2) 2-D distance calculation from each point to the center. 3) Segmentation into prediction data (outer ring) and deleted data (inner ring). 4) Modeling based on a smoothing spline.


## Input data ##
1. Georeferenced LAS point cloud
2. Shapefile for the study area
   
*Both input datasets must share the same coordinate reference system (CRS).*


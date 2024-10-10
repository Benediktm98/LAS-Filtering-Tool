# LF<sup>3</sup>T - LAS Forest Floor Filtering Tool

<div style="border:1px solid black; padding:10px; background-color:#fdd; color:red;">
  The manuscript is still in preparation. The tool will become publicly available once the manuscript has been published. 
</div>

LF<sup>3</sup>T is a high-performance R tool designed to filter ground data from LAS point clouds obtained from under-canopy laser scanning and photogrammetric surveys. Specifically tailored for densely vegetated areas, this tool is built upon the [LidR](https://github.com/r-lidar/lidR) and [TreeLS](https://github.com/tiagodc/TreeLS) packages and leverages chunk processing to efficiently handle large datasets.

## Description ##
LF<sup>3</sup>T is designed to enhance the extraction of high-resolution ground data from dense vegetation while refining noisy ground areas around tree stems. It enables the precise generation of Digital Terrain Models (DTMs), even in challenging, densely forested environments. By integrating advanced filtering techniques and surface refinement processes, it ensures high quality ground point data from complex forest environments, suitable for high precision mapping and monitoring of a variety of under-canopy geomorphic processes.

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

## ##
<img src="https://github.com/Benediktm98/LAS-Filtering-Tool/blob/main/LF3T_TreeDetection.png" alt="Tree detection processing steps">
Key processing steps of the tree detection. 1) Point cloud classification into ground and off-ground. 2) Normalization of the point cloud using TIN. 3) Search for circular features using the treeMap() function. 4). Stem point classification. 
<br>
<br>
<img src="https://github.com/Benediktm98/LAS-Filtering-Tool/blob/main/LF3T_GAM.png" alt="GAM processing steps">
Key processing steps of the Generalized Additive Modeling (GAM). 1) Dynamically adjusted cutout around detected tree stems. 2) 2-D distance calculation from each point to the center. 3) Segmentation into prediction data (outer ring) and deleted data (inner ring). 4) Modeling based on a smoothing spline.


## Input data ##
1. Georeferenced LAS point cloud
2. Shapefile for the study area
   
*Both input datasets must share the same coordinate reference system (CRS).*


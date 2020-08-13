# birdwingGPC Repository

This repository contains the XFLR5 files, R analysis code, and morphological measurements necessary to replicate figures in the following publications: 
1. LD Waldrop, Y He, TL Hedrick, JA Rader. 2020. Functional morphology of gliding flight I. Modeling reveals distinct performance landscapes based on soaring strategies. In press at Integrative and Comparative Biology. https://doi.org/10.1093/icb/icaa114  
2. JA Rader, TL Hedrick, Y He, LD Waldrop. Functional morphology of gliding flight II. Morphology follows predictions of gliding performance. In revision at Integrative and Comparative Biology.

## Repository contents

The repository contains the following files:
- Combined_birdwing_gpc_data_cwl.csv: File containing final data for gPC surrogate production. Produced from data_analysis R files.
- XFLR5 files (.xfl) containing the XFLR5 projects for each camber (representing different folders in the repository), plane (representing different aspect ratios), and Reynolds number (representing different analyses within the Wing and Plane Design module). 
- Data files (.csv) containing raw data exported from graphs within the XFLR5 projects. Each folder contains data files for a single camber, each file is named for its corresponding aspect ratio (AR). All Reynolds numbers/speed analyses are columns within each data file. Additionally, in estbirds/ the data file birds_for_surrogates_v3.csv contains raw morphology data for running performance estimates
- R code (.R) files that load and process data files to provide final analysis. data_analysis.R is the main script for this data processing. plotting_gpc_birds.R was for basic visualization and data exploration. 
- M code and mat files in estbirds/ uses surrogate functions (.mat files) to estimate performance based on morphological measurements (birds_for_surrogates_v3.csv). 
- Other assorted files: In the main directory, there are a number of important files: 
  - input_data_681.dat: a dat file that contains 3 columns and 681 rows. Each row is a separate simulation and each column is an input parameter value at which that simulation was run. The columns are: [1] Aspect ratio, [2] camber, and [3] Reynolds number.
  - AS6091_base.dat and AS6091.csv: points constructing 2D base camber used to create the various 2D foils in the study. See text of publication for reference.

## Repository structure

Folders: 
- estbirds: contains data and Matlab code necessary for estimating performance using performance landscape surrogates associated with Publication 2. 
- panelwingGPC: contains XFLR5, data, and R code files used to create gPC surrogates running the full XFLR5 model.
  - Folders "Cam1" through "Cam25": simulations are primarily sorted by wing chordwise camber, with 25 separate cambers represented. Within these files are .xfl files A-E, depending on how many aspect ratios were required for a particular camber. It was necessary to break up these files to keep file sizes within limits required to post them on Github. 
  - Folder "Planes": contains xml files of planes for import into XFLR5 for each aspect ratio (25 separate planes). There is also a sample plane saved. 
 -panelwingGPC_cwl: contains XFLR5, data, and R code files used to create gPC surrogates for only the constant wing-loading version of minimum sinking speed Vz,min.
This folder shares the same structure as panelwingGPC.
 
 ## How to use this repository
 
After cloning this repository, any xfl file can be opened with XFLR5 v6.47, available as a free download here: https://sourceforge.net/projects/xflr5/files/. To reporduce the data set of 681 simulations used to construct the gPC surrogate function, open the R file data_analysis.R, set the working directory to the repository's main folder, and run the code. Running this file will produce all output metrics used in Publications 1 and 2 except Vz,min with constant wing loading. In order to view these data, open and run data_analysis_cwl.R. 

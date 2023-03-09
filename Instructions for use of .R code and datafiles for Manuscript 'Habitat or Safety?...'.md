
# Instructions for using the code and data files in this repo
#### From the manuscript titled *'Habitat or safety? Drivers and management implications of juvenile lemon shark space use in mangrove nursery'*** .

Molly M Kressler
8 March 2023 

---

**This document illustrates the workflow for the analyses in the aforementioned manuscript.** 

There are **7 .R files** which perform the analyses. There are *two support .R files* which include the code to run supplementary analyses, e.g. a sensitivity anlysis for setting a threshold to classify 'ghost' detections. 

The 7 .R files were written in Sublime Text, on a MacBook, using R version 4.2.0, and can be opened in R or R-Studio, or an alternative text editor, e.g. Visual Studio. 

The accompanying **data** for the analyses can be found in the folder '**data**'. For a description of the datasets please see the section in this document called 'Data Sets Described for Manuscript 'Habitat or safety? Drivers and management implications of juvenile lemon shark space use in mangrove nursery'. 

All the analyses can be performed using the six provided datasets if the code flow is followed through in its entirety. 

---

## R Code Files 

#R #files #code #analysis #dataprep 

Manuscript 'Habitat or safety? Drivers and management implications of juvenile lemon shark space use in mangrove nursery'

R code files can be found [https://github.com/MollyKressler/habitatorsafetylemonsharks_ms.git]. 

To run all analyses run the code files in this order: 
1. STEP1detectiondata_KresslerTrevailetal_inprep.R 
	https://github.com/MollyKressler/habitatorsafetylemonsharks_ms/blob/34fa126d62a6ce7eb77a48530e8eb176f9e9ff4a/STEP1detectiondata_KresslerTrevail_inprep.r
2. STEP2detectionranges_buffers_KresslerTrevailetal_inprep.R 
	https://github.com/MollyKressler/habitatorsafetylemonsharks_ms/blob/34fa126d62a6ce7eb77a48530e8eb176f9e9ff4a/STEP2detectionranges_buffers_KresslerTrevailetal_inprep.r
3. STEP3habitathexagongrid_of_studysite_KresslerTrevailetal_inprep.R
	https://github.com/MollyKressler/habitatorsafetylemonsharks_ms/blob/main/STEP3habitathexagongrid_of_studysite_KresslerTrevailetal_inprep.r
4. STEP4pseudodetections_KresslerTrevailetal_inprep.R
	https://github.com/MollyKressler/habitatorsafetylemonsharks_ms/blob/main/STEP4pseudodetections_KresslerTrevailetal_inprep.R
5. STEP5modellingcode_KresslerTrevailetal_inprep.R
	https://github.com/MollyKressler/habitatorsafetylemonsharks_ms/blob/main/STEP5modellingcode_KresslerTrevailetal_inprep.R
6. STEP6evaluate_and_predict_KresslerTrevailetal_inprep.R
	https://github.com/MollyKressler/habitatorsafetylemonsharks_ms/blob/main/STEP6evaluate_and_predict_KresslerTrevailetal_inprep.R
7. STEP7_finalfigures.R
	https://github.com/MollyKressler/habitatorsafetylemonsharks_ms/blob/main/STEP7_finalfigures.R

Additionally, two supplementary R files are provided. 
1. determining_ghosts_hours_thresholds.R
2. justifying7pseudos_KresslerTrevailetal_inprep.R
These files perform analyses not crucial to the final results but nescessary for holistic interpretation of the choices made in data preparation and modelling. 

---

## Data Sets 

Manuscript 'Habitat or safety? Drivers and management implications of juvenile lemon shark space use in mangrove nursery'

#data #modelling #analysis

**All analyses and outcomes can be conducted and/or produced from these starting six data sets.**
The data sets will be described in no particular order, for details and attributions please see drop downs. Data sets can be found in the folder 'data' on the manuscript repo. 

- Detection data
	- Acoustic telemetry data 
	- 2019 - 2020
	- 2019 and part of 2020 provided by Evan Byrnes
	- part of 2020 provided by Matthew Smukall and Clemency White
- Habitat data, collected by Drone Adventures in partnerhsip with Save Our Seas Foundation and the Bimini Biological Field Station Foundation: 
	- provded as a shapefile, ESRI, by Matthew Smukall
	- open using the 'sf' package in R
	- Habitat is classified in vectors
- Center point of the cental mangrove forest int he North Bimini estuary
	- provided as a shapefile, KML, produced by Molly Kressler using Google Earth
	- open using the 'sf' package in R
- Bounding box which extends beyodn the habitat data coverage, 'bigger box'
	- provided as a shapefile, ESRI, produced by Molly Kressler using Google Earth
	- open using the 'sf' package in R
- Tidal data 
	- downloaded from the USA National Oceanographic and Atmospheric Association public website, by Molly Kressler
	- collated into years, 2019 & 2020
- Bimini land countours
	-  provided as a shapefile, ESRI, by Matthew Smukall
	- open using the 'sf' package in R


---

### Any issues? 

Please contact the corresponding author on the manuscript, Molly M Kressler at m.kressler@exeter.ac.uk. 


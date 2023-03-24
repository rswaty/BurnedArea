---
  title: "Compare Fire Datasets"
author: "Kori and Randy"
date: "2023-02-24"
output: html_document
---
  
  
  ## Analysis steps
  
  
  1. Read in datasets from LF and Hawbaker for each year by looping (?) then stack (?)
* assume all LF data in one directory; HB all in one directory
* make a list for LF and one for HB of the files 
* need to add year to each grid for both LF and HB
* this table would be 'long' with a column named something like "dataset" with values being 'HB' and 'LF', also need a 'year' field

*Need to learn about raster stacks and how to work with them*
  * This would be instead of (?) doing a combine of the LF and HB data per year separately.
* Assuming working in Git so will need a Git Ignore
* Process 3 years to try, including 2019 (Kori has this in Arc to compare) plus 2018 and 2020; maybe add an old year (as naming changes...do this to learn at some point) WTF
* Also add Karen's data to this
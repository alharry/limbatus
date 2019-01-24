# Life history of the common blacktip shark, *Carcharhinus limbatus*, from central eastern Australia and comparative demography of a cryptic shark complex

## Alastair V Harry, Paul A Butcher, William G Macbeth, Jess AT Morgan, Stephen M Taylor & Pascal T Geraghty

Published scientific manuscript: 

Harry AV, Butcher PA, Macbeth WG, Morgan JAT, Taylor SM & Geraghty PT (2019) Life history of the common blacktip shark, Carcharhinus limbatus, from central eastern Australia and comparative demography of a cryptic shark complex *Marine and Freshwater Research* <https://doi.org/10.1071/MF18141>

![](https://www.publish.csiro.au/images/journals/banners/ban_mf_1.png)

To reproduce analysis run `source("run_all.R")`. 

Note: Dummy lats and longs have been provided in the data file. For a password to the file with the correct lats and longs please email alastair.harry@gmail.com

Repository structure:
```
|--data/
  |--age_mat
  |--demog
  |--len_age
  |--len_clasper
  |--len_mat
  |--len_weight
|--analysis/
  |--setup.R
  |--query.R
  |--functions.R
  |--analysis.R
    |--age_mat.R
    |--demography.R
    |--demography2.R
    |--len_age.R
      |--len_age.cpp
    |--len_clasper.R
    |--len_mat.R
    |--len_weight.R
  |--load_results.R
|--run_all.R
|--rmds/
  |--Manuscript.Rmd
  |--Tables.Rmd
  |--Figures.Rmd
  |--Supplementary material.Rmd
|--reports/
|--references/
```

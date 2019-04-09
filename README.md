# Life history of the common blacktip shark, *Carcharhinus limbatus*, from central eastern Australia and comparative demography of a cryptic shark complex

## Alastair V Harry, Paul A Butcher, William G Macbeth, Jess AT Morgan, Stephen M Taylor & Pascal T Geraghty

Published scientific manuscript: 

Harry AV, Butcher PA, Macbeth WG, Morgan JAT, Taylor SM & Geraghty PT (2019) Life history of the common blacktip shark, Carcharhinus limbatus, from central eastern Australia and comparative demography of a cryptic shark complex *Marine and Freshwater Research* <https://doi.org/10.1071/MF18141>

[![DOI](https://zenodo.org/badge/167084058.svg)](https://zenodo.org/badge/latestdoi/167084058)

![](https://www.publish.csiro.au/images/journals/banners/ban_mf_1.png)

To reproduce analysis run `source("run_all.R")`. 

Note: Dummy lats and longs have been provided in the data file. For a password to the file with the correct lats and longs please email alastair.harry@gmail.com

Repository structure:
```
├── LICENSE
├── README.md
├── analysis
│   ├── age_mat.R
│   ├── analyses.R
│   ├── demography.R
│   ├── functions.R
│   ├── len_age.R
│   ├── len_age.cpp
│   ├── len_age.dll
│   ├── len_age_abt.R
│   ├── len_clasper.R
│   ├── len_mat.R
│   ├── len_weight.R
│   ├── load_results.R
│   ├── query.R
│   └── setup.R
├── data
│   ├── Data-correct-lats-longs.xlsx
│   ├── Data-dummy-lats-long.xlsx
│   ├── KH021208-8.png
│   ├── age_mat
│   │   ├── am_both.rds
│   │   ├── am_results.rds
│   │   └── anova_am.rds
│   ├── border.dbf
│   ├── border.prj
│   ├── border.shp
│   ├── border.shx
│   ├── bounds.csv
│   ├── data_final_abt.rds
│   ├── data_final_both.rds
│   ├── data_final_btp.rds
│   ├── demog
│   │   ├── demog_summary.rds
│   │   ├── m_r_int.rds
│   │   ├── m_rmax.rds
│   │   └── monte_carlo.rds
│   ├── len_age
│   │   ├── age.rds
│   │   ├── age2.rds
│   │   ├── age_dat.rds
│   │   ├── len0.rds
│   │   ├── len0_plot.rds
│   │   ├── mon_pred.rds
│   │   ├── nursery.rds
│   │   ├── preds.rds
│   │   ├── preds_both.rds
│   │   ├── qld_age_len_mod_f_log.rds
│   │   ├── qld_age_len_mod_f_vb.rds
│   │   ├── qld_age_len_mod_m_log.rds
│   │   ├── qld_age_len_mod_m_vb.rds
│   │   ├── raw_points.rds
│   │   ├── rep.rds
│   │   ├── resids.rds
│   │   └── true_age.rds
│   ├── len_clasper
│   │   ├── clasp_mod.rds
│   │   └── plot_data_both.rds
│   ├── len_mat
│   │   ├── anova_lm.rds
│   │   ├── lm_both.rds
│   │   ├── lm_results.rds
│   │   └── mod_lm.rds
│   ├── len_weight
│   │   ├── anova_lw.rds
│   │   ├── lw_abt.rds
│   │   ├── lw_both.rds
│   │   ├── lw_f.rds
│   │   ├── lw_m.rds
│   │   ├── lw_results.rds
│   │   ├── mod_lw.rds
│   │   └── mod_lw2.rds
│   ├── map
│   │   ├── COM20111216_ELB_region.dbf
│   │   ├── COM20111216_ELB_region.prj
│   │   ├── COM20111216_ELB_region.shp
│   │   └── COM20111216_ELB_region.shx
│   ├── ozdata.rda
│   ├── table1.xlsx
│   └── table2.xlsx
├── limbatus.Rproj
├── packrat
│   ├── init.R
│   ├── packrat.lock
│   └── packrat.opts
├── references
│   ├── blacktip.bib
│   └── international-journal-of-wildland-fire.csl
├── reports
│   ├── figures.pdf
│   ├── figures.tex
│   ├── figures_files
│   │   └── figure-latex
│   │       ├── fig1-1.pdf
│   │       ├── fig2-1.pdf
│   │       ├── fig3-1.pdf
│   │       ├── fig4-1.pdf
│   │       ├── fig5-1.pdf
│   │       └── fig6-1.pdf
│   ├── manuscript.docx
│   ├── manuscript.pdf
│   ├── manuscript.tex
│   ├── supplementary.pdf
│   ├── supplementary.tex
│   ├── supplementary_files
│   │   └── figure-latex
│   │       ├── figS1-1.pdf
│   │       ├── figS3-1.pdf
│   │       └── figS4-1.pdf
│   ├── tables.pdf
│   └── tables.tex
├── rmds
│   ├── figures.Rmd
│   ├── manuscript.Rmd
│   ├── supplementary.Rmd
│   └── tables.Rmd
└── run_all.R
```

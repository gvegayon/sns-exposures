# Code for "Contagion and Interpersonal Influence: Distinguishing Mechanisms of Behavior Change Using Social Network Theory"

This repository contains the data and code used in the paper to process the survey data, and generate the exposure measurements using the [netdiffuseR](https://cran.r-project.org/package=netdiffuseR) R package. The code uses the following R packages:

- [**netdiffuseR**](https://cran.r-project.com/package=netdiffuseR)
- [**igraph**](https://cran.r-project.com/package=igraph)
- [**sna**](https://cran.r-project.com/package=sna)
- [**data.table**](https://cran.r-project.com/package=data.table)
- [**quarto**](https://cran.r-project.com/package=quarto)
- [**texreg**](https://cran.r-project.com/package=texreg)

The code is organized as follows:

- `data/`: contains the raw data files, and the processed data files. Particularly, the file `data/exposures.R`, which generates the file `data/sns_model_data.csv`.

- `models/`: contains the R scripts used to process the data, and generate the exposure measurements. Particularly, the file `models/regressions.qmd` (a [Quarto](https://quarto.org) file) that runs the different regression models shown in the paper.


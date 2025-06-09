# Environmental and societal implications of transitioning to sustainable diets

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15623676.svg)](https://doi.org/10.5281/zenodo.15623676)
[![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

Supplementary material for the journal publication Rodés-Bachs, C., Sampedro, J., Van de Ven, D., Horowitz, R., Pardo, G., and Zhao, X. (2025) Environmental and societal implications of transitioning to sustainable diets.

This repository is released under the Apache v2.0 license; see the LICENSE for details.


## What's in this repo?

This repository contains the figures and analysis code used in the study. It is structured in the following way:

- `R` folder: R scripts.
- `output` folder: GCAM output files.
- `input` folder: data inputs for the analysis and figure creation.
- `figures` folder: folder where the created figures will be stored.

Due to the `output` and `input` folders' size, the corresponding data can be found in XXX Zenodo archive.

## How to run the code?

1. Download the data from [this Zenodo archive](https://doi.org/10.5281/zenodo.13993989) and unpack the files into the corresponding `input` and `output` folder.
2. Clone or download this repository to your local computer.
3. Run the code. If you are familiarized with Docker we recommend you to follow option `a)`, which provides you an R running environment. Otherwise, you can follow option `b)`, which requires Rstudio and to install manually the necessary libraries.

    a) Download [Docker](https://docs.docker.com/get-docker/), open Docker Desktop, and download the following Docker image through your console:
      ```base
      docker pull claudiarodes/implications_sustainable_diets:diets_v2
      ```
      Run the docker image adjusting the full path to the repository folder:
      ```bash
      docker run -v /full_path_to_the_repository_folder/implications_sustainable_diets:/app -it implications_sustainable_diets 
      ```
      Run the `R/paper_analysis.R` script to produce all the figures of the study, both from the main manuscript and the supplementary information and the `R/paper_methodology.R` script to produce the graphics to illustrate the  ensemble design and uncertainty dimensions considered in the study.

    b) Download [RStudio](https://posit.co/products/open-source/rstudio/), open it, and run the `R/paper_analysis.R`script to produce all the figures of the study, both from the main manuscript and the supplementary information and the `R/paper_methodology.R` script in to produce the graphics to illustrate the  ensemble design and uncertainty dimensions considered in the study.


## Funding acknowledgement

<img src="./logo.png" alt="IAM COMPACT logos" width="130" height="40" align="left"/>
This project has received funding from the European Union's Horizon 2020 research and innovation program under grant agreement number 101056306 (IAM COMPACT project).



<br /><br />
For questions, contact claudia.rodes@bc3research.org.

# Structural Topic Modelling for NHS survey data (exploration)

An exploration of methods and R libraries that can support information 
extraction from survey and free text responses.

# Description

This repository holds code for the NHSX Analytics Unit PhD data science internship project on structural topic modelling for NHS survey data. This project focuses on using structural topic modelling (STM) to gain insight from free text responses to NHS surveys and their associated metadata. The code includes an exploration of pre-processing techniques and sentiment analysis tools to support STM and acquire information from the survey responses.

The dataset used in the analysis is provided. It is originally from [Nottinghamshire Healthcare NHS Foundation Trust's CDU Data Science Team](https://github.com/CDU-data-science-team/pxtextmining)


### Project Structure

- The project code is found in the `R` folder of the repository (see Usage below for more information).
- The data used for in this analysis is found in the `data` folder of the repository.
- Exemplar outputs of this analysis is found in the `outputs` folder of the repository.
- The accompanying [report](./reports/report.pdf) is also available in the `reports` folder.


# Installation

## `cran` installation

This repository has been tested using [![R v3.6.1](https://img.shields.io/badge/r-v3.6.1-blue.svg)](https://cran.r-project.org/bin/windows/base/old/3.6.1/)

Launch the `stmnhsx.Rproj` file in a suitable IDE (e.g. RStudio).  

The required packages are stored in `libraries.r`.  Currently `main.r` sources this module and will automatically install any missing packages!

# Running the code
The folder [`main`](./R/main/) contains the core code for the stm analysis and visualisation. The folder [`experiments`](./R/experiments/) contains exploratory code used in additonal experiments in this project. To run the main code:
1. Run `preprocess.R` to load and prepare the data for the STM model.  This includes text-preprocessing of the text (removing punctuation and digits, stemming, tokenisation etc.), sentiment analysis and converting it to an stm data format. 
2. Run `modelSelection.R ` to run the STM on the data and determine the model with the best performance on the data. The number of topics the model is to look for can be determined by the user in the file. This file will series of outputs to evaluate the models. 
3. Run `modelVis.Rmd` to further visualise a model interactively and conduct text search on the labeled data. 

## Running additional experiments
- Run `modelOutput.R` to run the STM on the data.  This file will produce a series of outputs using stm package to visualise the topics and metadata effects. This uses the processed data from `preprocess.R`.
- Run `ngrams.R` to load the raw data and produce data frames of words, bigrams and trigrams sorted by frequency. 

# Roadmap

See the [open issues](https://github.com/nhsx/stm-survey-text/issues) for a list of proposed features (and known issues).

# Contributing

Contributions are what make the open source community such an amazing place to learn, inspire, and create. Any contributions you make are **greatly appreciated**.

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

_See [CONTRIBUTING.md](./CONTRIBUTING.md) for detailed guidance._

# License

Distributed under the MIT License. _See [LICENSE](./LICENSE) for more information._

# Contact

To find out more about the [Analytics Unit](https://www.nhsx.nhs.uk/key-tools-and-info/nhsx-analytics-unit/) visit our [project website](https://nhsx.github.io/AnalyticsUnit/projects.html) or get in touch at [analytics-unit@nhsx.nhs.uk](mailto:analytics-unit@nhsx.nhs.uk).

# Acknowledgements

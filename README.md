# Structural Topic Modelling for NHS survey data
## NHSX Analytics Unit - PhD Data Science Internship Project

### About the Project

[![status: experimental](https://github.com/GIScience/badges/raw/master/status/experimental.svg)](https://github.com/GIScience/badges#experimental)

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


### Built With

[![R v3.6.1](https://img.shields.io/badge/r-v3.6.1-blue.svg)](https://cran.r-project.org/bin/windows/base/old/3.6.1/)
- [quanteda v3.0.0](https://quanteda.io/news/news-3.0.html)
- [vader v0.2.1](https://cran.r-project.org/web/packages/vader/index.html)
- [stm v1.3.6](https://cran.r-project.org/web/packages/stm/index.html)

# Installation

## `cran` installation

Launch the `stmnhsx.Rproj` file in a suitable IDE (e.g. RStudio).  

The required packages are stored in `libraries.R`.  Currently R files in R/main/ source this module and will automatically install any missing packages!

## Installing WordNet

When installing WordNet, rJava is required for installation. 

- To install rJava on Windows, see the blog [here](https://cimentadaj.github.io/blog/2018-05-25-installing-rjava-on-windows-10/installing-rjava-on-windows-10/)
- To install rJava on MacOS, we used homebrew to install java (`brew install java`) and then ran `sudo R CMD javareconf` in the terminal to connect R and Java. 
- To install rJava on MacOS ARM64 (Macbook M1), try the MacOS instructions on the line above. If this fails and you receive an error stating the installation requires x86_64 architecture but arm64 was found, you will need to follow these instructions. 
First find all Java installations on your computer, open Terminal and write 'cd /usr/libexec/'. Then type ' ./java_home -V'. 
if you have multiple java installations, this is more than likely the problem. Try to locate all java installations with x86_64 attached to the name, it should resemble this: '18.0.1 (x86_64)' near the beginning of the line. Make sure to delete all installations with arm64 attached to the name. If you don't have an installation with the x86_64 configuration (x86_64), install one from Oracle java installations, or brew install java. Then type 'sudo R CMD javareconf', and this issue should be fixed. Test it by typing install.packages("rJAva"), then 'library rJava' into R. If there's no error message you've fixed the issue.

To install WordNet, you can download the executable from  can be downloaded from http://wordnetcode.princeton.edu/2.1/WordNet-2.1.exe or use a homebrew formula (`brew install wordnet`). Now run `install.packages(”wordnet”)` in your IDE and then Run the following in R:
- `library(wordnet)`
- `initDict()`
- `Sys.setenv(WNHOME = "C:/Program Files(x86)/WordNet/2.1")` (/usr/local/.../wordnet/2.1 in MacOS)
- if you used brew to install wordnet on your mac, it will more than likely be here.. (/opt/homebrew/Cellar/wordnet/3.1_1)
- `setDict("C:/Program Files(x86)/WordNet/2.1/dict")` (/usr/local/.../wordnet/2.1/dict in MacOS)
- again, if you used brew.....(/opt/homebrew/Cellar/wordnet/3.1_1/dict)
- `getDict()`

# Running the code

The folder [`R/main/`](./R/main/) contains the core code for the stm analysis and visualisation. The folder [`R/experiments/`](./R/experiments/) contains exploratory code used in additonal experiments in this project. To run the main code:

Update `main.R` User Inputs for the specific task and then run.  Suggest running single sections at a time.  The code starts off by loading the data before text-preprocessing of the text (removing punctuation and digits, stemming, tokenisation etc.), sentiment analysis and converting it to an stm data format.  The best STM models are then determined by running a search over the number of topics.  The outputs are then visualised in static and interactive ways.  Finally, the last section of code allows an interactive term search capability.

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

To find out more about the [Analytics Unit](https://www.nhsx.nhs.uk/key-tools-and-info/nhsx-analytics-unit/) or get in touch at [england.tdau@nhs.net](mailto:england.tdau@nhs.net).

# Acknowledgements

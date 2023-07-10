# Structural Topic Modelling for NHS survey data

**This model is not currently suitable for predicting patient non-attendance in a real-world healthcare environment.**

**Note**: *All example data used in this repository is simulated and for illustrative purposes only.  The dataset used in the analysis is provided. It is originally from [Nottinghamshire Healthcare NHS Foundation Trust's CDU Data Science Team](https://github.com/CDU-data-science-team/pxtextmining)*

See code `README` for installation and usage instructions. 

# Overview

A reusable codebase with example data for applying structural topic modelling (STM) to survey data. This technique allows contextual information (e.g. question number) to be included in the topic allocation.  

The codebase includes example preprocessing of data, NGram analysis, sentiment analysis and the actual structural topic modelling.  Additionally, there is a text search function enabled using [WordNet](https://wordnet.princeton.edu/).   

To visualise and interpret the topic models we examined the range of outputs in the [stm R package](http://statistik-jstat.uibk.ac.at/index.php/jss/article/view/v091i02), such as word clouds, plot the estimated effect of the metadata, and print most associated words. [ToLDAvis](https://www.rdocumentation.org/packages/stm/versions/1.3.6/topics/toLDAvis) was used to visualise the topic-word distributions in an interactive pop-out window. This provided an overview of topic quality by looking at topic content and similarity. [stminsights package](https://github.com/cschwem2er/stminsights) was used to produced an interactive dashboard for a detailed inspection of the model and topics.

<p align="center">
    <img src="./assets/stminsights_lowquality.png" alt="STMInsights Screenshot" width="80%"/>
</p>
<br> *Figure 1: Example Screenshot from STM insights*


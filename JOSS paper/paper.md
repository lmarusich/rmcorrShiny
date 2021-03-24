---
title: 'shiny-rmcorr: A web application for repeated measures correlation'
tags:
  - repeated measures correlation
  - statistics
  - multilevel modeling
  - Shiny
  - ??
authors:
  - name: Laura R. Marusich
    orcid: 0000-0002-3524-6110
    affiliation: 1 # (Multiple affiliations must be quoted)
  - name: Jonathan Z. Bakdash
    orcid: 0000-0002-1409-4779
    affiliation: "2, 3"
affiliations:
 - name: U.S. Army Combat Capabilities Development Command Army Research Laboratory South at the University of Texas at Arlington
   index: 1
 - name: U.S. Army Combat Capabilities Development Command Army Research Laboratory South at the University of Texas at Dallas
   index: 2
 - name: Department of Psychology and Special Education, Texas A&M-Commerce
   index: 3
# citation_author: Price-Whelan et. al.
date: 23 March 2021
year: 2021
bibliography: paper.bib
output: rticles::joss_article
csl: apa.csl
journal: JOSS
---

# Summary

The most common techniques for calculating the correlation between two variables are based
on the assumption that each data point of paired measures represents an independent
observation. For example, a study might calculate the correlation between the height and 
weight of randomly-drawn sample of people, where each person contributes a data point
consisting of one height and one weight. However, it is not unusual for studies to use
repeated measures designs, such as a study that collected height and weight from the same 
randomly-drawn sample of people at three different time points. In this example, each 
person contributes three data points of paired measures. These would not be considered 
independent observations. One common solution to this problem is to use aggregated data
by taking an average of the repeated measures data of each person and then correlating 
these averages. Another solution is to calculate the repeated measures correlation 
[@Bakdash2017; @Bland446; @Bland633], which assesses the common intra-individual 
(within-participants) association for paired repeated measures. The repeated measures correlation technique
is conceptually similar to a null multilevel model, with a common slope but varying 
intercept for each individual. Calculating the repeated measures correlation has the 
two-fold benefit of being simpler and more straightforward to implement than a multilevel
model, with the potential for far greater power than the aggregation solution. 

We previously developed the ``rmcorr`` R package [@R-rmcorr] to make this technique
widely available for researchers; it has since also been adapted as a function in the 
Pingouin statistics package [@Vallat2018] for Python. However, the use of both of these
packages requires some facility with statistical programming languages and thus not
easily accessible for all researchers. 

We have created the ``rmcorr-shiny`` application, which provides an intuitive interface for
computing and plotting the repeated measures correlation (see Figure 1 below). The primary
features of ``rmcorr-shiny`` include:

* The ability to import data in different file formats or use one of four included sample 
datasets.
* The display of raw output from ``rmcorr`` as well as formatted output for reporting
results. 
* Multiple options to generate and customize ``rmcorr`` plots (making use of the 
``ggplot2`` package [@R-ggplot2; @ggplot22016] and palettes from the ``RColorBrewer`` 
[@R-RColorBrewer] and ``pals`` [@R-pals] packages).
* Customized R code using the data and options chosen by the user that can be directly
pasted and executed in R to produce the same output as in ``rmcorr-shiny``.
* The ability to download plots (in a variety of file formats) or a .zip file of all output.

Note that many features were borrowed from the Raincloud-shiny app [@raincloudshiny] (not sure the best way to phrase this, or cite)

Screenshot (placeholder until we have a near-final version?)


``rmcorr-shiny`` can be used in a web browser [**here**](https://lmarusich.shinyapps.io/shiny_rmcorr/)
(should we make this into a package too? i saw other people do that. then people can run it locally)
(also, should we change the name of the repo to match the app? hyphen versus underscore)


# Acknowledgements

We acknowledge contributions from 

# References

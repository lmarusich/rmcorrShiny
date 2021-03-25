---
title: 'rmcorrShiny: A web application for repeated measures correlation'
tags:
  - repeated measures correlation
  - statistics
  - multilevel modeling
  - Shiny
  - correlation
  - repeated measures
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
 - name: Department of Psychology and Special Education, Texas A&M--Commerce
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

The most common techniques for calculating the correlation (e.g., Pearson correlation) between two variables are based on the assumption that each data point of paired measures represents an independent observation. For example, a study might calculate the correlation between the volume of a brain area and a person's age. In this example, each individual contributes a data point consisting of their brain volume and their age. However, it is not uncommon for studies to use
repeated measures designs, such as a study that collecting the area of brain volume and age at two different time points [@raz2005regional]. In this example, each person contributes two (repeated) data points of paired measures. Repeated measures of the same individual are no longer independent observations. Erroneously modeling repeated measures data as independent observations is surprisingly prevalent in published research, even though such results will generally be misleading [@aarts2014solution; @lazic2010problem; @bakdash2020statistical]. A common solution to this problem is to use aggregated data by taking an average of the repeated measures data of each person, making the data independent, and then correlating these averages.  

Instead of aggregation, an alternative solution is to calculate the repeated measures correlation [@Bakdash2017; @Bland446; @Bland633], which assesses the common intra-individual (within-participants) association for paired repeated measures data. The repeated measures correlation technique is conceptually similar to a null multilevel model, with a common slope but varying intercept for each individual. Calculating the repeated measures correlation has multiple potential benefits. 
It is simpler and more straightforward to implement than a multilevel model, with the potential for far greater statistical power than aggregation, and possible insights into patterns among individuals that may otherwise be obscured by aggregation [@Bakdash2017]. 

We previously developed the ``rmcorr`` R package [@R-rmcorr] to make this technique widely available for researchers; it has since also been adapted as a function in the Pingouin statistics package [@Vallat2018] for Python. However, the use of both of these packages requires some facility with  programming languages and thus they not easily accessible to all researchers. 

We have created the ``rmcorrShiny`` application, which provides an intuitive graphical interface for computing and plotting the repeated measures correlation (see Figure 1 below with brain area volume and age from [@raz2005regional]). 

The primary features of ``rmcorrShiny`` include:

* The ability to import data in different file formats or use one of four included sample 
datasets.
* The display of raw output from ``rmcorr`` as well as formatted output for reporting
results. 
* Multiple options to generate and customize ``rmcorr`` plots (making use of the 
``ggplot2`` package [@R-ggplot2; @ggplot22016] and palettes from the ``RColorBrewer`` 
[@R-RColorBrewer] and ``pals`` [@R-pals] packages).
* Customized R code using the data and options chosen by the user that can be directly
pasted and executed in R to produce the same output as in ``rmcorrShiny``.
* The ability to download plots (in a variety of file formats) or a .zip file of all output.

Note that many features in ''rmcorrShiny'' were borrowed or based on modifications of code in the Raincloud-shiny app [@raincloudshiny] (not sure the best way to phrase this, or cite: Jon- How does this sound now?) 

Screenshot (placeholder until we have a near-final version?)


``rmcorrShiny`` can be used in a web browser [**here**](https://lmarusich.shinyapps.io/shiny_rmcorr/)
(should we make this into a package too? i saw other people do that. then people can run it locally)
(also, should we change the name of the repo to match the app? hyphen versus underscore)


# Acknowledgements

The views and conclusions contained in this document are those of the authors and should 
not be interpreted as representing the official policies, either expressed or implied, of 
the U.S. Army Combat Capabilities Development Command Army Research Laboratory or the U.S. 
Government. The U.S. Government is authorized to reproduce and distribute reprints for 
Government purposes notwithstanding any copyright notation. 

We acknowledge contributions from.... 

# References

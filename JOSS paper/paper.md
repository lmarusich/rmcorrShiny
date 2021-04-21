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
date: 21 April 2021
bibliography: paper.bib

---
# Summary

The most common techniques for calculating the correlation between two variables 
(e.g., Pearson correlation coefficient) assume that each pair of data points arises from an 
independent observation. Take, for example, a study that calculates the correlation
between age and the volume of a specific brain region for a sample of people. In this example, each
individual contributes a data point consisting of a brain volume and an age. However, it 
is not uncommon for studies to use repeated measures designs, such as a study that collected 
the brain region volume and age at two different time points [@raz2005regional]. Each 
participant in this study contributed two (repeated) data points of paired measures. Repeated 
measures of the same individual are no longer independent observations and should not be 
analyzed as such. Erroneously modeling repeated measures data as independent observations is 
surprisingly prevalent in published research, even though such results will generally be 
misleading [@aarts2014solution; @lazic2010problem; @bakdash2020statistical]. A common way to 
resolve this problem is to use aggregated data: first taking an average of the repeated measures 
data of each person so that every individual again contributes a single paired data point, and
then calculating the correlation from these averages (between-participants).  

Instead of aggregation, an alternative solution is to calculate the repeated measures
correlation [@Bakdash2017; @Bland446; @Bland633], which assesses the common
intra-individual (within-participants) association for paired repeated measures data. The
repeated measures correlation technique is conceptually similar to a null multilevel model,
with a common (fixed effect) slope but varying (random effect) intercept for each individual. 
Calculating the repeated measures correlation has multiple potential benefits. It is simpler 
and more straightforward to implement than a multilevel model, with the potential for much 
greater statistical power than aggregation. In addition, repeated measures correlation can 
provide insights into patterns within individuals that may be obscured by aggregation [@Bakdash2017]. 

# Statement of need

We previously developed the ``rmcorr`` package [@R-rmcorr] in  ``R`` [@Rcore] to make the repeated measures
correlation technique widely available for researchers; it has since also been adapted as 
a function in the ``Pingouin`` statistics package [@Vallat2018] for Python. However, the use 
of both of these packages requires some facility with programming languages, which may limit
accessibility. 

Here we introduce ``rmcorrShiny``, a ``Shiny`` [@R-shiny-ref] app, which provides an intuitive graphical
interface for computing and plotting the repeated measures correlation (see \autoref{fig:example}). 
The primary features of ``rmcorrShiny`` include:

* The ability to import data in a variety of different file formats or to use one of four included sample 
datasets.
* Options for bootstrapping the confidence interval (CI) for the ``rmcorr`` effect size.
* The display of raw data and the output from ``rmcorr`` as well as formatted output for reporting scientific results. 
* Multiple options to generate and customize ``rmcorr`` plots, making use of the 
``ggplot2`` package [@R-ggplot2; @ggplot22016] and palettes from the ``RColorBrewer`` 
[@R-RColorBrewer] and ``pals`` [@R-pals] packages.
* Customized ``R`` code generated using the data and options chosen by the user that can be directly
pasted and executed in ``R`` to produce the same output as in ``rmcorrShiny``, or as a 
starting point for additional customization in ``R``. 
* The ability to download plots (in multiple file formats) or a .zip file of all output.

Note that many features in ``rmcorrShiny``, including the panel interface, were based on modifications of code 
from the ``Raincloud-shiny`` app [@raincloudshiny].  

![Screenshot of the `` rmcorrShiny`` app, showing the Data--Variables tabs (left side) and
the Plot tab (right side), using sample data from Raz et al. [-@raz2005regional]. Plot:
The x-axis is age and the y-axis is volume of the cerebellar hemisphere. Each participant,
plotted in a different color, contributes two points representing assessments of age and
brain volume at two time periods. The corresponding lines depict the repeated measures
correlation model. The negative slope indicates an age-related decrease in the volume of
this brain area. See Bakdash and Marusich [-@Bakdash2017] for more information about
interpreting repeated measures correlation.  
\label{fig:example}](rmcorr_example_input_plot.jpg)

``rmcorrShiny`` can be used in a web browser [**here**](https://lmarusich.shinyapps.io/shiny_rmcorr/), 
or the package can be installed from Github and run locally in ``R``, using the following commands: 

```r
devtools::install_github("lmarusich/rmcorrShiny")
library(rmcorrShiny)
rmcorrShiny::rmcorrShiny()
```

# Acknowledgements

The views and conclusions contained in this document are those of the authors and should 
not be interpreted as representing the official policies, either expressed or implied, of 
the U.S. Army Combat Capabilities Development Command Army Research Laboratory or the U.S. 
Government. The U.S. Government is authorized to reproduce and distribute reprints for 
Government purposes notwithstanding any copyright notation.

We would like to thank Derek Anderson and Erin Zaroukian for helpful feedback on the `` rmcorrShiny`` app.

# References

# README

## Set Up

The repository is meant as an accompaniment to the paper *Monitoring
Covariance in Multivariate Time Series: Comparing Machine Learning and
Statistical Approaches*.

First install the `mlmewma` package from
https://github.com/dpweix/mlmewma.git. The gated recurrent unit (GRU) is
run using the Tensorflow software in python. This code can be found in
the downloaded `R` package at `mlmewma/inst/python/gru_functions.py`.
Whenever the `path_py` variable is included in a file, it is important
that the absolute file path is provided. Otherwise the python code
cannot be found by the package.

Once the path is determined use `source_python` from the `reticulate`
package to load in all required python functions for the `mlmewma`
package.

Additionally, these files make use of `here` package to avoid needed
absolute pathing for file locations. In order for the `here` package to
work, please open this repository as an R project. Alternatively,
absolute pathing may be added to each file for a specific user.

``` r
library("reticulate")
# Custom package https://github.com/dpweix/mlmewma.git
library("mlmewma")

# Load GRU functions
path_py <- "~/git/mlmewma/inst/python/gru_functions.py"
source_python(path_py)
```

## File Descriptions

<table>
<caption>Description of files used to recreate results in
paper.</caption>
<colgroup>
<col style="width: 11%" />
<col style="width: 88%" />
</colgroup>
<thead>
<tr class="header">
<th>File name</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>1-case-study-data.R</td>
<td>Read/clean data from the case study. Save data as .rds files and
creates the breaks of training data (model), training data (estimate
<em>h</em>), and testing data.</td>
</tr>
<tr class="even">
<td>2-case-study-apply-methods.R</td>
<td>Fit models and apply methods on the data cleaned in file 1. Saves
the resulting model predictions, plotting statistics, and control
limits.</td>
</tr>
<tr class="odd">
<td>3-case-study-figures.R</td>
<td>Create all figures used in the paper. Dependent on having run files
1 and 2.</td>
</tr>
<tr class="even">
<td>4-broken-methods.R</td>
<td>Contains the function <code>gen_sim_study_brk</code>. This function
runs the entire simulation study based on a choice of data structure.
The return values for this function are plotting statistic, run length,
and control limit for each method type.</td>
</tr>
<tr class="odd">
<td>5-arl-study.R</td>
<td>This file imports <code>gen_sim_study_brk</code> and runs a single
simulation with the chosen settings (usually imported from file 6). Then
the resulting run length and control limit are saved in a .rds file in
the results folder.</td>
</tr>
<tr class="even">
<td>6-sim-study.R</td>
<td>This file is used to run the simulation study (neither file 4 or 5
need to be run). All simulation settings are chosen and file 5 is
repeatedly submitted as an Rstudio background job to encourage parallel
running of the simulations.<br />
<br />
Once all files are saved for each data generation structure the ARL
tables can be loaded in as data frames, and converted to LaTeX
tables.</td>
</tr>
<tr class="odd">
<td>7-visualize-study.R</td>
<td>This file provides time series plots for all the different data
generation structures and their respective faults.</td>
</tr>
<tr class="even">
<td>8-visualize-study-3D.R</td>
<td>This file generates 3D scatter plots for all the different data
generation structures and their respective faults.</td>
</tr>
</tbody>
</table>

Description of files used to recreate results in paper.

## How to use this repository

This repository can be used to recreate the results in the paper, or as
a general guide for how to use the `mlmewma` package. This package is
meant for use in fault detection on the covariance of multivariate time
series data, particularly data with non-linear relationships between
variables.

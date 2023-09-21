# README

## Set Up

The repository is meant as an accompaniment to the paper *Monitoring
Covariance in Multivariate Time Series: Comparing Machine Learning and
Statistical Approaches*.

The original excel data file is `bw30-navajo-raw-data.xlsx`. The
`bw30-navajo.csv` and `bw30-navajo.rds` files contain cleaned version of
the data. The `.csv` file can be used for analysis in any programming
language, whereas the `.rds` file includes additional formatting and can
only be used in `R`. Our analysis is based on the `bw30-navajo.rds`
file. Both `bw30-navajo.csv` and `bw30-navajo.rds` are created from
`bw30-navajo-raw-data.xlsx` in the first section of the
`1-case-study-data.R` script.

To run the following scripts, first install the `mlmewma` package from
https://github.com/dpweix/mlmewma.git. The gated recurrent unit (GRU) is
run using the `TensorFlow` software in `Python`. This code can be found
in the downloaded `R` package at `mlmewma/inst/python/gru_functions.py`.
Whenever the `path_py` variable is included in a file, it is important
that the **absolute file path** is provided. Otherwise, the Python code
cannot be found by the package.

Once the path is determined, use `source_python` from the `reticulate`
package to load in all required `Python` functions for the `mlmewma`
package. Note that `TensorFlow` must be installed on the `Python`
instance toward which `reticulate` points.

Additionally, these files make use of the `here` package to avoid
needing absolute pathing for file locations, aside from
`gru_functions.py`. In order for the `here` package to work, please open
this repository as an `R` project. Alternatively, absolute pathing may
be added to each file for a specific user.

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
<colgroup>
<col style="width: 9%" />
<col style="width: 90%" />
</colgroup>
<thead>
<tr class="header">
<th>File name</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>0-test-methods.R</td>
<td>This short script can be used to confirm that the
<code>mlmewma</code> package has been properly installed and the path to
the <code>Python</code> file is correct.</td>
</tr>
<tr class="even">
<td>1-case-study-data.R</td>
<td>This script reads and cleans the data from the case study. The
resulting clean data are saved as .rds files. Additionally, the breaks
separating the training data (model), training data (estimate
<em>h</em>), and testing data are defined here.</td>
</tr>
<tr class="odd">
<td>2-case-study-apply-methods.R</td>
<td>This script fits the models and applies the methods to the data
cleaned in file 1. The resulting model predictions, plotting statistics,
and control limits can be saved.</td>
</tr>
<tr class="even">
<td>3-case-study-figures.R</td>
<td>This script creates all figures concerning the case study used in
the paper. This file depends on the output from scripts 1 and 2.</td>
</tr>
<tr class="odd">
<td>4-broken-methods.R</td>
<td>Do not run this script. This script only contains the function
<code>gen_sim_study_brk</code>. This function runs the entire simulation
study based on a choice of data structure. The return values for this
function are the plotting statistic, run length, and control limit for
each method type.</td>
</tr>
<tr class="even">
<td>5-arl-study.R</td>
<td>Do not run this script. This script imports
<code>gen_sim_study_brk</code> and runs a single simulation with the
chosen settings. The resulting run length and control limit are saved in
a .rds file in the results folder.</td>
</tr>
<tr class="odd">
<td>6-sim-study.R</td>
<td>This script runs the simulation study under conditions chosen at the
top of the file. After the simulation settings are chosen, file 5 is
repeatedly submitted as an Rstudio background job to encourage parallel
running of the simulations.<br />
<br />
Once all files are saved for each data generation structure, the ARL
tables can be loaded in as data frames and converted to LaTeX
tables.</td>
</tr>
<tr class="even">
<td>7-visualize-study.R</td>
<td>This script produces time series plots and run length histograms for
all the different data generation structures and their respective
faults.</td>
</tr>
<tr class="odd">
<td>8-visualize-study-3D.R</td>
<td>This script generates 3D scatter plots for all the different data
generation structures and their respective faults.</td>
</tr>
</tbody>
</table>

## How to use this repository

This repository can be used to recreate the results in the paper or as a
general guide for how to use the `mlmewma` package. This package is
meant for multivariate statistical process monitoring of the covariance
of multivariate time series data, particularly autocorrelated data with
non-linear relationships between variables.

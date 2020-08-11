# Extinction-in-Nearctic-birds
R scripts used in the paper "Extinction in Nearctic birds" from Smith, Gehara and Harvey

Packages you need to install before running the scripts:
devtools, PipeMaster, keras, caret, ggplot2

### Dependencies instalation: 

to install the packages open R and run:

> install.packages("devtools")
>
> library(devtools)
>
> install_github("gehara/PipeMaster@master")
>
> install.packages("ggplot2")
>
> install.packages("keras")
>
> install.packages("caret")

### Running the scripts

To run the analysis clone the repository and run the scripts in the following order:

1) simulate_data.R simulates the data

2) plot_pca.R plots the PCAs of the observed and simulated data

3) model_class.R runs a neural network for model classification

4) param_est.R runs a neural network regression for parameter estimation

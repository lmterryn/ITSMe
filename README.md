
<!-- README.md is generated from README.Rmd. Please edit that file -->
<p align="center">
<img src="man/figures/logo.png" height="200" >
</p>
<!-- badges: start -->
<!-- badges: end -->

## Goal

The goal of the ITSMe R package is to provide easy to use functions to
quickly obtain structural metrics from individual tree point clouds and
their respective TreeQSMs.

## Installation

You can install the development version of ITSMe from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("lmterryn/ITSMe")
```

## Input

The functions are developed for tree point clouds obtained with TLS and
quantitative structure models (QSMs) obtained with TreeQSM
(<https://github.com/InverseTampere/TreeQSM>).

## Individual tree structural metrics

The structural metrics which can be calculated with the ITSMe package
are summarized in the tables below:

structural metric \| function name \| input \|  
——————————\|:————————–:——-\|——————–\|  
diameter at breast height \| dbh_pc, dbh_qsm \| point cloud, QSM \|  
diameter above buttresses \| dab_pc \| point cloud \|  
tree height \| tree_height_pc, tree height_qsm \| point cloud, QSM \|  
projected crown area \| projected_crown_area_pc \| point cloud \|  
crown volume (m<sup>3</sup>) \| volume_crown_pc \| point cloud \|

## Example

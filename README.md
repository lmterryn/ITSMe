
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

structural metrics which can be calculated with the ITSMe package are
summarized in the tables below.

### Basic structural metrics

| structural metric                    |          function name          |            input |
|--------------------------------------|:-------------------------------:|-----------------:|
| diameter at breast height (m)        |         dbh_pc, dbh_qsm         | point cloud, QSM |
| diameter above buttresses (m)        |             dab_pc              |      point cloud |
| tree height (m)                      | tree_height_pc, tree height_qsm | point cloud, QSM |
| projected crown area (m<sup>2</sup>) |     projected_crown_area_pc     |      point cloud |
| crown volume (m<sup>3</sup>)         |         volume_crown_pc         |      point cloud |

### Structural metrics from Terryn et al. (2020)

These are the metrics defined in [Terryn et
al. (2020)](https://doi.org/10.1016/j.isprsjprs.2020.08.009). When the
tree point cloud is provided along with the TreeQSM in the functions,
dbh and tree height values are based on the point clouds rather than the
QSMs.

| structural metric                      |        function name         |              input |
|----------------------------------------|:----------------------------:|-------------------:|
| stem branch angle (degrees)            |    stem_branch_angle_qsm     |                QSM |
| stem branch cluster size               | stem_branch_cluster_size_qsm |                QSM |
| stem branch radius (-/m)               |    stem_branch_radius_qsm    | QSM (+point cloud) |
| stem branch length (-/m)               |    stem_branch_length_qsm    | QSM (+point cloud) |
| stem branch distance (-/m)             |   stem_branch_distance_qsm   | QSM (+point cloud) |
| dbh tree height ratio                  |     dbh_height_ratio_qsm     | QSM (+point cloud) |
| dbh tree volume ratio (m<sup>−2</sup>) |     dbh_volume_ratio_qsm     | QSM (+point cloud) |
| volume below 55                        |     volume_below_55_qsm      | QSM (+point cloud) |

## Example

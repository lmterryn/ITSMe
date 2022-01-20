
<!-- README.md is generated from README.Rmd. Please edit that file -->

# StructMetQSM

<!-- badges: start -->
<!-- badges: end -->

The goal of StructMetQSM is to provide easy to use functions to quickly
obtain structural metrics from treeQSMs obtained with version 2.4.0 of
TreeQSM (<https://github.com/InverseTampere/TreeQSM>).

## Installation

You can install the development version of StructMetQSM from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("lmterryn/StructMetQSM")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(StructMetQSM)
QSM_path <- "C:/Users/lmterryn/example_qsm.mat"
qsm <- read_tree_qsm(QSM_path)
pos <- tree_position_qsm(qsm$cylinder)
sba <- stem_branch_angle_qsm(qsm$branch)
sbcs <- stem_branch_cluster_size_qsm(qsm$cylinder)
sbr_th <- stem_branch_radius_qsm(qsm$cylinder, qsm$treedata, "treeheight")
sbl_dbh <- stem_branch_length_qsm(qsm$branch, qsm$treedata, "dbh")
sbd_dbh <- stem_branch_distance_qsm(qsm$cylinder, qsm$treedata, "dbh")
ratio_dbh_h <- dbh_height_ratio_qsm(qsm$treedata)
ratio_dbh_vol <- dbh_volume_ratio_qsm(qsm$treedata)
vol_55 <- volume_below_55_qsm(qsm$cylinder, qsm$treedata)
ratio_len_vol <- cylinder_length_volume_ratio_qsm(qsm$treedata)
shedding <- shedding_ratio_qsm(qsm$branch,qsm$treedata)
bar <- branch_angle_ratio_qsm(qsm$branch)
rel_vol_ratio <- relative_volume_ratio(qsm$cylinder,qsm$treedata)
```

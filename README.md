
<!-- README.md is generated from README.Rmd. Please edit that file -->
<p align="center">
<img src="man/figures/logo.png" height="200" >
</p>
<!-- badges: start -->

[![R-CMD-check](https://github.com/lmterryn/ITSMe/workflows/R-CMD-check/badge.svg)](https://github.com/lmterryn/ITSMe/actions)
<!-- badges: end -->

## Goal

The goal of the ITSMe R-package is to provide easy to use functions to
quickly obtain structural metrics from individual tree point clouds and
their respective quantitative structure models (QSMs).

## Installation

You can install the development version of ITSMe from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("lmterryn/ITSMe", build_vignettes = TRUE)
```

## Input

The functions are developed for tree point clouds obtained with TLS and
QSMs obtained with [TreeQSM](https://github.com/InverseTampere/TreeQSM).
The functions can, however, also be used on tree point clouds obtained
from MLS and UAV-LS if their point densities are high enough
(e.g. sufficient stem points for dbh/dab estimation).

## Individual tree structural metrics

Structural metrics which can be calculated with the ITSMe package are
summarized in the tables below.

### Basic structural metrics

| structural metric                   |          function name          |            input |
|-------------------------------------|:-------------------------------:|-----------------:|
| diameter at breast height (m)       |         dbh_pc, dbh_qsm         | point cloud, QSM |
| diameter above buttresses (m)       |             dab_pc              |      point cloud |
| tree height (m)                     | tree_height_pc, tree height_qsm | point cloud, QSM |
| projected area (m<sup>2</sup>)      |        projected_area_pc        |      point cloud |
| alpha volume (m<sup>3</sup>)        |         alpha_volume_pc         |      point cloud |
| tree volume (m<sup>3</sup>)         |         tree_volume_qsm         |              QSM |
| trunk volume (m<sup>3</sup>)        |        trunk_volume_qsm         |              QSM |
| total branch volume (m<sup>3</sup>) |     total_branch_volume_qsm     |              QSM |
| total branch length (m)             |     total_branch_length_qsm     |              QSM |
| total cylinder length (m)           |      total_cyl_length_qsm       |              QSM |

### Structural metrics from Terryn et al. (2020)

These are the metrics defined in [Terryn et
al. (2020)](https://doi.org/10.1016/j.isprsjprs.2020.08.009) and were
copied and adapted from [Akerblom et
al. (2017)](https://doi.org/10.1016/j.rse.2016.12.002) except for the
branch angle ratio and the relative volume ratio. Definitions of the
metrics can be found in the help files of the functions and the papers
of Terryn et al. (2020) and Akerblom et al. (2017). Normalisation
according to Terryn et al. (2020) as well as Akerblom et al. (2017) is
possible through the normalisation parameter included in the functions
of the metrics that were adapted by Terryn et al. (2020). If the tree
point cloud is provided along with the TreeQSM in the functions, dbh and
tree height values are based on the point clouds rather than the QSMs.
When the buttress parameter is indicated “TRUE” the diameter above
buttresses instead of the diameter at breast height is used.

| structural metric                             |          function name           |              input |
|-----------------------------------------------|:--------------------------------:|-------------------:|
| stem branch angle (degrees)                   |      stem_branch_angle_qsm       |                QSM |
| stem branch cluster size                      |   stem_branch_cluster_size_qsm   |                QSM |
| stem branch radius (-/m)                      |      stem_branch_radius_qsm      | QSM (+point cloud) |
| stem branch length (-/m)                      |      stem_branch_length_qsm      | QSM (+point cloud) |
| stem branch distance (-/m)                    |     stem_branch_distance_qsm     | QSM (+point cloud) |
| dbh tree height ratio                         |       dbh_height_ratio_qsm       | QSM (+point cloud) |
| dbh tree volume ratio (m<sup>−2</sup>)        |       dbh_volume_ratio_qsm       | QSM (+point cloud) |
| volume below 55                               |       volume_below_55_qsm        |                QSM |
| cylinder length volume ratio (m<sup>−2</sup>) | cylinder_length_volume_ratio_qsm |                QSM |
| shedding ratio                                |        shedding_ratio_qsm        |                QSM |
| branch angle ratio                            |      branch_angle_ratio_qsm      |                QSM |
| relative volume ratio                         |    relative_volume_ratio_qsm     |                QSM |
| crown start height                            |      crown_start_height_qsm      | QSM (+point cloud) |
| crown height                                  |         crown_height_qsm         | QSM (+point cloud) |
| crown evenness                                |        crown_evenness_qsm        |                QSM |
| crown diameter crown height ratio             |  crown_diameterheight_ratio_qsm  | QSM (+point cloud) |
| dbh minimum tree radius ratio                 |     dbh_minradius_ratio_qsm      | QSM (+point cloud) |

## Examples

For complete workflows, have a look at the ITSMe vignette with:

``` r
vignette("ITSMe")
```

Calculating the diameter at breast height versus the diameter above
buttresses of a tree:

``` r
library(ITSMe)
# Read the point cloud file from the Specified path to the tree point cloud file
pc_tree <- read_tree_pc(path = "path/to/point/cloud.txt")
# Use dbh_pc function with default parameters and plot the fit
dbh <- dbh_pc(pc = pc_tree, plot = TRUE)
# Use dab_pc function with default parameters and plot the fit
dab <- dab_pc(pc = pc_tree, plot = TRUE)
```

<p align="center">
<img src="man/figures/dbh_example.jpeg" height="500" >
</p>
<p align="center">
<img src="man/figures/dab_example.jpeg" height="500" >
</p>

If you want to determine the dbh or dab for several tree point clouds
(in the same folder) and visually check the circle fitting, use the
plot_dbh_fit_pcs and plot_dab_fit_pc functions. The plot_dab_fit_pcs
function can also be used to optimise the parameters of the dab_pc
function when default values do not give the desired results: 1. Run
plot_dab_fit_pcs with default values (thresholdbuttress = 0.001,
maxbuttressheight = 7). 2. Check the generated figures in your OUT_path.
3. Increase/decrease maxbuttressheight if buttresses on your trees reach
higher/lower heights. 4. Increase/decrease thresholdbuttress if the
diameter is taken too high/low.

Use plot_crown_classification_pcs to check the crown classification that
is used for the projected_crown_area_pc and crown_volume_pc functions.
The plot_pca_pcs and plot_cv_pcs can help to determine the desired
concavity and alpha values to calculate the projected crown area and
crown volume respectively.

Calculating the stem branch distance of a TreeQSM:

``` r
library(ITSMe)
# Read the TreeQSM file from the Specified path to the TreeQSM file
qsm <- read_tree_qsm(path = "path/to/QSM.mat")
# Use stem_branch_distance_qsm function
sbd <- stem_branch_distance_qsm(cylinder = qsm$cylinder, 
                                treedata = qsm$treedata, normalisation = "dbh")
# Using the point cloud information for more accurate dbh normalisation
pc_tree <- read_tree_pc(path = "path/to/point/cloud.txt")
sbd <- stem_branch_distance_qsm(cylinder = qsm$cylinder, 
                                treedata = qsm$treedata, normalisation = "dbh", 
                                pc = pc_tree, buttress = TRUE)
```

Calculating a summary data.frame with the basic structural metrics (tree
position, dbh, dab, tree height, projected crown area, crown volume)
that can be obtained from individual tree point clouds for all point
clouds in a specific folder:

``` r
library(ITSMe)
#Specify the path to the folder containing multiple tree point cloud files
PCs_path <- 
#Run summary function with default parameter settings
basic_summary <- summary_basic_pointcloud_metrics(PCs_path = "path/to/point/cloud/folder/", 
                                                  extension = ".txt")
```

If you set the plot parameter TRUE and provide an OUT_path, this
function saves a summary figure for each tree:

<p align="center">
<img src="man/figures/summary.jpeg" height="500" >
</p>

Calculating a summary data.frame with the structural metrics defined by
Terryn et al. (2020) for all TreeQSMs in a specific folder:

``` r
library(ITSMe)
#Specify the path to the folder containing the respective tree point cloud files
#If you want dbh/dab and height to be calculated based on tree point clouds:
#Specify the path to the folder containing the respective tree point cloud files
#Run summary function with default parameter settings
qsm_summary <- summary_qsm_metrics(QSMs_path = "path/to/QSM/folder/", 
                                      version = "2.3.0",
                                      PCs_path = "path/to/point/cloud/folder/", 
                                      extension = ".txt")
```

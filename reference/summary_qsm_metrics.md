# Summary structural metrics from QSMs

Returns a summary data.frame containing all the metrics defined by
Terryn et al. (2020). Also contains: X and Y-position, dbh, tree height,
tree volume and trunk volume. When tree point clouds are provided, dbh
and tree height are based on the point cloud instead of the QSM.

## Usage

``` r
summary_qsm_metrics(
  QSMs_path,
  version = "2.4.1",
  multiple = FALSE,
  sbr_normalisation = "treeheight",
  sbl_normalisation = "treeheight",
  sbd_normalisation = "no",
  cylindercutoff = 0,
  PCs_path = NA,
  extension = ".txt",
  buttress = FALSE,
  thresholdR2 = 0.001,
  slice_thickness = 0.06,
  thresholdbuttress = 0.001,
  maxbuttressheight = 7,
  concavity = 4,
  dtm = NA,
  r = 5,
  OUT_path = FALSE
)
```

## Arguments

- QSMs_path:

  A character with the path to the folder that contains the treeQSMs.
  These files have to be of the format xxx_qsm.mat (xxx is the unique
  tree id) or xxx_qsm_0.mat (0 at the end is for example the n-th QSM
  that is made for tree xxx). Multiple QSMs can be present in one QSM
  file, in this case set parameter multiple TRUE. When multiple QSMs are
  present for one tree the mean of the values of the different QSMs is
  taken for that tree as a final value for a certain feature.

- version:

  A character indicating the version of TreeQSM that was used to produce
  the QSMs (Default = "2.4.1"). Other possible versions are "2.4.0",
  "2.0", "2.3.0", "2.3.1" and "2.3.2".

- multiple:

  Logical (default = FALSE), indicates if a single .mat file for one
  tree holds multiple QSMs at once.

- sbr_normalisation:

  Character (default="treeheight"). Normalisation parameter of
  [`stem_branch_radius_qsm`](https://lmterryn.github.io/ITSMe/reference/stem_branch_radius_qsm.md).

- sbl_normalisation:

  Character (default="treeheight"). Normalisation parameter of
  [`stem_branch_length_qsm`](https://lmterryn.github.io/ITSMe/reference/stem_branch_length_qsm.md).

- sbd_normalisation:

  Character (default="no"). Normalisation parameter of
  [`stem_branch_distance_qsm`](https://lmterryn.github.io/ITSMe/reference/stem_branch_distance_qsm.md).

- cylindercutoff:

  This is the cutoff radius in meters for which cylinders are to be
  included in the total tree volume calculation. Default of 0 includes
  all cylinders.

- PCs_path:

  A character with the path to the folder that contains the tree point
  clouds. Default is NA when the point clouds are not available. The
  point clouds are used to determine the DBH, tree height, projected
  crown area and crown volume. The DBH and tree height obtained from the
  tree point clouds are then used for the normalisation of the other
  features. The point cloud files have to be of the format xxx_pc in
  order to link the tree point cloud to its' respective treeQSM.

- extension:

  A character refering to the file extension of the point cloud files
  (default=".txt"). Can be ".txt", ".ply" or ".las". Only relevant if
  the tree point clouds are available.

- buttress:

  Logical (default=FALSE), indicates if the trees have buttresses. Only
  relevant if the tree point clouds are available. Only relevant if the
  tree point clouds are available.

- thresholdR2:

  Numeric value (default=0.001). Parameter of the
  [`dbh_pc`](https://lmterryn.github.io/ITSMe/reference/dbh_pc.md)
  function used to calculate the diameter at breast height. Only
  relevant if the tree point cloud is available and buttress == FALSE.

- slice_thickness:

  Numeric value (default = 0.06). Parameter of the
  [`dbh_pc`](https://lmterryn.github.io/ITSMe/reference/dbh_pc.md) and
  [`dab_pc`](https://lmterryn.github.io/ITSMe/reference/dab_pc.md)
  functions used to calculate the diameter at breast height and above
  buttresses. Only relevant if the tree point cloud is available.

- thresholdbuttress:

  Numeric value (default=0.001). Parameter of the
  [`dab_pc`](https://lmterryn.github.io/ITSMe/reference/dab_pc.md)
  function used to calculate the diameter above buttresses. Only
  relevant if the tree point clouds are available and buttress == TRUE.

- maxbuttressheight:

  Numeric value (default=7). Parameter of the
  [`dab_pc`](https://lmterryn.github.io/ITSMe/reference/dab_pc.md)
  function used to calculate the diameter above buttresses. Only
  relevant if the tree point clouds are available and buttress == TRUE.

- concavity:

  Numeric value (default=4) concavity for the computation of the
  functional diameter using a concave hull based on
  [`concaveman`](https://joelgombin.github.io/concaveman/reference/concaveman.html).
  This concavity value is used in the functions
  [`dbh`](https://lmterryn.github.io/ITSMe/reference/dbh.md),
  [`stem_branch_length_qsm`](https://lmterryn.github.io/ITSMe/reference/stem_branch_length_qsm.md),
  [`stem_branch_distance_qsm`](https://lmterryn.github.io/ITSMe/reference/stem_branch_distance_qsm.md),
  [`dbh_height_ratio_qsm`](https://lmterryn.github.io/ITSMe/reference/dbh_height_ratio_qsm.md),
  [`dbh_volume_ratio_qsm`](https://lmterryn.github.io/ITSMe/reference/dbh_volume_ratio_qsm.md),
  and
  [`dbh_minradius_ratio_qsm`](https://lmterryn.github.io/ITSMe/reference/dbh_minradius_ratio_qsm.md).
  Only relevant if the tree point cloud is available.

- dtm:

  The digital terrain model from
  [`tree_height_pc`](https://lmterryn.github.io/ITSMe/reference/tree_height_pc.md).

- r:

  Numeric value (default=5) r which determines the range taken for the
  dtm from
  [`tree_height_pc`](https://lmterryn.github.io/ITSMe/reference/tree_height_pc.md).
  Only relevant if a dtm is provided.

- OUT_path:

  A character with name of the output file (including the path to the
  folder), where the summary csv file should be saved or logical
  (default=FALSE) in this case no csv file is produced.

## Value

The summary of all metrics from Terryn et al. (2020) as a data.frame.
The summary is saved in a csv file if an output folder is provided. If
multiple QSMs are provided for all trees the mean values and standard
deviations for each tree are also calculated and saved in 2 other csv
files. In this case the function returns a list of the summaries with
the means and standard deviations in the second and third element of the
list respectively.

## Details

Metrics Terryn et al. (2020): stem branch angle (sba,
[`stem_branch_angle_qsm`](https://lmterryn.github.io/ITSMe/reference/stem_branch_angle_qsm.md)),
stem branch cluster size (sbcs,
[`stem_branch_cluster_size_qsm`](https://lmterryn.github.io/ITSMe/reference/stem_branch_cluster_size_qsm.md)),
stem branch radius (sbr,
[`stem_branch_radius_qsm`](https://lmterryn.github.io/ITSMe/reference/stem_branch_radius_qsm.md)),
stem branch length (sbl,
[`stem_branch_length_qsm`](https://lmterryn.github.io/ITSMe/reference/stem_branch_length_qsm.md)),
stem branch distance (sbd,
[`stem_branch_distance_qsm`](https://lmterryn.github.io/ITSMe/reference/stem_branch_distance_qsm.md)),
dbh height ratio (dhr,
[`dbh_height_ratio_qsm`](https://lmterryn.github.io/ITSMe/reference/dbh_height_ratio_qsm.md)),
dbh volume ratio (dvr,
[`dbh_volume_ratio_qsm`](https://lmterryn.github.io/ITSMe/reference/dbh_volume_ratio_qsm.md)),
volume below 55 (vb55,
[`volume_below_55_qsm`](https://lmterryn.github.io/ITSMe/reference/volume_below_55_qsm.md)),
cylinder length volume ratio (clvr,
[`cylinder_length_volume_ratio_qsm`](https://lmterryn.github.io/ITSMe/reference/cylinder_length_volume_ratio_qsm.md)),
shedding ratio (sr,
[`shedding_ratio_qsm`](https://lmterryn.github.io/ITSMe/reference/shedding_ratio_qsm.md)),
branch angle ratio (bar,
[`branch_angle_ratio_qsm`](https://lmterryn.github.io/ITSMe/reference/branch_angle_ratio_qsm.md)),
relative volume ratio (rvr,
[`relative_volume_ratio_qsm`](https://lmterryn.github.io/ITSMe/reference/relative_volume_ratio_qsm.md)),
crown start height (csh,
[`crown_start_height_qsm`](https://lmterryn.github.io/ITSMe/reference/crown_start_height_qsm.md)),
crown height (ch,
[`crown_height_qsm`](https://lmterryn.github.io/ITSMe/reference/crown_height_qsm.md)),
crown evenness (ce,
[`crown_evenness_qsm`](https://lmterryn.github.io/ITSMe/reference/crown_evenness_qsm.md)),
crown diameter height ratio (cdhr,
[`crown_diameterheight_ratio_qsm`](https://lmterryn.github.io/ITSMe/reference/crown_diameterheight_ratio_qsm.md)),
dbh minimum radius ratio (dmr,
[`dbh_minradius_ratio_qsm`](https://lmterryn.github.io/ITSMe/reference/dbh_minradius_ratio_qsm.md)).

## References

Terryn, L., Calders, K., Disney, M., Origo, N., Malhi, Y., Newnham, G.,
... & Verbeeck, H. (2020). Tree species classification using structural
features derived from terrestrial laser scanning. ISPRS Journal of
Photogrammetry and Remote Sensing, 168, 170-181.

## Examples

``` r
if (FALSE) { # \dontrun{
# Calculate the summary with default parameters and export to csv
# recommended for non-buttressed trees
summary <- summary_qsm_metrics(
  QSMs_path = "path/to/folder/QSMs/",
  OUT_path = "path/to/out/folder/"
)
# also using point cloud info
summary <- summary_qsm_metrics(
  QSMs_path = "path/to/folder/QSMs/",
  PCs_path = "path/to/folder/PCs/",
  extension = ".txt",
  OUT_path = "path/to/out/folder/"
)
# Calculate the summary with non-default parameter values
# recommended for buttressed trees
summary <- summary_qsm_metrics(
  QSMs_path = "path/to/folder/QSMs/",
  PCs_path = "path/to/folder/PCs/",
  extension = ".txt", buttress = TRUE,
  OUT_path = "path/to/out/folder/"
)
} # }
```

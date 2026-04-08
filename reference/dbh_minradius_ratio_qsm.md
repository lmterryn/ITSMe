# DBH minimum tree radius ratio TreeQSM

Calculates the ratio between the dbh and the minimum tree radius from a
TreeQSM.

## Usage

``` r
dbh_minradius_ratio_qsm(
  treedata,
  cylinder,
  pc = NA,
  buttress = FALSE,
  thresholdR2 = 0.001,
  slice_thickness = 0.06,
  thresholdbuttress = 0.001,
  maxbuttressheight = 7,
  concavity = 4,
  dtm = NA,
  r = 5
)
```

## Arguments

- treedata:

  Treedata field of a TreeQSM that is returned by
  [`read_tree_qsm`](https://lmterryn.github.io/ITSMe/reference/read_tree_qsm.md).

- cylinder:

  Cylinder field of a TreeQSM that is returned by
  [`read_tree_qsm`](https://lmterryn.github.io/ITSMe/reference/read_tree_qsm.md).

- pc:

  The tree point cloud as a data.frame with columns X,Y,Z. Output of
  [`read_tree_pc`](https://lmterryn.github.io/ITSMe/reference/read_tree_pc.md).
  Default is NA and indicates no tree point cloud is available.

- buttress:

  Logical (default=FALSE), indicates if the trees have buttresses. Only
  relevant if pc is available.

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
  relevant if the tree point cloud is available and buttress == TRUE.

- maxbuttressheight:

  Numeric value (default=7). Parameter of the
  [`dab_pc`](https://lmterryn.github.io/ITSMe/reference/dab_pc.md)
  function used to calculate the diameter at breast height. Only
  relevant if the tree point cloud is available and buttress == TRUE.

- concavity:

  Numeric value (default=4) concavity for the computation of the
  functional diameter using a concave hull based on
  [`concaveman`](https://joelgombin.github.io/concaveman/reference/concaveman.html).
  only relevant when a point cloud is provided.

- dtm:

  The digital terrain model as a data.frame with columns X,Y,Z (default
  = NA). If the digital terrain model is in the same format as a point
  cloud it can also be read with
  [`read_tree_pc`](https://lmterryn.github.io/ITSMe/reference/read_tree_pc.md).
  only relevant when a point cloud is provided.

- r:

  Numeric value (default=5) r which determines the range taken for the
  dtm. Should be at least the resolution of the dtm. Only relevant when
  a dtm is provided.

## Value

The ratio of the dbh and the minimum tree radius.

## Details

This ratio is defined as "Ratio between the DBH and the minimum of the
vertical bin radius estimates" (Akerblom et al., 2017 & Terryn et al.,
2020). The vertical bin radius estimates are calculated with
[`vertical_bin_radii_qsm`](https://lmterryn.github.io/ITSMe/reference/vertical_bin_radii_qsm.md).
DBH is calculated with
[`dbh`](https://lmterryn.github.io/ITSMe/reference/dbh.md).

## References

Akerblom, M., Raumonen, P., Makipaa, R., & Kaasalainen, M. (2017).
Automatic tree species recognition with quantitative structure models.
Remote Sensing of Environment, 191, 1-12.

Terryn, L., Calders, K., Disney, M., Origo, N., Malhi, Y., Newnham, G.,
... & Verbeeck, H. (2020). Tree species classification using structural
features derived from terrestrial laser scanning. ISPRS Journal of
Photogrammetry and Remote Sensing, 168, 170-181.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read tree qsm and calculate the dbh minimum tree radius ratio
qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
dmrr <- dbh_minradius_ratio_qsm(
  treedata = qsm$treedata,
  cylinder = qsm$cylinder
)
# with point cloud data
pc_tree <- read_tree_pc(PC_path = "path/to/point_cloud.txt")
dmrr <- dbh_minradius_ratio_qsm(
  treedata = qsm$treedata,
  cylinder = qsm$cylinder, pc = pc_tree
)
# for a buttressed tree
dmrr <- dbh_minradius_ratio_qsm(
  treedata = qsm$treedata,
  cylinder = qsm$cylinder, pc = pc_tree,
  buttress = TRUE
)
} # }
```

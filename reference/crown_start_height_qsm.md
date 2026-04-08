# Crown start height TreeQSM

Calculates the crown start height from a TreeQSM.

## Usage

``` r
crown_start_height_qsm(treedata, cylinder, pc = NA, dtm = NA, r = 5)
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

The crown start height.

## Details

The crown start height is defined as "The height of the first stem
branch in the tree crown relative to the tree height" (Akerblom et al.,
2017 & Terryn et al., 2020). The tree height is calculated with
[`tree_height`](https://lmterryn.github.io/ITSMe/reference/tree_height.md).
Crown cylinders are determined with
[`crownset_qsm`](https://lmterryn.github.io/ITSMe/reference/crownset_qsm.md).

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
# Read tree qsm and calculate the crown start height
qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
csh <- crown_start_height_qsm(
  treedata = qsm$treedata,
  cylinder = qsm$cylinder
)
# with point cloud data
pc_tree <- read_tree_pc(PC_path = "path/to/point_cloud.txt")
csh <- crown_start_height_qsm(
  treedata = qsm$treedata,
  cylinder = qsm$cylinder, pc = pc_tree
)
} # }
```

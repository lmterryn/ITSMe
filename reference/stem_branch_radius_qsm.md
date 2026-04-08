# Stem branch radius TreeQSM

Calculates the stem branch radius from a TreeQSM.

## Usage

``` r
stem_branch_radius_qsm(
  cylinder,
  treedata,
  normalisation = "treeheight",
  pc = NA,
  dtm = NA,
  r = 5
)
```

## Arguments

- cylinder:

  Cylinder field of a TreeQSM that is returned by
  [`read_tree_qsm`](https://lmterryn.github.io/ITSMe/reference/read_tree_qsm.md).

- treedata:

  Treedata field of a TreeQSM that is returned by
  [`read_tree_qsm`](https://lmterryn.github.io/ITSMe/reference/read_tree_qsm.md).

- normalisation:

  Can be either "treeheight" or "parentcylinder". In case of
  "treeheight" the mean radius of the 10 biggest branches is divided by
  the tree height (Terryn et al., 2020). In case of "parentcylinder" the
  mean is taken of the ratios of the radius of the 10 biggest branches
  and the radius of their parent cylinders (Akerblom et al., 2017). When
  something different than "treeheight" or "parentcylinder" is given, no
  normalisation is done. Default is no normalisation.

- pc:

  The tree point cloud as a data.frame with columns X,Y,Z. Output of
  [`read_tree_pc`](https://lmterryn.github.io/ITSMe/reference/read_tree_pc.md).
  Default is NA and indicates no tree point cloud is available. Only
  relevant if normalisation equals "treeheight".

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

The stem branch radius. Unitless with normalisation, in meters without
normalisation. NaN when there are no stem branches.

## Details

The stem branch radius is defined as "Mean of the 10 largest 1st order
branches measured at the base. Can be normalised by the tree height or
the the stem radius at respective height" (Akerblom et al., 2017 &
Terryn et al., 2020). Tree height is calculated with
[`tree_height`](https://lmterryn.github.io/ITSMe/reference/tree_height.md).

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
# Read tree qsm and calculate the stem branch radius
# from Akerblom et al. (2017)
qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
sbr <- stem_branch_radius_qsm(
  cylinder = qsm$cylinder,
  treedata = qsm$treedata,
  normalisation = "parentcyl"
)
# from Terryn et al. (2020)
sbr <- stem_branch_radius_qsm(
  cylinder = qsm$cylinder,
  treedata = qsm$treedata,
  normalisation = "treeheight"
)
# with point cloud data
pc_tree <- read_tree_pc(PC_path = "path/to/point_cloud.txt")
sbr <- stem_branch_radius_qsm(
  cylinder = qsm$cylinder,
  treedata = qsm$treedata,
  normalisation = "treeheight",
  pc = pc_tree
)
} # }
```

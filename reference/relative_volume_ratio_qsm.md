# Relative volume ratio TreeQSM

Calculates the relative volume ratio from a TreeQSM.

## Usage

``` r
relative_volume_ratio_qsm(cylinder, treedata)
```

## Arguments

- cylinder:

  Cylinder field of a TreeQSM that is returned by
  [`read_tree_qsm`](https://lmterryn.github.io/ITSMe/reference/read_tree_qsm.md).

- treedata:

  Treedata field of a TreeQSM that is returned by
  [`read_tree_qsm`](https://lmterryn.github.io/ITSMe/reference/read_tree_qsm.md).

## Value

The relative volume ratio.

## Details

The relative volume ratio is defined as "Ratio of the percentage volume
within 80 to 90% of the tree height and the percentage volume within 0
to 10% of the tree height" (Terryn et al., 2020).

## References

Terryn, L., Calders, K., Disney, M., Origo, N., Malhi, Y., Newnham, G.,
... & Verbeeck, H. (2020). Tree species classification using structural
features derived from terrestrial laser scanning. ISPRS Journal of
Photogrammetry and Remote Sensing, 168, 170-181.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read tree qsm and calculate the relative volume ratio
qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
rvr <- relative_volume_ratio_qsm(
  cylinder = qsm$cylinder,
  treedata = qsm$treedata
)
} # }
```

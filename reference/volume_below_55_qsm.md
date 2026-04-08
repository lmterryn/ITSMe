# Volume below 55 TreeQSM

Calculates the volume below 55% from a TreeQSM.

## Usage

``` r
volume_below_55_qsm(cylinder, treedata)
```

## Arguments

- cylinder:

  Cylinder field of a TreeQSM that is returned by
  [`read_tree_qsm`](https://lmterryn.github.io/ITSMe/reference/read_tree_qsm.md).

- treedata:

  Treedata field of a TreeQSM that is returned by
  [`read_tree_qsm`](https://lmterryn.github.io/ITSMe/reference/read_tree_qsm.md).

## Value

The volume below 55.

## Details

The volume below 55 is defined as "the relative branch volume below 55%
of tree height" (Akerblom et al., 2017 & Terryn et al., 2020).

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
# Read tree qsm and calculate the volume below 55
qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
vol_55 <- volume_below_55_qsm(
  cylinder = qsm$cylinder,
  treedata = qsm$treedata
)
} # }
```

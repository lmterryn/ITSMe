# Shedding ratio TreeQSM

Calculates the shedding ratio from a TreeQSM.

## Usage

``` r
shedding_ratio_qsm(branch, cylinder, treedata)
```

## Arguments

- branch:

  Branch field of a TreeQSM that is returned by
  [`read_tree_qsm`](https://lmterryn.github.io/ITSMe/reference/read_tree_qsm.md).

- cylinder:

  Cylinder field of a TreeQSM that is returned by
  [`read_tree_qsm`](https://lmterryn.github.io/ITSMe/reference/read_tree_qsm.md).

- treedata:

  Treedata field of a TreeQSM that is returned by
  [`read_tree_qsm`](https://lmterryn.github.io/ITSMe/reference/read_tree_qsm.md).

## Value

The shedding ratio.

## Details

The shedding ratio is defined as "The number of stem branches without
children divided by the number of all branches in the bottom third"
(Akerblom et al., 2017 & Terryn et al., 2020).

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
# Read tree qsm and calculate the shedding ratio
qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
sr <- shedding_ratio_qsm(
  branch = qsm$branch, cylinder = qsm$cylinder,
  treedata = qsm$treedata
)
} # }
```

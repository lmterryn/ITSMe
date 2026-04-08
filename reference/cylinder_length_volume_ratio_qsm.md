# Cylinder length-volume ratio TreeQSM

Calculates the cylinder length volume ratio from a TreeQSM.

## Usage

``` r
cylinder_length_volume_ratio_qsm(treedata)
```

## Arguments

- treedata:

  Treedata field of a TreeQSM that is returned by
  [`read_tree_qsm`](https://lmterryn.github.io/ITSMe/reference/read_tree_qsm.md).

## Value

The cylinder length volume ratio in meters-2.

## Details

The cylinder length volume ratio is defined as "the ratio between total
length and volume of the branch cylinders" (Akerblom et al., 2017 &
Terryn et al., 2020).

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
# Read tree qsm and calculate the cylinder length volume ratio
qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
clvr <- cylinder_length_volume_ratio_qsm(treedata = qsm$treedata)
} # }
```

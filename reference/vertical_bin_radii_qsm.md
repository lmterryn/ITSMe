# Radii vertical bins TreeQSM

Calculates the radii of the three vertical bins fitted to the TreeQSM
cylinders.

## Usage

``` r
vertical_bin_radii_qsm(treedata, cylinder)
```

## Arguments

- treedata:

  Treedata field of a TreeQSM that is returned by
  [`read_tree_qsm`](https://lmterryn.github.io/ITSMe/reference/read_tree_qsm.md).

- cylinder:

  Cylinder field of a TreeQSM that is returned by
  [`read_tree_qsm`](https://lmterryn.github.io/ITSMe/reference/read_tree_qsm.md).

## Value

The radii of the three vertical bins.

## Details

These radii are the radii of the cylinders whose axis are vertical and
goes through the bin centre point, and which contains approximately 90%
of the volume of the branch cylinders in that bin. The tree is divided
into three vertical bins, and the centre point of each bin is defined as
the average of mean points of stem cylinders in the bin. If the bin does
not contain stem cylinders the centre of the previous bin is used
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
# Read tree qsm and calculate the vertical bin radii
qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
radii <- vertical_bin_radii_qsm(
  treedata = qsm$treedata,
  cylinder = qsm$cylinder
)
} # }
```

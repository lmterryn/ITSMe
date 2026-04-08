# Stem branch cluster size TreeQSM

Calculates the stem branch cluster size from a TreeQSM.

## Usage

``` r
stem_branch_cluster_size_qsm(cylinder)
```

## Arguments

- cylinder:

  Cylinder field of a TreeQSM that is returned by
  [`read_tree_qsm`](https://lmterryn.github.io/ITSMe/reference/read_tree_qsm.md).

## Value

The stem branch cluster size. NaN when there are no stem branches.

## Details

The stem branch cluster size is defined as "the average number of 1st
order branches inside a 40cm height interval for 1st order branches.
Each branch can only belong to one interval" (Akerblom et al., 2017 &
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
# Read tree qsm and calculate the stem branch cluster size
qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
sbcs <- stem_branch_cluster_size_qsm(cylinder = qsm$cylinder)
} # }
```

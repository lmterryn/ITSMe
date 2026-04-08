# Stem branch angle TreeQSM

Calculates the stem branch angle from a TreeQSM.

## Usage

``` r
stem_branch_angle_qsm(branch)
```

## Arguments

- branch:

  Branch field of a TreeQSM that is returned by
  [`read_tree_qsm`](https://lmterryn.github.io/ITSMe/reference/read_tree_qsm.md).

## Value

The stem branch angle in degrees. NaN when there are no stem branches.

## Details

The stem branch angle is defined as "the median of the branching angles
of the 1st order branches in degrees. 0 is upwards and 180 downwards
(parallel with the trunk)" (Akerblom et al., 2017 & Terryn et al.,
2020).

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
# Read tree qsm and calculate the stem branch angle
qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
sba <- stem_branch_angle_qsm(branch = qsm$branch)
} # }
```

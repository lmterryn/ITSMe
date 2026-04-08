# Crown evenness TreeQSM

Calculates the crown evenness from a TreeQSM.

## Usage

``` r
crown_evenness_qsm(cylinder)
```

## Arguments

- cylinder:

  Cylinder field of a TreeQSM that is returned by
  [`read_tree_qsm`](https://lmterryn.github.io/ITSMe/reference/read_tree_qsm.md).

## Value

The crown evenness.

## Details

The crown evenness is defined as "The crown cylinders divided into 8
angular bins. Ratio between the minimum heights of the highest and
lowest bin." (Akerblom et al., 2017 & Terryn et al., 2020). Crown
cylinders are determined with
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
# Read tree qsm and calculate the crown evenness
qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
ce <- crown_evenness_qsm(cylinder = qsm$cylinder)
} # }
```

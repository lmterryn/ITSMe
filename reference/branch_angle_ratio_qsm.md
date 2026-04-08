# Branch angle ratio TreeQSM

Calculates the branch angle ratio from a TreeQSM.

## Usage

``` r
branch_angle_ratio_qsm(branch)
```

## Arguments

- branch:

  Branch field of a TreeQSM that is returned by
  [`read_tree_qsm`](https://lmterryn.github.io/ITSMe/reference/read_tree_qsm.md).

## Value

The branch angle ratio. NaN when there are no branches.

## Details

The branch angle ratio is defined as "Ratio of the medians of the
branching angles of the 1st order branches and 2nd order branches"
(Terryn et al., 2020).

## References

Terryn, L., Calders, K., Disney, M., Origo, N., Malhi, Y., Newnham, G.,
... & Verbeeck, H. (2020). Tree species classification using structural
features derived from terrestrial laser scanning. ISPRS Journal of
Photogrammetry and Remote Sensing, 168, 170-181.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read tree qsm and calculate the branch angle ratio
qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
br <- branch_angle_ratio_qsm(branch = qsm$branch)
} # }
```

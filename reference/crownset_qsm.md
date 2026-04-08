# Crownset TreeQSM

Returns the indices of the cylinders belonging to the crown of the
treeQSM.

## Usage

``` r
crownset_qsm(cylinder)
```

## Arguments

- cylinder:

  Cylinder field of a TreeQSM that is returned by
  [`read_tree_qsm`](https://lmterryn.github.io/ITSMe/reference/read_tree_qsm.md).

## Value

An integer containing the indices of the cylinders belonging to the
crownset.

## Details

The crownset is determined based on four steps (that are designed to
exclude dead branches at the bottom of the stem) according to Akerblom
et al. (2017). STEP 1: Initialize the crown set as cylinders that have a
branching order higher than three. If the initial set is empty, the
minimum order is lowered until the set becomes non-empty. STEP 2: As
long as the crown set extends, append the parent cylinders of the crown
set that are not part of the stem. STEP 3: Append to the crown set
cylinders that are not part of the stem but whose start point is higher
than the lowest starting point of crown cylinders connected to the stem.
STEP 4: As long as the crown set extends, append the child cylinders of
the crown set.

## References

Akerblom, M., Raumonen, P., Makipaa, R., & Kaasalainen, M. (2017).
Automatic tree species recognition with quantitative structure models.
Remote Sensing of Environment, 191, 1-12.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read tree qsm and extract the crownset
qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
crown <- crownset_qsm(cylinder = qsm$cylinder)
} # }
```

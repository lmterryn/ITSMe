# Total tree volume TreeQSM

Extracts the total tree volume from the treedata of a TreeQSM.

## Usage

``` r
tree_volume_qsm(treedata, cylinder = NA, cylindercutoff = 0)
```

## Arguments

- treedata:

  Treedata field of a TreeQSM that is returned by
  [`read_tree_qsm`](https://lmterryn.github.io/ITSMe/reference/read_tree_qsm.md).

- cylinder:

  Cylinder field of a TreeQSM that is returned by
  [`read_tree_qsm`](https://lmterryn.github.io/ITSMe/reference/read_tree_qsm.md),
  (default = NA - you do not need cylinder when cylindercutoff == 0).

- cylindercutoff:

  This is the cutoff radius in meters for which cylinders are to be
  included in the volume calculation.Default of 0 includes all
  cylinders.

## Value

The total volume of the TreeQSM in liters. If the trunk was modeled with
triangulation the total volume is the sum of the triangulated volume of
the stem (bottom), the volume of the stem cylinder (top) and the volume
of the branch cylinders.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read tree qsm and extract tree volume
qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
tot_vol <- tree_volume_qsm(treedata = qsm$treedata)
# Only include cylinders larger than 2.5 cm in radius
tot_vol <- tree_volume_qsm(treedata = qsm$treedata, cylinder = qsm$cylinder,
                           cylindercutoff = 0.25)
} # }
```

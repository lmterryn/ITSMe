# Total trunk volume TreeQSM

Extracts the total trunk volume from the treedata of a TreeQSM.

## Usage

``` r
trunk_volume_qsm(treedata)
```

## Arguments

- treedata:

  Treedata field of a TreeQSM that is returned by
  [`read_tree_qsm`](https://lmterryn.github.io/ITSMe/reference/read_tree_qsm.md).

## Value

The total trunk volume of the TreeQSM in liters. If the trunk was
modelled with triangulation the total volume is the sum of the
triangulated volume of the stem (bottom) and the volume of the stem
cylinder (top).

## Examples

``` r
if (FALSE) { # \dontrun{
# Read tree qsm and extract trunk volume
qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
trunkvol <- trunk_volume_qsm(treedata = qsm$treedata)
} # }
```

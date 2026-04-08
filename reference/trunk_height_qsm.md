# Trunk height TreeQSM

Extracts the trunk height from the treedata of a TreeQSM. For stems
without and with trangulation it selects the TrunkLenght and
TriaTrunkLength object from treedata respectively.

## Usage

``` r
trunk_height_qsm(treedata)
```

## Arguments

- treedata:

  Treedata field of a TreeQSM that is returned by
  [`read_tree_qsm`](https://lmterryn.github.io/ITSMe/reference/read_tree_qsm.md).

## Value

The trunk height of the TreeQSM in meters.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read tree qsm and extract tree height
qsm <- read_tree_qsm(QSM_path = "path/to/qsm.mat")
trunk_height <- trunk_height_qsm(treedata = qsm$treedata)
} # }
```

# Distance from the center

Calculates the distance of each 2D point (X,Y) in a point cloud from the
center (xc, yc) of a circle.

## Usage

``` r
calc_r(x, y, xc, yc)
```

## Arguments

- x:

  X values of the points.

- y:

  Y values of the points.

- xc:

  X-coordinate of the center.

- yc:

  Y-coordinate of the center.

## Value

The distance of 2D points to the center

## Details

Support function used to determine the DBH from a tree point cloud with
[`dbh_pc`](https://lmterryn.github.io/ITSMe/reference/dbh_pc.md) and
[`dab_pc`](https://lmterryn.github.io/ITSMe/reference/dab_pc.md).

## Examples

``` r
if (FALSE) { # \dontrun{
Ri <- calc_r(x_dbh, y_dbh, x_c, y_c)
R <- mean(Ri)
} # }
```

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting_metrics_pc.R
\name{plot_circle_fit_pcs}
\alias{plot_circle_fit_pcs}
\title{Calculate and save figures of \code{\link{diameter_slice_pc}} function}
\usage{
plot_circle_fit_pcs(
  PCs_path,
  extension = ".txt",
  slice_height = 1.3,
  slice_thickness = 0.06,
  functional = TRUE,
  concavity = 4,
  dtm = NA,
  r = 5,
  OUT_path = "./",
  plotcolors = c("#000000", "#1c027a", "#08aa7c", "#fac87f")
)
}
\arguments{
\item{PCs_path}{A character with the path to the folder that contains the
tree point clouds.}

\item{extension}{A character refering to the file extension of the point
cloud files (default=".txt"). Can be ".txt", ".ply" or ".las".}

\item{slice_height}{Numeric value (default = 1.3) that determines the height
above the lowest point of the point cloud at which the diameter is
measured. Parameter of the \code{\link{diameter_slice_pc}} function used to
calculate the diameter of a stem slice.}

\item{slice_thickness}{Numeric value (default = 0.6) that determines the
thickness of the slice which is used to measure the diameter. Parameter of
the \code{\link{diameter_slice_pc}} function used to calculate the diameter
of a stem slice.}

\item{functional}{Logical (default=FALSE), indicates if the functional
diameter should be calculated.}

\item{concavity}{Numeric value (default=2) concavity for the computation of
the functional diameter using a concave hull based on
\code{\link[concaveman]{concaveman}}.}

\item{dtm}{The digital terrain model (default = NA), parameter of
\code{\link{tree_height_pc}}.}

\item{r}{Numeric value (default=5) r, parameter of
\code{\link{tree_height_pc}}. Only relevant if a dtm is provided.}

\item{OUT_path}{A character with the path to the folder where the figures
should be saved (default = current folder).}

\item{plotcolors}{list of three colors for plotting. Only relevant when plot
= TRUE. The stem points, fitted circle, the concave hull and the estimated
center are colored by the first, second and third and fourth element of
this list respectively.}
}
\value{
A list with in the first element a numeric containing the diameter
values for each tree point cloud, the second element the residuals on the
circle fittings, the third element the functional diameters. In the fourth
element there is the list with the plots. Figures are also saved in the
output folder.
}
\description{
Calculates  the diameter and saves the figures acquired when running
\code{\link{diameter_slice_pc}} on multiple tree point clouds in a folder.
}
\details{
Uses \code{\link{read_tree_pc}} to read the point clouds and
\code{\link{diameter_slice_pc}} with parameter plot = TRUE to calculate the
diameter and plot the circle fitting.
}
\examples{
\dontrun{
# Calculate diameters at breast height (1.3m) and save circle fitting figures
diam_values <- plot_circle_fit_pcs(
  PCs_path = "path/to/folder/PCs/",
  extension = ".txt",
  OUT_path = "path/to/figure/folder/"
)
# Calculate diameters at 2.5 m with a slice of 20 cm and save the figures
diam_values <- plot_circle_fit_pcs(
  PCs_path = "path/to/folder/PCs/",
  extension = ".txt", slice_height = 2.5,
  slice_thickness = 0.2,
  OUT_path = "path/to/figure/folder/"
)
}
}

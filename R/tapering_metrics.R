#' Taper plot
#'
#' Plots of the linear regression between diameter and height
#'
#' @param df Data.frame with diameter height information.
#'
#' @return Plot of the linear regression
#'
#' @examples
#' \dontrun{
#' taper_plot <- ggplot2::ggplot(T, ggplot2::aes(diameter, height)) +
#' ggplot2::geom_point() +
#' ggplot2::geom_smooth(method='lm') +
#' ggplot2::geom_text(x = (max(T$diameter)+min(T$diameter))/2,
#'                    y = max(T$height)-0.5, label = lm_eqn(T), parse = TRUE)
#' }
lm_eqn <- function(df) {
  m <- stats::lm(height ~ diameter, df)

  if (stats::coef(m)[2] < 0) {
    eq <-
      substitute(
        italic(height) == a - b %.% italic(diameter) * "," ~  ~ italic(r) ^ 2 ~
          "=" ~ r2,
        list(
          a = format(unname(stats::coef(m)[1]), digits = 2),
          b = format(unname(abs(
            stats::coef(m)[2]
          )), digits = 2),
          r2 = format(summary(m)$r.squared, digits = 3)
        )
      )
  } else {
    eq <-
      substitute(
        italic(height) == a + b %.% italic(diameter) * "," ~  ~ italic(r) ^ 2 ~
          "=" ~ r2,
        list(
          a = format(unname(stats::coef(m)[1]), digits = 2),
          b = format(unname(stats::coef(m)[2]), digits = 2),
          r2 = format(summary(m)$r.squared, digits = 3)
        )
      )
  }
  as.character(as.expression(eq))

}


#' Tapering of a trunk point cloud
#'
#' Calculates the tapering of the trunk based on a linear regression on the
#' diameters at each height of the trunk.
#'
#' @param pc The trunk point cloud as a data.frame with columns X,Y,Z.
#' @param slice_thickness Numeric value (default = 0.1) that determines the
#'   thickness of the slice which is used to measure the diameter at each
#'   height.
#' @param maxtaperheight Numeric value (default = 10) that determines the
#'   maximum height used for the tapering calculation.
#' @param interval Numeric value (default = 1) that determines the interval over
#'   which the median of the diameters is taken to reduce influence of outliers
#'   for the taper measurement. Choose interval value equal to the
#'   slice_thickness if you want to use all the calculated diameters.
#' @param buttress Logical (default=FALSE), indicates if the trees have
#'   buttresses (higher than breast height).
#' @param thresholdbuttress Numeric value (default=0.001). Parameter of the
#'   \code{\link{dab_pc}} function used to calculate the diameter above
#'   buttresses. Only relevant when buttress == TRUE.
#' @param maxbuttressheight Numeric value (default=7). Parameter of the
#'   \code{\link{dab_pc}} function used to calculate the diameter at breast
#'   height. Only relevant when buttress == TRUE.
#' @param plot Logical (default=FALSE), indicates if the taper curve is plotted.
#'
#' @return list with tapering coefficients a and b, dataframe T with the
#'   diameters at each respective height, and the taper plot.
#' @export
#'
#' @examples
#' \dontrun{
#' # Read trunk point cloud
#' pc_trunk <- read_tree_pc(PC_path = "path/to/point_cloud.txt")
#' # calculate the tapering
#' output <- stem_tapering_pc(pc_trunk)
#' }
stem_tapering_pc <-
  function(pc,
           slice_thickness = 0.1,
           maxtaperheight = 10,
           interval = 1,
           buttress = FALSE,
           thresholdbuttress = 0.0015,
           maxbuttressheight = 5,
           plot = FALSE) {
    if (buttress) {
      out <- dab_pc(pc, thresholdbuttress, maxbuttressheight)
      s <- out$h
      d <- out$dab
    } else {
      s <- 1.3
      out <- dbh_pc(pc)
      d <- out$dbh
    }
    out <- diameter_slice_pc(pc, slice_height = s)
    xc <- out$center$par[[1]]
    yc <- out$center$par[[2]]
    h0 <- 0
    max_height <- (max(pc$Z) - min(pc$Z)) - slice_thickness / 2
    if (max_height > maxtaperheight) {
      max_height <- maxtaperheight
    }
    slice_height <- h0 + slice_thickness / 2
    S <- D <- R2s <- Xc <- Yc <- c()
    while (slice_height < max_height + slice_thickness / 2) {
      S <- append(S, slice_height)
      out <- diameter_slice_pc(pc, slice_height, slice_thickness)
      if (length(out$center) > 1) {
        Xc <- append(Xc, out$center$par[[1]])
        Yc <- append(Yc, out$center$par[[2]])
      } else {
        Xc <- append(Xc, NaN)
        Yc <- append(Yc, NaN)
      }
      D <- append(D, out$diameter)
      R2s <- append(R2s, out$R2)
      slice_height <- slice_height + slice_thickness
    }
    res_c <- sqrt((Xc - xc) ^ 2 + (Yc - yc) ^ 2)
    rmse_c <- sqrt(mean(res_c, na.rm = TRUE))
    x <-
      !(is.na(D) |
          is.na(S)) &
      (S > s) & (S < maxtaperheight) #& (D> Lower & D < Upper)
    I <- seq(s, max_height, by = interval)
    if (length(I) > 1) {
      D_median <- c()
      for (i in 1:(length(I) - 1)) {
        D_median <- append(D_median,
                           stats::median(D[x][S[x] > I[i] &
                                                S[x] < I[i + 1]]))
      }
      T <- data.frame(diameter = D_median, height = I[2:length(I)])
      if (nrow(T) > 1) {
        if (plot == TRUE) {
          diameter <- height <- NULL
          taper_plot <-
            ggplot2::ggplot(T, ggplot2::aes(diameter, height)) +
            ggplot2::geom_point() +
            ggplot2::geom_smooth(method = 'lm') +
            ggplot2::geom_text(
              x = (max(T$diameter) + min(T$diameter)) / 2,
              y = max(T$height) - 0.5,
              label = lm_eqn(T),
              parse = TRUE
            )
        } else {
          taper_plot <- NaN
        }
        linear_model <- summary(stats::lm(T$height ~ T$diameter))
        b <- linear_model$coefficients[1]
        a <- linear_model$coefficients[2]
        r2 <- linear_model$adj.r.squared
        rmse_d <- sqrt(mean((((
          S - b
        ) / a) - D) ^ 2, na.rm = TRUE))
      } else {
        a <- b <- r2 <- T <- taper_plot <- rmse_d <- rmse_c <- plot <- NaN
      }
    } else {
      a <- b <- r2 <- T <- taper_plot <- rmse_d <- rmse_c <- plot <- NaN
    }
    return(
      list(
        "a" = a,
        "b" = b,
        "r2" = r2,
        "rmse_d" = rmse_d,
        "rmse_c" = rmse_c,
        "T" = T,
        "plot" = taper_plot
      )
    )
  }

#' Easily perform PCA and draw a biplot of the results with a calibrated axis
#'
#' takes a data frame, performs PCA on the numeric columns, and draws a biplot with a clearly labeled calibrated axis for one of the original numeric columns. The first non-numeric column is used to label PC scores (points).
#'
#'
#' @param data data.frame or object that can be coerced to one. PCA will be performed on numeric columns. The first non-numeric column will be used to label PC scores (points).
#'
#' @param key_axis character indicating the column name of the axis to calibrate. If none is specified, no axes will be calibrated.
#'
#' @param ticklab vector of values indicating axis breaks and tick labels for calibrated axis. If not specified, five evenly spaced round values will be chosen by \code{pretty()}. Ignored if \code{key_axis} is not specified.
#'
#' @param project logical indicating whether projection lines should be drawn from PC scores to PC vectors. Defaults to \code{TRUE}.
#'
#' @param scale logical passed to the \code{scale.} parameter of \code{prcomp()}. Defaults to \code{TRUE} (in contrast to \code{prcomp()}).
#'
#' @param fix_sign logical indicating whether the signs of the loadings and scores should be chosen so that the first element of each loading is non-negative. Defaults to \code{FALSE} (in contrast to \code{princomp()} from which this parameter name was borrowed. \code{prcomp()} does not have such an option.)
#'
#' @param points logical indicating whether to plot principal component scores. Defaults to \code{TRUE}.
#' 
#' @param point_labels logical indicating whether to plot point labels. Defaults to \code{TRUE}.
#'
#' @param arrows logical indicating whether to plot principal component loading vectors. Defaults to \code{TRUE}.
#'
#' @param mult (NULL) numeric value used to scale the length of the rotation vectors, in effect creating secondary axes with different scales than the primary axes, although the secondary axes are not shown. If not specified, the factor is set to the ratio of 75th percentile distance between the scores and the origin to the length of the longest vector.
#'
#' @param vector_colors = vector of two colors, the first for the non-calibrated vectors and the second for the calibrated axis (and vector). Defaults to \code{c("deepskyblue3", "brown2")}.
#' 
#' @param point_color = color for points and point labels. Defaults to \code{"grey60"}.
#' 
#' @param point_size = point size. Defaults to \code{"1.25"}.

#'
#' @details
#' This function was developed mainly for teaching and learning purposes. There are many options for drawing biplots. The main contributions of this one are ease of use and option to calibrate one of the axes. Calibration calculations are performed by \code{calibrate::calibrate()}.
#'
#' @examples
#'
#' swiss$country <- rownames(swiss)
#' draw_biplot(swiss, key_axis = "Agriculture")
#'
#'
#'
#' @section References:
#'
#' (to be added)
#' @export
#'
draw_biplot <- function(data,
                        key_axis = "none",
                        ticklab = NULL,
                        project = TRUE,
                        scale = TRUE,
                        fix_sign = FALSE,
                        points = TRUE,
                        point_labels = TRUE,
                        arrows = TRUE,
                        mult = NULL,
                        vector_colors = c("deepskyblue3", "brown2"),
                        point_color = "grey60",
                        point_size = 1.25
                        ) {

  df <- as.data.frame(data) %>%
    dplyr::select(where(is.numeric))

  label <- as.data.frame(data) %>%
    dplyr::select(!where(is.numeric))

  if (ncol(label) == 0) {
    label <- 1:nrow(data)
  } else {
    label <- label[, 1, drop = TRUE]
  }

  if (key_axis != "none") {
    if (is.character(key_axis) & !(key_axis %in% colnames(df))) {
      message(paste(
        "Column",
        key_axis,
        "doesn't exist, using",
        colnames(df)[1],
        "instead."
      ))
      key_axis <- colnames(df)[1]
    } else if (is.numeric(key_axis) &
               (key_axis < 1 | key_axis > ncol(df))) {
      message(paste(
        "Column",
        key_axis,
        "doesn't exist, using Column 1 instead."
      ))
      key_axis <- colnames(df)[1]
    }
  }

  if (key_axis != "none") {
    key_axis_col <- df[, key_axis, drop = TRUE]
    if (is.null(ticklab))
      ticklab <- pretty(key_axis_col)
  }

  pca <- stats::prcomp(df, scale. = scale)
  loadings <- pca$rotation
  # fix sign
  if (fix_sign) {
    for (i in 1:nrow(loadings)) {
      if (loadings[1, i] < 0)
        loadings[, i] <- loadings[, i] * -1
    }
    scores <- as.matrix(scale(df)) %*% loadings
  } else {
    scores <- pca$x
  }

  s <- summary(pca)
  xlab <-
    paste("PC 1 (", round(s$importance[2, 1] * 100, 1), "%)", sep = "")
  ylab <-
    paste("PC 2 (", round(s$importance[2, 2] * 100, 1), "%)", sep = "")

  dfpoints <- data.frame(scores) %>%
    dplyr::mutate(label = label)

  dfarrows <- data.frame(xend = loadings[, 1], yend = loadings[, 2],
                         label = rownames(loadings))
  dfarrows$angle <- atan2(dfarrows$yend, dfarrows$xend) * 180 / pi - 90 * sign(dfarrows$xend) + 90
  dfarrows$highlight <- ifelse(dfarrows$label == key_axis, TRUE, FALSE)

  if (key_axis != "none") {
    c <- calibrate::calibrate(g = loadings[key_axis, c("PC1", "PC2")],
                   y = key_axis_col - mean(key_axis_col),
                   tm = ticklab - mean(key_axis_col),
                   Fr = scores[, c("PC1", "PC2")],  # matrix
                   tmlab = ticklab,
                   tl = .2,
                   graphics = FALSE,
                   verb = FALSE)

    dfpoints <- dfpoints %>%
      dplyr::mutate(xsdrop = c$Fpr[, 1], ysdrop = c$Fpr[, 2])

    dfaxis <- data.frame(x = c$M[1, 1], y = c$M[1, 2],
                         xend = c$M[nrow(c$M), 1],
                         yend = c$M[nrow(c$M), 2])

    dfticks <- data.frame(c$M, c$Mn, ticklab) %>%
      stats::setNames(c("x", "y", "xend", "yend", "label"))
    dfticks$label_x <- 2 * dfticks$xend - dfticks$x
    dfticks$label_y <- 2 * dfticks$yend - dfticks$y
  }



  # determine rotation vector multiplier if not provided (75th percentile of point distances from origin over longest rotation vector)

  if (is.null(mult)) {
    points_dist <- sqrt(dfpoints$PC1^2 + dfpoints$PC2^2)
    arrows_length <- sqrt(dfarrows$xend^2 + dfarrows$yend^2)
    mult <- stats::quantile(points_dist, probs = .75) / max(arrows_length)
  }

  # points and point labels
  if (points) {
    pointalpha <- 1
    labelalpha <- ifelse(point_labels, 1, 0)
  } else {
    pointalpha <- 0
    labelalpha <- 0
  }

  g <- ggplot2::ggplot(dfpoints, ggplot2::aes(x = .data$PC1, y = .data$PC2)) +
    ggplot2::geom_point(color = point_color, alpha = pointalpha, size = point_size) +
    ggrepel::geom_text_repel(ggplot2::aes(label = label), size = 2.75,
                             color = point_color, alpha = labelalpha) +
  #  ggplot2::coord_fixed() +
    ggplot2::scale_x_continuous(expand = c(.1, .1)) +
    ggplot2::scale_y_continuous(expand = c(.1, .1)) +
    ggplot2::scale_color_manual(values = vector_colors, guide = "none") +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::theme_bw(14)

  # rotation vectors
  if (arrows) {
  g <- g +
    ggplot2::geom_segment(data = dfarrows,
                 ggplot2::aes(x = 0, y = 0, xend = .data$xend * mult,
                     yend = .data$yend * mult, color = .data$highlight),
                 arrow = grid::arrow(length = grid::unit(.03, "npc"))) +
    ggplot2::geom_text(data = dfarrows,
              ggplot2::aes(x = .data$xend * mult * 1.03,
                           y = .data$yend * mult * 1.03,
                  label = label, angle = .data$angle,
                  color = .data$highlight, size = as.integer(.data$highlight),
                  hjust = -.5 * sign(.data$xend) + .5)) +
    ggplot2::scale_size(range = c(3, 4), guide = "none")
  }


if (key_axis != "none") {
  # calibrated axis: axis, tick marks, tick mark labels

  g <- g +
    ggplot2::geom_segment(data = dfaxis,
                          ggplot2::aes(x = .data$x, y = .data$y,
                                       xend = .data$xend, yend = .data$yend),
                          color = vector_colors[2],
                          linewidth = .2) +
    ggplot2::geom_segment(data = dfticks,
                          ggplot2::aes(x = .data$x, y = .data$y,
                                       xend = .data$xend, yend = .data$yend),
                          color = vector_colors[2]) +
    ggplot2::geom_text(data = dfticks,
                       ggplot2::aes(x = .data$label_x, y = .data$label_y,
                                    label = label),
                       color = vector_colors[2], size = 3)

  # projection lines
  if (project & points)
    g <- g +
      ggplot2::geom_segment(data = dfpoints, ggplot2::aes(x = .data$PC1, y = .data$PC2,
                  xend = .data$xsdrop, yend = .data$ysdrop), linetype = "dashed",
                  linewidth = .25,
                  col = point_color)
  }

  g
}

utils::globalVariables("where")
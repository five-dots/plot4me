
#' @export
cont_x <- function(df, x_var, hist = TRUE, bw = bw_scott, dens = TRUE,
                   norm = TRUE, mean = TRUE, median = TRUE,
                   theme = theme_plot4me) {

  if (!is.data.frame(df)) stop("df must be a data.frame.", call. = FALSE)
  x_var <- rlang::enquo(x_var)
  x <- dplyr::pull(df, !!x_var)
  if (!is.numeric(x)) stop("x must be numeric.", call. = FALSE)

  if (norm || mean || median) {
    mean_x <- mean(x, na.rm = TRUE)
    median_x <- stats::median(x, na.rm = TRUE)
    sd_x <- stats::sd(x, na.rm = TRUE)
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = !!x_var))

  ## histogram
  dens_hist <- hist && (dens || norm)
  count_hist <- hist && !(dens || norm)
  if (dens_hist)
    p <- p + ggplot2::geom_histogram(ggplot2::aes(y = ..density..),
                                     binwidth = bw, fill = "white",
                                     color = "black", boundary = 0)
  if (count_hist)
    p <- p + ggplot2::geom_histogram(binwidth = bw, fill = "white",
                                     color = "black", boundary = 0)

  ## densitiy
  if (dens)
    p <- p + ggplot2::geom_density(fill = "black", alpha = 0.2)

  ## norm_dist
  if (norm)
    p <- p + ggplot2::stat_function(fun = stats::dnorm,
                                    args = list(mean = mean_x, sd = sd_x))

  ## mean vline
  if (mean)
    p <- p + ggplot2::geom_vline(xintercept = mean_x, color = "red")

  ## mean vline
  if (median)
    p <- p + ggplot2::geom_vline(xintercept = median_x, color = "blue")

  p + theme()
}

#' @export
disc_x_cont_y <- function(df, x_var, y_var, width = 0.25, violin = TRUE,
                          box = TRUE, point = TRUE, mean = TRUE,
                          theme = theme_plot4me) {

  if (!is.data.frame(df)) stop("df must be a data.frame.", call. = FALSE)

  x_var <- rlang::enquo(x_var)
  y_var <- rlang::enquo(y_var)
  x <- dplyr::pull(df, !!x_var)
  y <- dplyr::pull(df, !!y_var)

  if (!is.numeric(y))
    stop("y must be numeric.", call. = FALSE)

  if (!is.factor(x))
    df <- dplyr::mutate(df, !!x_var := as.factor(!!x_var))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = !!x_var, y = !!y_var,
                                        color = !!x_var, fill = !!x_var))

  if (violin)
    p <- p + ggplot2:: geom_violin(alpha = 0.3)

  if (box)
    p <- p + ggplot2::geom_boxplot(color = "black", fill = "white",
                                   width = width)

  if (point)
    p <- p + ggplot2::geom_point(
      position = ggplot2::position_jitter(width = width * 1.25, height = 0.0),
      alpha = 0.1, color = "black")

  if (mean)
    ## p <- p + ggplot2::stat_summary(fun.y = base::mean, geom = "point",
    ##                                color = "black", shape = 4, size = 5)
    p <- p + ggplot2::stat_summary(fun = base::mean, geom = "point",
                                   color = "black", shape = 4, size = 5)

  p + theme_plot4me() + ggplot2::theme(legend.position = "none")
}

#' @export
cont_x_cont_y <- function(df, x_var, y_var, point = TRUE, smooth = TRUE,
                          theme = theme_plot4me) {

  if (!is.data.frame(df)) stop("df must be a data.frame.", call. = FALSE)

  x_var <- rlang::enquo(x_var)
  y_var <- rlang::enquo(y_var)
  x <- dplyr::pull(df, !!x_var)
  y <- dplyr::pull(df, !!y_var)

  if (!is.numeric(x))
    stop("x must be numeric.", call. = FALSE)

  if (!is.numeric(y))
    stop("y must be numeric.", call. = FALSE)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = !!x_var, y = !!y_var))

  if (point)
    p <- p + ggplot2::geom_point(alpha = 0.2)

  if (smooth)
    p <- p + ggplot2::geom_smooth(method = stats::lm)

  p + theme_plot4me()
}

#' @export
theme_plot4me <- function() {
  ggplot2::theme_light() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold.italic"),
      axis.title = ggplot2::element_blank()
      ## legend.position = "none"
    )
}

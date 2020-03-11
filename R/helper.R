
.bw <- function(x, f) (max(x) - min(x)) / f(x)

#' @export
bw_FD <- function(x) .bw(x, grDevices::nclass.FD)

#' @export
bw_scott <- function(x) .bw(x, grDevices::nclass.scott)

#' @export
bw_Sturges <- function(x) .bw(x, grDevices::nclass.Sturges)

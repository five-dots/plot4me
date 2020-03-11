
test_that("cont_x() functions work", {

  df <- data.frame(x = rnorm(1000))
  x_var <- rlang::quo(x)
  hist <- TRUE
  bw <- bw_scott
  hist <- TRUE
  dens <- TRUE
  norm <- FALSE
  mean <- FALSE
  median <- TRUE

  p <- cont_x(df, x, hist, bw, dens, norm, mean, median)

  expect_is(p, "gg")
  expect_is(p, "ggplot")

  expect_error(cont_x(as.matrix(iris), Sepal.Length))
  expect_error(cont_x(iris, Species))

})

test_that("disc_x_cont_y() functions work", {

  df <- data.frame(
    x = as.factor(letters[sample.int(3, 1000, replace = TRUE)]),
    y = rnorm(1000)
  )

  x_var <- rlang::quo(x)
  y_var <- rlang::quo(y)

  width <- 0.25
  violin <- TRUE
  box <- TRUE
  point <- TRUE
  mean <- TRUE

  p <- disc_x_cont_y(df, x, y, width, violin, box, point, mean)

  expect_is(p, "gg")
  expect_is(p, "ggplot")

  expect_error(disc_x_cont_y(df, y, x))
  expect_error(disc_x_cont_y(as.matrix(df), x, y))

})

test_that("cont_x_cont_y() functions work", {

  df <- data.frame(
    x = rnorm(1000),
    y = rnorm(1000)
  )

  x_var <- rlang::quo(x)
  y_var <- rlang::quo(y)

  point <- TRUE
  smooth <- TRUE

  p <- cont_x_cont_y(df, x, y, point, smooth)

  expect_is(p, "gg")
  expect_is(p, "ggplot")

  expect_error(cont_x_cont_y(as.matrix(df), x, y))

})

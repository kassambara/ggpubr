test_that("xscale()/yscale() honour every value they document", {
  # The @param block lists sqrt as allowed, but the switch() handled only
  # percent/dollar/scientific. An unmatched switch() returns NULL, and adding
  # NULL to a ggplot is a no-op, so xscale("sqrt") silently transformed nothing.
  for (s in c("log2", "log10", "sqrt", "percent", "dollar", "scientific")) {
    expect_s3_class(xscale(s), "Scale")
    expect_s3_class(yscale(s), "Scale")
  }
  # "none" returning NULL is correct: it means "add no scale".
  expect_null(xscale("none"))
  expect_null(yscale("none"))
})

test_that("the sqrt helpers transform the axis, not merely return an object", {
  d <- data.frame(x = c(1, 4, 9, 16, 25), y = c(1, 4, 9, 16, 25))
  labs_of <- function(p, ax) {
    pn <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]
    as.character(if (ax == "x") pn$x$get_labels() else pn$y$get_labels())
  }
  base <- ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_point()
  expect_equal(labs_of(base + xscale("sqrt"), "x"),
               labs_of(base + ggplot2::scale_x_sqrt(), "x"))
  expect_equal(labs_of(base + yscale("sqrt"), "y"),
               labs_of(base + ggplot2::scale_y_sqrt(), "y"))
  # and it must differ from adding nothing, which is what the defect produced
  expect_false(identical(labs_of(base + xscale("sqrt"), "x"), labs_of(base, "x")))
})

test_that("sqrt transforms survive the formatting flag", {
  observed <- c(
    x = xscale("sqrt", .format = TRUE)$trans$name,
    y = yscale("sqrt", .format = TRUE)$trans$name
  )
  expect_identical(observed, c(x = "sqrt", y = "sqrt"))
})

test_that("transform= and the deprecated trans= give identical scales", {
  # ggplot2 3.5.0 renamed trans to transform and deprecated trans. The rename
  # must not change what is drawn.
  d <- data.frame(x = c(1, 10, 100, 1000), y = c(1, 2, 4, 8))
  labs_of <- function(sc) as.character(
    ggplot2::ggplot_build(
      ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_point() + sc
    )$layout$panel_params[[1]]$x$get_labels())
  for (tr in c("log2", "log10", "sqrt")) {
    expect_equal(labs_of(ggplot2::scale_x_continuous(transform = tr)),
                 labs_of(suppressWarnings(ggplot2::scale_x_continuous(trans = tr))))
  }
})

test_that("create_aes() accepts a length > 1 value instead of erroring", {
  # is_parsable_aes() combined its tests with the elementwise &, so a length > 1
  # value produced a length > 1 condition, which if() rejects from R 4.2.
  expect_no_error(create_aes(list(x = c("a", "b"))))
  expect_no_error(create_aes(list(x = character(0))))
  expect_no_error(create_aes(list(x = c("1", "2"))))
})

test_that("create_aes() still resolves length-1 values exactly as before", {
  # The fix must not change any input that already worked.
  expect_equal(create_aes(list(x = "wt"))$x, rlang::quo(wt), ignore_attr = TRUE)
  # a numeric string stays a literal, not a symbol
  expect_identical(create_aes(list(x = "1"))$x, "1")
  # a non-character value passes through untouched
  expect_identical(create_aes(list(x = 1))$x, 1)
})

test_that("histogram labels survive a facet named like an internal column", {
  # .hist_label_data() built list-columns called hist.data and lab.data, so
  # faceting by a user column of either name overwrote the grouping key.
  set.seed(42)
  panels_match <- function(facet_name) {
    d <- data.frame(v = c(stats::rnorm(40), stats::rnorm(40, 3)))
    d[[facet_name]] <- rep(c("p", "q"), each = 40)
    b <- ggplot2::ggplot_build(
      gghistogram(d, x = "v", label = "v", facet.by = facet_name, y = "..count..")
    )
    lab_layer <- b$data[[length(b$data)]]
    length(unique(lab_layer$PANEL)) == nrow(b$layout$layout)
  }
  expect_true(panels_match("lab.data"))
  expect_true(panels_match("hist.data"))
  expect_true(panels_match("grp"))
})

test_that("the histogram label path calls unnest() with its required cols", {
  # tidyr signals "`cols` is now required when using `unnest()`", which becomes
  # an error under warn = 2. Assert no such condition is raised.
  set.seed(42)
  d <- data.frame(v = c(stats::rnorm(40), stats::rnorm(40, 3)),
                  g = rep(c("p", "q"), each = 40))
  conditions <- character(0)
  withCallingHandlers(
    invisible(ggplot2::ggplot_build(
      gghistogram(d, x = "v", label = "v", facet.by = "g", y = "..count..")
    )),
    warning = function(w) {
      conditions <<- c(conditions, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(grep("cols.*required|unnest", conditions), 0)
})

test_that("the percent/dollar/scientific scales format labels as their names say", {
  # The loop above only proves these branches build a Scale. It would pass with
  # the labellers swapped for one another, which is exactly the mistake a
  # supersession sweep can make. Assert the strings instead.
  #
  # This locks against a future wrong substitution rather than against the
  # pre-branch code: the superseded originals (scales::percent, ::dollar,
  # ::scientific) produced these same strings, which is why replacing them was
  # safe. Its control is therefore a mutation, not a revision.
  fmt <- function(s) {
    sc <- xscale(s)
    sc$labels(c(0.25, 0.5))
  }
  expect_equal(fmt("percent"), c("25%", "50%"))
  expect_match(fmt("dollar")[1], "^\\$0\\.25$")
  expect_match(fmt("scientific")[1], "e")
})

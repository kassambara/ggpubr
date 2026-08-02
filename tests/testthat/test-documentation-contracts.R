.render_help <- function(topic) {
  help_topic <- help(topic, package = "ggpubr")
  rd <- if (is.list(help_topic) && !is.null(help_topic$path)) {
    tools::parse_Rd(help_topic$path)
  } else {
    utils:::.getHelpFile(help_topic)
  }
  paste(capture.output(tools::Rd2txt(rd)), collapse = "\n")
}

.render_help_item <- function(topic, item) {
  lines <- strsplit(.render_help(topic), "\n", fixed = TRUE)[[1]]
  item_lines <- trimws(lines)
  start <- which(startsWith(item_lines, paste0(item, ":")))[1]
  stop <- min(length(lines), start + 7L)
  paste(item_lines[start:stop], collapse = " ")
}

test_that("ggdotplot documents size as dot scaling", {
  expect_match(.render_help("ggdotplot"), "relative diameter of dots")
})

test_that("layer documentation does not invert inherit.aes defaults", {
  topics <- c("geom_pwc", "stat_compare_means")
  inherit_docs <- vapply(
    topics, .render_help_item, character(1), item = "inherit.aes"
  )
  expect_identical(
    c(
      defaults_true = all(vapply(
        list(geom_pwc, stat_compare_means),
        function(fun) identical(formals(fun)$inherit.aes, TRUE), logical(1)
      )),
      docs_point_to_usage = all(grepl(
        "function usage states the default for each layer",
        inherit_docs,
        fixed = TRUE
      ))
    ),
    c(defaults_true = TRUE, docs_point_to_usage = TRUE)
  )
})

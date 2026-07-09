# Regression tests for #751: stat_compare_means(label = "p.format") failed with
# `could not find function "create_p_label"` when ggpubr was loaded but not
# attached (i.e. called as ggpubr::stat_compare_means() without library(ggpubr)).
# The label expression `after_stat(create_p_label(p.format))` is evaluated in a
# data mask that does not see the ggpubr namespace off the search path, so the
# internal helper must be namespace-qualified (ggpubr::create_p_label).

df <- ToothGrowth
df$dose <- factor(df$dose)

test_that(".update_mapping() namespace-qualifies create_p_label() (#751)", {
  # Every label that routes through create_p_label() inside after_stat() must
  # reference it as ggpubr::create_p_label so the mask resolves it without the
  # package being attached. A revert to the unqualified form fails this test.
  for (lab in c("p.format", "..p.format..", "p", "..p..")) {
    q <- ggpubr:::.update_mapping(ggplot2::aes(), lab)$label
    txt <- rlang::as_label(q)   # mapping$label is a quosure
    expect_match(txt, "ggpubr::create_p_label", fixed = TRUE,
                 info = paste("label =", lab))
  }
})

test_that("label expression resolves create_p_label off the search path (#751)", {
  # Faithfully reproduce the reporter's scenario -- ggpubr loaded but NOT attached
  # -- WITHOUT globally detaching the package (which would break internal-function
  # lookup for other test files sharing the session). after_stat() evaluates its
  # inner call in a data mask whose lexical scope walks up to the search path; we
  # emulate that by evaluating the mapping's inner call in an environment parented
  # at baseenv() (so ggpubr is not reachable, exactly as when unattached). With the
  # namespace qualification it resolves; an unqualified create_p_label() errors
  # here with "could not find function", which is the #751 crash.
  for (lab in c("p.format", "p")) {
    q <- ggpubr:::.update_mapping(ggplot2::aes(), lab)$label
    expr <- rlang::quo_get_expr(q)  # ggplot2::after_stat(ggpubr::create_p_label(p.format))
    inner <- expr[[2L]]             # ggpubr::create_p_label(p.format)
    inner[[2L]] <- 0.05            # substitute the p.format stat column with a value
    clean <- new.env(parent = baseenv())   # no ggpubr on the search path
    expect_no_error(val <- eval(inner, envir = clean))
    expect_true(is.character(val) && nzchar(val), info = paste("label =", lab))
  }
})

test_that("attached-case output is unchanged (no regression, #751)", {
  # With ggpubr attached (the normal case), the fix must produce the exact same
  # label text as before -- it only qualifies the namespace, same function.
  if (!"package:ggpubr" %in% search()) suppressMessages(attachNamespace("ggpubr"))
  p <- ggboxplot(df, x = "supp", y = "len") +
    stat_compare_means(label = "p.format")
  lbl <- ggplot2::ggplot_build(p)$data[[2]]$label
  # supp has two groups -> a single Wilcoxon p, formatted as "p = <value>".
  expect_length(unique(lbl), 1L)
  expect_match(unique(lbl), "^p = ")
})

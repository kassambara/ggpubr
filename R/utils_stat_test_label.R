# check symnum.args --------------------------------
fortify_signif_symbols_encoding <- function(symnum.args = list()){
  if(.is_empty(symnum.args)){
    symnum.args <- list(
      cutpoints = c(0, 1e-04, 0.001, 0.01, 0.05, Inf),
      symbols = c("****", "***", "**", "*", "ns")
    )
  }
  else {
    symnum.args.isok <- (length(symnum.args$symbols) == length(symnum.args$cutpoints)-1)
    if(!symnum.args.isok)
      stop(
        "Incorrect format detected in 'symnum.args'. ",
        "Check the documentation. ",
        "length(symbols) should be length(cutpoints)-1",
        call. = FALSE
        )
  }
  symnum.args
}

# Check user specified label -----------------------------

# Check if is glue package expression
is_glue_expression <- function(label){
  grepl("\\{|\\}", label, perl = TRUE)
}

# Check if label is a plotmath expression
contains_plotmath_symbols <- function(label){
  grepl("==|italic\\s?\\(|bold\\s?\\(|bolditalic\\s?\\(", label)
}
starts_with_list <- function(label){
  grepl("^list\\s?\\(", label)
}
is_plotmath_expression <- function(label){
  starts_with_list(label) | contains_plotmath_symbols(label)
}

# Fortify label --------------------
# if label is plotmath expression, then
# fortify it in case users miss something
contains_p_signif <- function(label){
  any(grepl("p*\\.signif", label))
}
contains_twoequal_signs <- function(label){
  any(grepl("==", label))
}
replace_simple_by_double_equals <- function(label){
  if(!contains_twoequal_signs(label)){
    label <- gsub("=", "==", label)
  }
  label
}

escape_psignif_asteriks <- function(label){
  # Escaping asteriks (special plotmath symbols) in p.signif or p.adj.signif by adding quote
  label <- gsub(pattern = "\\}\\{(p.*.signif)\\}", replacement = "}*`{\\1}`", x = label)
  # p signif preceded with space
  label <- gsub(pattern = "~\\{(p.*.signif)\\}", replacement = "~`{\\1}`", x = label)
  label <- gsub(pattern = "=+?\\s+?\\{(p.*.signif)}", replacement = "== `{\\1}`", x = label)
  label
}



# Get statistical test label to be displayed -------------------
# stat.test: statistical test output
# description: the description of the stat test, for example: "Anova"
# label: can be p, p.signif, p.adj or glue expression
add_stat_label <- function (stat.test,  label = NULL){
  is_plotmath <- FALSE
  if(is.null(label)){
    stat.test$label <- add_p(stat.test$p.format)
  }
  else{
    is_plotmath <- is_plotmath_expression(label)
    if(is_plotmath){
      label <- fortify_plotmath(label)
    }
    if(is_glue_expression(label)){
      stat.test <- stat.test %>% mutate(label = glue(label))
    }
    else {
      if(!(label %in% colnames(stat.test))){
        stop(
          "Can't find the value of the argument label ('", label, "') in the computed ",
          "statistical test table.", call. = FALSE
          )
      }
      stat.test$label <- as.character(stat.test[[label]])
    }
  }
  label <- gsub(pattern = "=+(\\s+)?<", replacement = "<\\1", stat.test$label )
  if(is_plotmath){
    label <- replace_simple_by_double_equals(label)
    label <- gsub(pattern = "\\s", replacement = "~", label)
    label <- gsub(pattern = "~==~", replacement = "~`=`~", label )
    label <- gsub(pattern = "~<~", replacement = "~`<`~", label )
    # Make sure that decimal values will be displayed asis in character when parsed by ggplot
    # Add quote around numeric values
    label <- gsub("([0-9.-]+)", "'\\1'", label)
  }
  stat.test$label <- label
  stat.test
}

fortify_plotmath <- function(label){
  label <- gsub(pattern = "~", replacement = " ", x = label, fixed = TRUE)
  if(!starts_with_list(label)) label <- paste0("list(", label, ")")
  if(contains_p_signif(label)){
    # Escape p signif stars
    label <- gsub(pattern = "\\}\\{p.signif\\}", replacement = "}*`{p.signif}`", x = label)
    label <- gsub(pattern = "\\}\\{p.adj.signif\\}", replacement = "}*`{p.adj.signif}`", x = label)
    # Escape p signif stars preceded by space
    label <- gsub(pattern = "\\s\\{p.signif\\}", replacement = " `{p.signif}`", x = label)
    label <- gsub(pattern = "\\s\\{p.adj.signif\\}", replacement = " `{p.adj.signif}`", x = label)
    # Escape p signif stars preceded by equal signs
    label <- gsub(pattern = "=(\\s+)?\\{p.signif}", replacement = "=\\1`{p.signif}`", x = label)
    label <- gsub(pattern = "=(\\s+)?\\{p.adj.signif}", replacement = "=\\1`{p.adj.signif}`", x = label)
  }
  label <- gsub(pattern = "eta2[g]", replacement = "eta[g]^2", x = label, fixed = TRUE)
  label <- gsub(pattern = "eta2[p]", replacement = "eta[p]^2", x = label, fixed = TRUE)
  label
}

# Add p prefix
# add_p(0.05) --> p = 0.05
# add_p("<0.05") --> p < 0.05
add_p <-function(label){
  contain.inf.symbol <- grepl("<", label)
  label2 <- paste0("p", " = ", label)
  if(sum(contain.inf.symbol) > 0){
    # no need to add =
    label2[contain.inf.symbol] <- paste0("p", label[contain.inf.symbol])
  }
  # Add space before and after inf symbol
  label2 <- gsub(pattern = "<", replacement = " < ", label2)
  label2
}

# Add statistical test number of samples
add_stat_n <- function(stat.test){
  stat.test$n <- rstatix::get_n(stat.test)
  stat.test
}


# gg layer data checking --------------------------
# Check whether there is multiple grouping variables
# This is the case for grouped plots
contains_multiple_grouping_vars <- function(data){
  !all(data$x == data$group)
}

# Check if group variable is specified in aes
is_group_aes_specified <- function(mapping){
  answer <- FALSE
  if(is.null(mapping)) {
    answer <- FALSE
  }
  else if(!is.null(mapping$group)) {
    answer <- TRUE
  }
  answer
}


# Manipulating statistical test outputs -----------------
keep_only_tbl_df_classes <- function(x){
  toremove <- setdiff(class(x), c("tbl_df", "tbl", "data.frame"))
  if(length(toremove) > 0){
    class(x) <- setdiff(class(x), toremove)
  }
  x
}

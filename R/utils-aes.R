# Extract group name from mapping, aes
aes_get_group <- function(mapping){
  group <- NULL
  if(!is.null(mapping)){
    if(!is.null(mapping$group)) group <- rlang::as_label(mapping$group)
  }
  group
}

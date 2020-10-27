#' This function write new yaml to the versioned header
#' @importFrom rlang %||%
write_yaml_version <- function(orig_text, version){
  is_solution <- grepl("^solution_", version, perl = TRUE)
  orig_yaml <- rmarkdown::yaml_front_matter(orig_text)
  version_yaml <-
    if (is_solution) orig_yaml$version[[glue::glue("solution_{version}")]] %||% orig_yaml$version[[version]]
    else orig_yaml$version[[version]]
  if (!length(version_yaml)) return(orig_yaml)
  orig_yaml$version <- NULL
  yaml::as.yaml(modifyList(orig_yaml, version_yaml, keep.null = FALSE))
}

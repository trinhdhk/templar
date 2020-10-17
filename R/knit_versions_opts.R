#' Generator of knit_versions.
#' @description This function is intended to be used in the context of Rmd files only
#' @param ... Whatever you want to edit the knit_versions() function. See \link{knit_versions}
#' @return A partial call of knit_versions.
knit_versions_opt <- function(...){
  purrr::partial(knit_versions, ...)
}

#' Function to write the different versions.
#'
#' @param v Version list
#' @param orig_name Original file name
#' @param orig_dir Original file directory
#' @param orig_text Original file text
#' @param sec_info Sections info
#' @param all_info All chunks info
#' @param folders Folders to store the new generated files
#'
#' @return Doesn't return anything, but generates the new RMD files.

write_version <- function(v, orig_name, orig_dir, orig_text, sec_info, all_info, folders, ..., .use_jobs = identical(Sys.getenv("RSTUDIO"), "1")){
  options(knitr.duplicate.label = 'allow')
  new_name <- get_new_name(v, orig_name, orig_dir, folders)

  new_yaml <- write_yaml_version(file.path(orig_dir, orig_name), v)
  new_text <- version_cleaner(v, orig_text, sec_info, all_info, orig_dir)

  delimiters <- grep("^(---|\\.\\.\\.)\\s*$", new_text)
  new_text_no_yaml <- new_text[-(delimiters[1]):-(delimiters[2])]
  new_text <- c('---', new_yaml, '---', new_text_no_yaml)

  writeLines(new_text, new_name)

  if (.use_jobs) {
   .__interactive_knit_jobs__(new_name, ... )
  } else {
    rmarkdown::render(new_name, envir = new.env(), ...)
  }
  invisible(TRUE)
}

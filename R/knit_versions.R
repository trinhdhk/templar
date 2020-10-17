#' Knit different versions of a file from chunk and section options
#'
#' \code{knit_versions} is a function that provides an independent version or `versions`
#' with extra options to give more authorities to the users.
#'
#' @param orig_file The original file serving as the template
#' @param global_eval Logical.
#' @param to_knit Character vector specifying which versions to write and knit
#' into separate files.  If not specified, all versions are produced.
#' @param folders List of versions and subfolder to put them in. Use pattern
#' \code{version_name = folder_name}.  Default is each version in its own folder.
#' @param knit_global Whether to knit the global file, default to TRUE
#'
#' @returns none
#'
#' @export
knit_versions <- function(orig_file,
                          global_eval = TRUE,
                          to_knit = NULL,
                          folders = NULL,
                          knit_global = TRUE,
                          .use_jobs = identical(Sys.getenv("RSTUDIO"), "1"),
                          .ncores = ceiling(parallel::detectCores()/2),
                          ...){
  if (missing(orig_file))
    return(purrr::partial(knit_versions,
                          global_eval = {{global_eval}},
                          to_knit = {{to_knit}},
                          folders = {{folders}},
                          knit_global = {{knit_global}},
                          .use_jobs = {{.use_jobs}},
                          .ncores = {{.ncores}},
                          ...))
  orig_dir  <- dirname(orig_file)
  orig_name <- basename(orig_file)
  # browser()
  orig_text <- readLines(orig_file)
  has_call <-
    length(grep("(?<!_)versions\\(", orig_text, perl = TRUE)) &&
    !all(grepl("^#", grep("(?<!_)versions\\(", orig_text, value = TRUE, perl = TRUE)))
  orig_text <- prep_orig_text(orig_text, global_eval)
  # tmpdir <- if (.use_jobs) tempdir() else NULL

  if (knit_global){
    if (has_call)
      message("Found ", crayon::green('versions()'), " call within markdown file, pass to `rmarkdown::render`...")
    if (.use_jobs){
      .__interactive_knit_jobs__(orig_file, ...)
    } else{
      message("- Knitting global file.")
      rmarkdown::render(orig_file, ...)
    }
    if (has_call) return(invisible(TRUE))
  }

  # Pull out chunk label info pertaining to versions

  chunk_info <- get_version_chunks(orig_text)
  sec_info <- get_version_text(orig_text)

  # This condition is to support when people don't submit %%% sections

  if (length(sec_info)){
    all_info <-
      dplyr::full_join(chunk_info, sec_info,
                       by = intersect(names(chunk_info), names(sec_info))) %>% #to suppress tidyverse's message
      dplyr::mutate_all(~tidyr::replace_na(.,FALSE))
  } else {
    all_info <- chunk_info %>%
      dplyr::mutate_all(~tidyr::replace_na(.,FALSE))
  }

  not_versions <- c("starts", "ends", "is_versioned", "none")

  # In case we only want to knit a few of the versions

  if (length(to_knit)) {
    all_info <- all_info[, c(not_versions, to_knit)]
  } else {
    to_knit <- setdiff(names(all_info), not_versions)
  }

  # Handles the absence of solution code chunks

  if (is.null(all_info[["solution"]])) {
    all_info[["solution"]] <- FALSE
  }

  to_knit <- stringr::str_subset(to_knit, "solution", negate = TRUE)

  all_info <- purrr::map_df(to_knit, get_solution_chunks, all_info) %>%
    dplyr::select(-solution) %>%
    dplyr::group_by(starts, ends) %>%
    dplyr::summarise(dplyr::across(.fns = any, na.rm = TRUE), .groups = "drop")

  to_knit <- setdiff(names(all_info), not_versions)

  if (length(folders) && !is.list(folders))
    folders <- structure(as.list(rep(folders, length(to_knit))), names = to_knit)

  # Write and knit file for each version

  if (.ncores > 1 && !.use_jobs){
    # fn_map <- furrr::future_map
    future::plan(future::multisession, workers = .ncores)
    message("- Knitting versioned files.")
  }
  # else {
    # fn_map <- purrr::map
  # }

  if (.use_jobs) cat(crayon::green("Job created for "))
  for (tk in to_knit){
    if (.use_jobs) cat(tk, " ")
    write_version(tk, orig_name, orig_dir, orig_text, sec_info, all_info, folders, ...,
                  .use_jobs = .use_jobs)
  }
  # fn_map(to_knit, write_version,
  #        orig_name, orig_dir, orig_text, sec_info, all_info, folders, .use_jobs)

  cat("\n")
  return(invisible(TRUE))
}

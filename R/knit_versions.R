#' Knit different versions of a file from chunk and section options
#'
#' \code{knit_versions} is a function that provides an independent version or `versions`
#' with extra options to give more authorities to the users.
#'
#' @param orig_file The original file serving as the template
#' @param global_eval Logical.
#' @param to_knit Character vector specifying which versions to write and knit
#' into separate files.  If not specified, all versions are produced.
#' @param folders List of versions and sub-folder to put them in. Use pattern
#' \code{version_name = folder_name}.  Default is each version in its own folder.
#' @param knit_global Whether to knit the global file, default to TRUE.
#' @param solution_with_question Whether to include questions in solutions.
#' @param .use_jobs Default to TRUE. Whether to use RStudio's local jobs for better interactivity.
#' When "jobs" is used, .ncores will not be respected.
#' @param .ncores Default to half of the number of cores available passed to \link[future]{multiprocess}.
#' This is only respected when .use_jobs == FALSE. .ncores == 1, versioned files will be knitted sequentially.
#' @param ... additional parameters passed to rmarkdown::render.
#' Note that when orig_file is missing and the partial call is returned, this argument will be ignored to
#' avoid conflicts with RStudio's Knit button.
#'
#' @return if orig_file is not missing, will trigger knitting and return TRUE;
#' otherwise will return a partial call to itself waiting for the file to be supplied.
#'
#' @details
#' This function is meant to be call independently from the Knitting process and/or
#' replacing the knitting engine by add it to the yaml part of Rmd files.
#' In the latter case, orig_file will not be presented and a partial call will be returned.
#' The syntax to specify knit_version as a knit engine in Rmd files is:
#'
#' \code{knit: templar::knit_versions(<options, excluding orig_file>)}
#'
#' See the example below for more details.
#'
#' @seealso \link{versions}, \link[rmarkdown]{render}, \link[knitr]{knit}, \link[future]{mutliprocess}
#'
#' @examples
#'
#' \dontrun{
#' ---
#' title: "Example"
#' output: html_document
#' knit: templar::knit_versions(knit_global = FALSE)
#' ---
#'
#' ```{r, include=FALSE}
#' knitr::opts_chunk$set(echo = TRUE)
#' ```
#'
#' %%%
#' version: A
#'
#' You are taking **Exam A**
#' %%%
#'
#' %%%
#' version: B
#'
#' You are taking **Exam B**
#' %%%
#'
#' ## Question 1: Means
#'
#' Find the mean of the vector `a`
#'
#' ```{r, version = "A"}
#' set.seed(123)
#' ```
#'
#' ```{r, version = "B"}
#' set.seed(456)
#' ```
#'
#' ```{r, version = "A"}
#' a <- rnorm(10)
#' ```
#'
#' %%%
#' version: A
#'
#' The mean is `r mean(a)`
#' %%%
#'
#' %%%
#' version: none
#'
#' Note to self: make a version C later.
#' %%%
#'
#' }
#'
#'
#' @export
knit_versions <- function(orig_file,
                          global_eval = TRUE,
                          to_knit = NULL,
                          folders = NULL,
                          knit_global = TRUE,
                          solution_with_question = TRUE,
                          .use_jobs = identical(Sys.getenv("RSTUDIO"), "1"),
                          .ncores = ceiling(parallel::detectCores()/2),
                          ...){
  if (missing(orig_file)){
    if (!missing(...))
      warning("Ellipsis arguments are ignored in partial call generator to avoid conflicts with rmarkdown::render.",
              "Please specify those in the evaluated call instead, i.e. when orig_file is present.")
    return(purrr::partial(knit_versions,
                          global_eval = {{global_eval}},
                          to_knit = {{to_knit}},
                          folders = {{folders}},
                          knit_global = {{knit_global}},
                          solution_with_question = {{solution_with_question}},
                          .use_jobs = {{.use_jobs}},
                          .ncores = {{.ncores}}))
  }

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
      message("Found ", crayon::green('versions()'),
              " call within markdown file, pass to ", crayon::green("`rmarkdown::render`"),".\n",
              ">> Knit_version() is not supposed to used with version(),",
              " if you want to use the former, please comment out or delete every ", crayon::green("`templar::versions()`"),
              " call in the markdown file, or use the built-in Knit button or", crayon::green("rmarkdown::render()"), ".\n",
              ">> If you want to use ", crayon::green("`templar::knit_version`"), " as the default knit engine",
              " please add this line at the bottom the yaml part. You can safely remove ", crayon::green('versions()'), " calls.\n\n",
              crayon::bgYellow(crayon::red("knit: templar::knit_versions()\n")))
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

  all_info <- purrr::map_df(to_knit, get_solution_chunks, all_info, solution_with_question) %>%
    dplyr::select(-solution) %>%
    dplyr::group_by(starts, ends) %>%
    dplyr::summarise(dplyr::across(.fns = any, na.rm = TRUE), .groups = "drop")

  to_knit <- setdiff(names(all_info), not_versions)

  if (length(folders) && !is.list(folders))
    folders <- structure(as.list(rep(folders, length(to_knit))), names = to_knit)

  # Write and knit file for each version

  if (.ncores > 1 && !.use_jobs){
    future::plan(future::multisession, workers = .ncores)
    message("- Knitting versioned files.")
  }

  if (.use_jobs) cat(crayon::green("Job created for "), if (knit_global) "(global) ")
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

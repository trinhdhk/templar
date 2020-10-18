#' Knit different versions of a file from chunk and section options
#'
#' \code{versions} is a function that should be included in the setup chunk of
#' an R Markdown document.  Its purpose is to write, then knit, R Markdown source
#' files for several versions of a document, such as different exams in a course.
#'
#'
#' @param global_eval Logical.
#' @param to_knit Character vector specifying which versions to write and knit
#' into separate files.  If not specified, all versions are produced.
#' @param folders List of versions and subfolder to put them in. Use pattern
#' @param solution_with_question Whether to include questions in solutions.
#' \code{version_name = folder_name}.  Default is each version in its own folder.
#'
#' @returns none
#'
#' @details
#'
#' Code chunks may be tagged as version-specific using the option \code{version}.
#' Text sections may also be tagged as version-specific using \code{%%%} wrappers.
#' See example Rmd source below.
#'
#' The version label \code{"solution"} is special,
#' as it is combined with the other version labels to make a solution set.
#'
#' The version label \code{"none"} is special; it will be ignored in the creation
#' of the child documents.  Use it to leave yourself notes in the original document.
#'
#' @examples
#'
#'\dontrun{
#' ---
#' title: "Example"
#' output: html_document
#' ---
#'
#' ```{r, include=FALSE}
#' knitr::opts_chunk$set(echo = TRUE)
#' templar::versions()
#' ```
#'
#' \%\%\%
#' version: A
#'
#' You are taking **Exam A**
#' \%\%\%
#'
#' \%\%\%
#' version: B
#'
#' You are taking **Exam B**
#' \%\%\%
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
#' \%\%\%
#' version: A
#'
#' The mean is `r mean(a)`
#' \%\%\%
#'
#' \%\%\%
#' version: none
#'
#' Note to self: make a version C later.
#' \%\%\%
#'
#' }
#'
#' @export
versions <- function(global_eval = TRUE,
                     to_knit = NULL,
                     folders = NULL,
                     solution_with_question = TRUE) {

  if (!isTRUE(getOption('knitr.in.progress'))){
    return()
  }

  orig_file <- knitr::current_input(dir = TRUE)
  orig_opts <- knitr::opts_chunk$get()
  if (global_eval){
    knitr::opts_chunk$set(eval = TRUE)
  }
  message("Start knitting versioned markdowns.")
  knit_versions(orig_file = orig_file, global_eval = global_eval,
                to_knit = to_knit, folders = folders, knit_global = FALSE,
                solution_with_question = solution_with_question)
  message("Finish knitting versioned markdowns.")
  knitr::opts_chunk$set(orig_opts)
}

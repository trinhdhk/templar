#' Helper function to convert ellipsis arguments to string to add to glue
#' @param ... Ellipsis arguments passed from parent call.
#' @param comma_prefix Whether to add a comma before
#' @return A string
.__dot_to_param_str__ <- function(..., comma_prefix = FALSE){
  dot <- c(...)
  if (!length(dot)) return('')
  dot <- sapply(seq_along(dot), function(i){
    name.x <- names(dot[i])
    if (!length(name.x)) name.x <- ''
    x <- dot[[i]]
    if (nchar(name.x)) return(glue::glue('{name.x} = {x}'))
    as.character(x)
  }, USE.NAMES = FALSE)
  toString(c(if(comma_prefix) ', ', dot))
}

#' Helper function to create knitting script and call RStudio job to run that script.
#' @param file Rmd file to knit
#' @param ... additional parameters passed to rmarkdown::render
#' @return A string containing job's ID.
.__interactive_knit_jobs__ <- function(file, ...){
  # set the link to the knitter file.
  # the script file is placed within the sub-folder of each version.
  temp_script <- file.path(dirname(file), glue::glue('._{basename(file)}._knitter_.R'))
  file_name <- basename(file)

  # due to some unknown reasons, RStudio sometimes fails to create jobs if using Knit button.
  # might be due to the parent process was killed before the job was created.
  # A dummy file will be created as a checkpoint.
  # If this dummy file doesn't exist, function will wait for .5 second and then recheck.
  # If dummy is created, the job is supposedly running just fine.
  dummy <- file.path(dirname(file), glue::glue('._{basename(file)}._dummy_'))

  ell <- .__dot_to_param_str__(..., comma_prefix = TRUE)

  # Write everything to the script
  writeLines(c(
    glue::glue("# Creating dummy file ------------"),
    glue::glue("writeLines('DUMMY FILE FOR {file_name}', con = '{dummy}')"), # creation of dummy
    glue::glue("# Rendering {file_name} -------------"),
    glue::glue("rmarkdown::render('{file}'{ell})"),
    "# Cleaning up -----------",
    glue::glue("file.remove('{temp_script}')")),
    con = temp_script)
  m <- rstudioapi::callFun("runScriptJob", path = temp_script, name = file_name, encoding = "UTF-8",
                      workingDir = dirname(file), exportEnv = parent.frame())
  while (!file.exists(dummy)) Sys.sleep(.5) # This loop only exits if dummy file is created aka the job is running
  file.remove(dummy) # Remove this dummy when the jobs is running already.
  m
}


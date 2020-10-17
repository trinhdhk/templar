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

.__interactive_knit_jobs__ <- function(file, ...){
  temp_script <- file.path(dirname(file), glue::glue('._{basename(file)}._knitter_.R'))
  dummy <- file.path(dirname(file), glue::glue('._{basename(file)}._dummy_'))
  file_name <- basename(file)
  ell <- .__dot_to_param_str__(..., comma_prefix = TRUE)
  writeLines(c(
    glue::glue("file.create('{dummy}')"),
    glue::glue("# Rendering {file_name} -------------"),
    glue::glue("rmarkdown::render('{file}'{ell})"),
    "# Cleaning up -----------",
    glue::glue("file.remove('{temp_script}')")),
    con = temp_script)
  # rstudioapi::jobRunScript(temp_script, name = file_name, encoding = "UTF-8")
  m <- rstudioapi::callFun("runScriptJob", path = temp_script, name = file_name, encoding = "UTF-8",
                      workingDir = dirname(file), exportEnv = parent.frame())
  while (!file.exists(dummy)) Sys.sleep(1)
  file.remove(dummy)
  m
}


.__dot_to_param_str__ <- function(..., comma_prefix = FALSE){
  dot <- c(...)
  dot <- sapply(seq_along(dot), function(i){
    name.x <- names(dot[i])
    x <- dot[[i]]
    if (nchar(name.x)) return(glue::glue('{name.x} = {x}'))
    as.character(x)
  }, USE.NAMES = FALSE)
  toString(c(if(comma_prefix) '', dot))
}

.__interactive_knit_jobs__ <- function(file,..., suppressTemplar = TRUE){
  temp_script <- tempfile()
  file_name <- basename(file)
  ell <- .__dot_to_param_str__(...)
  writeLines(c(
    glue::glue("# Rendering {file_name} -------------"),
    glue::glue("rmarkdown::render('{file}', envir = new.env() {ell})")),
    con = temp_script)
  # rstudioapi::jobRunScript(temp_script, name = file_name, encoding = "UTF-8")
  rstudioapi::callFun("runScriptJob", path = temp_script, name = file_name, encoding = "UTF-8",
                      workingDir = dirname(file))
  TRUE
}


################################################################################
# to create project directory
# by: Lam PK
# version: 23 July 2015
################################################################################

createProject <- function(dir, structure = "default"){
  ## to create project directory
  ## initial version: 23 July 2015
  dir.create(dir, recursive = TRUE)
  # template
    template <- switch(structure,
                       default = file.path(system.file(package = "Lmisc"), "structure", "default"))

  # copy folder structure
  file.copy(from = dir(template, full.names = TRUE), to = dir, recursive = TRUE)
  # inform
  cat(paste("Project", basename(dir), "was created in", dirname(dir), "\n", sep = " "))
}


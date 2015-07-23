################################################################################
# to create project directory
# by: Lam PK
# version: 23 July 2015
################################################################################

createProject <- function(dir, structure = "default"){
  ## to create project directory
  ## initial version: 23 July 2015

  ## read project template
  template <- switch(structure,
                     default = file.path(system.file(package = "Lmisc"), "structure", "default.txt"))
  .str <- readLines(template)

  ## create folder structure
  sapply(.str, FUN= function(x){dir.create(path = file.path(dir, x), recursive = TRUE)})

  # inform
  cat(paste("Project", basename(dir), "was created in", dirname(dir), "\n", sep = " "))
}


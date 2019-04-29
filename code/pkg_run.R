pkg_run = function(pkgs, inst_verbose = TRUE, load_verbose = TRUE) {
  # Loads required R libraries (and minimum versions), installing/updating them first 
  #   (in parallel) as needed.
  #
  # Args:
  #   pkgs: A character list of R packages. Package version can be supplied as a second element 
  #      in a vector.
  #   inst_verbose: If TRUE, prints installation progress. Default is TRUE.
  #   load_verbose: If TRUE, prints loading progress. Default is TRUE.
  #
  # Returns:
  #   None.
  options(Ncpus = parallel::detectCores() - 2)
  
  lapply(pkgs, function (x) {
    
    if(
       !is.element(x[1], installed.packages()[,1]) ||                           
       (length(x) > 1 && (packageVersion(x[1]) < x[2]))
       ) {
  
          install.packages(x[1], quiet=!inst_verbose)
      }
    
    if (load_verbose) {require(x[1], character.only = TRUE)} else {
      
      suppressMessages(require(x[1], character.only = TRUE))
      
    }
    
  })
  
  return(invisible())
  
}

packages_needed <- c("tidyverse","shiny","shinyFiles",
                     "shinycssloaders","DT","dr4pl","dplyr")
if (length(setdiff(packages_needed, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages_needed, rownames(installed.packages())))  
}

shiny::runGitHub('pad4pl','cgrilson7')
################################################################################
# Database Password Function
################################################################################
# The set_db_password function is designed to reduce the effort required
# to link to my secure database.

# Load required package(s)

#' A Database Access Function
#'
#' Set up access to my secure database, for a variety of computers.
#' @param
#' @return
#' @export
#' @examples

set_db_password <- function(){
  set_machine <- readline(prompt = "Is your machine the Linux laptop or Windows PC? Enter '1' for the laptop and '2' for the PC:")
  ifelse(
    set_machine == 1,
    # Linux laptop
    dbpsswddir <- "/home/a_friend/data_analysis/database/",
    # Windows PC
    dbpsswddir <- "C:/b_Data_Analysis/Database/"
  )   # End 'ifelse' statement
  print(paste("You have set the location of the database password file as: ", dbpsswddir, sep = ""))
  return(dbpsswddir)
}   # End set_db_password function

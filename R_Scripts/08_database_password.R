


set_db_password <- function(){
  set_machine <- readline(prompt = "Is your machine the Linux laptop or Windows PC? Enter '1' for the laptop and '2' for the PC:")
  ifelse(
    set_machine == 1,
    # Linux laptop
    dbpsswddir <- "/home/a_friend/data_analysis/database/",
    # Windows PC
    dbpsswddir <- "C:/b_Data_Analysis/Projects/TDF_Predict/Data_Files"
  )   # End 'ifelse' statement
  print(paste("You have set the location of the database password file as: ", dbpsswddir, sep = ""))
  return(dbpsswddir)
}   # End set_db_password function

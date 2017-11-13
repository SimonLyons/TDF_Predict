# ## FIVE ##
# Take two lists of rider names and determine the positions of the best match.
# Returns no match ('NA') where there is no good match or there are duplicates.
levelTwoListMatch <- function(search_name_list, input_name_list){
  # Create a dataframe to retain the Levenshtein result and best match name
  name_match_table <- as.data.frame(matrix(data = NA, nrow = length(search_name_list), ncol = 4))
  # class(name_match_table$search_name) <- "factor"
  colnames(name_match_table) <- c("search_name", "input_match", "max_pos", "leven_result")
  search_name_list <- as.character(search_name_list)   # Convert name list to string/character list
  
  for(k in 1:length(search_name_list)){
    # Calculate the Levenshtein value for the search name against each name in the input_list
    levSim_each_name <- levenNameAgainstNameList(search_name_list[[k]], input_name_list)
    name_match_table$search_name[k] <- search_name_list[k]
    max_pos <- match(max(levSim_each_name), levSim_each_name)
    name_match_table$max_pos[[k]] <- max_pos
    name_match_table$input_match[[k]] <- stri_trim_both(input_name_list[max_pos])
    name_match_table$leven_result[[k]] <- max(levSim_each_name)
  }   # End FOR loop running through search name list
  
  name_match_table$search_name <- search_name_list
  levSim_each_name <- lapply(search_name_list, levenNameAgainstNameList, input_name_list)
  max_pos <- match(lapply(levSim_each_name, max), levSim_each_name)
  length(max_pos[[1]])
  ghjk <- lapply(levSim_each_name, max)
  
  
  match(lapply(levSim_each_name, max)[[1]], levSim_each_name[[1]])
  
  lapply(levSim_each_name, match , lapply(levSim_each_name, max))
  
  ghjk[[1]] %in% levSim_each_name[[1]]
  match(ghjk[[1]], levSim_each_name[[1]])
  

    pos_Match <- function(max_lev_value, lev_values_for_a_name){
    match(max_lev_value, lev_values_for_a_name)
    
    
  }
  
  asdff <- lapply(levSim_each_name, %in%, ghjk)
  ?match
  max(asdff)
  zxcv <- match(ghjk[1], asdff[[1]])
  input_name_list[zxcv]
  
  max_pos <- lapply(levSim_each_name, match, lapply(levSim_each_name, max))
  max_pos[[2]]
  input_match
  input_name_list[max_pos[[1]]]
  search_name_list[max_pos[[1]]]
  length(max_pos[[1]])
  levenResultTable <- function(search_name, input_name_list){
    
    
    
    
  }
  
  
  # Create a new column keeping only the best Levenshtein result for each of the matched riders
  # and therefore filtering out (leaving 'NA') the duplicates
  name_match_table <- name_match_table %>% group_by(input_match) %>% 
    mutate(best_pos = case_when(input_match == input_match & leven_result == max(leven_result) & leven_result > 0.7 ~ max_pos)) %>% 
    ungroup()
  
  # Retun only the vector of the best matching positions
  return(name_match_table$best_pos)
}   # End 'levelTwoListMatch' function

get_me_effects <- function(model, 
                        term, 
                        grouping_var, 
                        grouping_var_lv1, 
                        grouping_var_lv2){
  
  effects_data <- effect(term = term,
                         xlevels = list(grouping_var = c(grouping_var_lv1, grouping_var_lv2)),
                         mod = model)

  effects_data <- as.data.frame(effects_data)
  return(effects_data)
}

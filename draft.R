source("function_setup.R")

setup_results <- setup()

levels <- setup_results[[1]]
main_bus <- setup_results[[2]]
choices_machines <- setup_results[[3]]
choices_recipes <- setup_results[[4]]
recipes <- setup_results[[5]]

rm(setup_results)


#create a function that iterates on levels and produces the number of machines and inputs necessary
plan_project <- function(recipe,number,usebus="Y"){
  
  #start by iterating downward and making a to do list
  for (i in max(levels_all$level):1){
    
  }
}

#then go upward and get info from big table
#output an in/out table then update the main bus table
#create an excel table of level recipes to add to big table (uranium mining may be an issue)

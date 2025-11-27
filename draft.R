source("function_setup.R")
source("function_change.R")

setup_results <- setup()

main_bus <- setup_results[[1]]
choices_machines <- setup_results[[2]]
choices_recipes <- setup_results[[3]]
recipes <- setup_results[[4]]

rm(setup_results)

#create a function that iterates on levels and produces the number of machines and inputs necessary
plan_project <- function(data_input, data_recipes, data_machines, product, quantity, usebus="Y"){
  
  #reduce the recipe rows by joining with the two choices datasets
  data_recipes <- data_recipes %>%
    filter(preferred == 1)
  
  data_all <- data_input %>%
    right_join(data_recipes, by = c("product_name","name"))
  
  level_max <- data_all %>%
    filter(product_name == product) %>%
    select(product_name, recipe_level) %>%
    distinct(product_name, recipe_level) %>%
    pull(recipe_level)
  
  #start by iterating downward and making a to do list
  return(recursive_task_table_gpt(data_all, product, quantity, level_max))
  # to_do_list <- data.frame()
  # 
  # interested <- c(product)
  # needed <- c(quantity)
  # 
  # for (i in level_max:1){
  #   
  #   rows_want <- data_all %>%
  #     filter(recipe_level == i & product_name %in% interested) %>%
  #     mutate(no_machines = NEED TO FIND A BETTER WAY FOR NEEDED AND INTERESTED, maybe iterate on the number of rows of the previous step, could be a function)
  #   
  # }
}

#test the function
test <- plan_project(recipes, choices_recipes, choices_machines, "engine-unit", 8, usebus="Y")

#then go upward and get info from big table
#output an in/out table then update the main bus table
#create an excel table of level recipes to add to big table (uranium mining may be an issue)

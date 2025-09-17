source("function_setup.R")
source("function_change.R")

setup_results <- setup()

main_bus <- setup_results[[1]]
choices_machines <- setup_results[[2]]
choices_recipes <- setup_results[[3]]
recipes <- setup_results[[4]]

rm(setup_results)

#create a function that iterates on levels and produces the number of machines and inputs necessary
plan_project <- function(data, data_recipes, data_machines, product, quantity, usebus="Y"){
  
  #reduce the recipe rows by joining with the two choices datasets
  data_recipes <- data_recipes %>%
    filter(preferred == 1)
  
  data_machines <- data_machines %>%
    filter(accessible == 1)
  
  data_all <- data %>%
    full_join(data_recipes, by = c("product_name","name")) %>%
    full_join(data_machines, by = "machine_name")
  
  level_max <- data_all %>%
    filter(product_name == product) %>%
    select(product_name, recipe_level) %>%
    distinct(product_name) %>%
    pull(recipe_level)
  
  #start by iterating downward and making a to do list
  to_do_list <- data.frame()
  
  interested <- c(product)
  needed <- c(quantity)
  
  for (i in level_max:1){
    
    rows_want <- data_all %>%
      filter(recipe_level == i & product_name %in% interested) %>%
      mutate(no_machines = NEED TO FIND A BETTER WAY FOR NEEDED AND INTERESTED)
    
  }
}

#then go upward and get info from big table
#output an in/out table then update the main bus table
#create an excel table of level recipes to add to big table (uranium mining may be an issue)

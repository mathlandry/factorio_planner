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
    right_join(data_recipes, by = c("product_name","name")) %>%
    distinct(name, product_name, ing_name, .keep_all = TRUE)
  
  level_max <- data_all %>%
    filter(product_name == product) %>%
    select(product_name, recipe_level) %>%
    distinct(product_name, recipe_level) %>%
    pull(recipe_level)
  
  #start by iterating downward and making a to do list
  to_do <- recursive_task_table(data_all, product, quantity) %>%
    mutate(
      ingredient_hierarchy = paste0(strrep("-", 4 * recurse_level), ingredient_name)
    )
  
  to_do_simple <- to_do %>%
    group_by(ingredient_name,level) %>%
    summarise(quantity_needed = sum(quantity_needed, na.rm = TRUE),
        .groups = "drop") %>%
    arrange(desc(level))
  
  
  return(list(to_do,to_do_simple))
  # to_do_list <- data.frame()
  # 
  # interested <- c(product)
  # needed <- c(quantity)
  # 

}

#test the function
test <- plan_project(recipes, choices_recipes, choices_machines, "bulk-inserter", 0.5, usebus="Y")
test1 <- test[[1]]
test2 <- test[[2]]
#then go upward and get info from big table
#output an in/out table then update the main bus table
#create an excel table of level recipes to add to big table (uranium mining may be an issue)

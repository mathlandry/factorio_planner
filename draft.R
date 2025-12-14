source("function_change.R")
source("function_setup.R")


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
  
  data_pre_curated <- data_input %>%
    right_join(data_recipes, by = c("product_name","name")) 
  
  data_curated <- data_pre_curated %>%
    distinct(name, product_name, ing_name, .keep_all = TRUE)
  
  level_max <- data_curated %>%
    filter(product_name == product) %>%
    select(product_name, recipe_level) %>%
    distinct(product_name, recipe_level) %>%
    pull(recipe_level)
  
  #start by iterating downward and making a to do list
  to_do <- recursive_task_table(data_curated, product, quantity) %>%
    mutate(
      ingredient_hierarchy = paste0(strrep("-", 4 * recurse_level), ingredient_name)
    )
  
  to_do_simple <- to_do %>%
    group_by(ingredient_name,level) %>%
    summarise(quantity_needed = sum(quantity_needed, na.rm = TRUE),
        .groups = "drop") %>%
    arrange(desc(level))
  
  plan_simple <- to_do_simple %>%
    mutate(desc_col = n():1) %>%
    left_join(data_recipes, by = c("ingredient_name" = "product_name")) %>%
    select(ingredient_name, level, quantity_needed, desc_col, name) %>%
    left_join(data_pre_curated, by = c("name")) %>%
    select(ingredient_name, level, quantity_needed, desc_col, name, recipe_no, product_no, product_name, product_amount, recipe_time, machine_name, crafting_speed, production_per_machine) %>%
    distinct() %>%
    mutate(
      number_machines = quantity_needed / production_per_machine
    )
  
  plan_detailed <- to_do %>%
    mutate(desc_col = n():1) %>%
    left_join(data_recipes, by = c("ingredient_name" = "product_name")) %>%
    select(ingredient_name, level, quantity_needed, recurse_level, ingredient_no, ingredient_hierarchy, desc_col, name) %>%
    left_join(data_pre_curated, by = c("name"), relationship = "many-to-many") %>%
    select(ingredient_name, level, quantity_needed, recurse_level, ingredient_no, ingredient_hierarchy, desc_col, name, recipe_no, product_no, product_name, product_amount, recipe_time, machine_name, crafting_speed, production_per_machine) %>%
    distinct() %>%
    mutate(ID = paste0(name,desc_col)) %>%
    mutate(
      number_machines = quantity_needed / production_per_machine
    )
      
  
  #iterate on recipes in plan_simple then choose with select_machines()
  plan_simple_final <- plan_simple
  
  for (recipe in unique(plan_simple$name)) {
    needed <- plan_simple %>%
      filter(name == recipe) %>%
      pull(product_amount) %>%
      first()
    chosen <- select_machine(plan_simple_final, data_machines, recipe, needed)
    plan_simple_final <- plan_simple_final %>% 
      filter(
        machine_name == chosen | name != recipe
      )
  }
  
  #do the same for the detailed plan
  plan_detailed_final <- plan_detailed
  
  for (ident in unique(plan_detailed$ID)) {
    rows <- plan_detailed_final %>% filter(ID == ident)
    needed <- rows %>%
      filter(ID == ident) %>%
      pull(product_amount) %>%
      first()
    recipe <- rows %>%
      filter(ID == ident) %>%
      pull(name) %>%
      first()
    chosen <- select_machine(rows, data_machines, recipe, needed)
    plan_detailed_final <- plan_detailed_final %>% 
      filter(
        machine_name == chosen | ID != ident
      ) %>%
      mutate(
        number_machines = quantity_needed / production_per_machine
      )
  }
  
  return(list(to_do, to_do_simple, plan_detailed_final, plan_simple_final))

  #output an in/out table then update the main bus table
  #implement the bus option
  #implement a parameter to prioritize number of machines over whole numbers
}

#test the function
test <- plan_project(recipes, choices_recipes, choices_machines, "bulk-inserter", 14, usebus="Y")
test1 <- test[[1]]
test2 <- test[[2]]
test3 <- test[[3]]
test4 <- test[[4]]
#create an excel table of level recipes to add to big table (uranium mining may be an issue)


select_machine(recipes, choices_machines, "inserter", 9)

change_recipe <- function(data, product, recipe) {
  input <- data
  
  output <- input %>%
    mutate(preferred = case_when(
      product_name == product & name == recipe ~ 1,
      product_name == product ~ 0,
      TRUE ~ preferred
    ))
  
  return(output)
}

change_machine <- function(data, machine, value = 1) {
  input <- data
  
  output <- input %>%
    mutate(accessible = case_when(
      machine_name == machine ~ value,
      TRUE ~ accessible
    ))
  
  return(output)
}

update_bus <- function(data, item_step, prod_step, cons_step) {
  input <- data
  
  output <- input %>%
    mutate(
      produced = case_when(
        item == item_step ~ case_when(
          is.na(produced) ~ prod_step,
          TRUE ~ produced + prod_step
        ),
        TRUE ~ produced
      ),
      consumed = case_when(
        item == item_step ~ case_when(
          is.na(consumed) ~ cons_step,
          TRUE ~ consumed + cons_step
        ),
        TRUE ~ consumed
      ),
      available = case_when(
        item == item_step ~ produced - consumed,
        TRUE ~ available
      )
    )
  
  return(output)
}

select_machine <- function(data, machines, recipe, production_goal) {
  input <- data %>% 
    filter(name == recipe) %>% 
    select(name, machine_name, crafting_speed) %>%
    distinct()
  
  machines_dat <- machines %>% 
    filter(accessible == 1) %>% 
    select(machine_name)
  
  input_machines <- machines_dat %>% 
    left_join(machines_dat, by = "machine_name") %>%
    mutate(
      number_required = production_goal / crafting_speed,
      whole = case_when(
        production_goal %% crafting_speed > 0 ~ 0,
        TRUE ~ 1
      )
    ) %>%
    filter(whole == max(whole)) %>%
    filter(crafting_speed == max(crafting_speed))
  
  return(input_machines$machine_name)
}

#function that takes in data_all, product_name and quantity
recursive_task_table <- function(data, product, quantity){
  
  recipe_dat <- data %>%
    filter(product_name == product) %>%
    mutate(
      machines_needed = quantity / production_per_machines,
      sub_needed = machines_needed * ing_amount
    )
  
  current_level <- recipe_dat %>%
    select(recipe_level) %>%
    distinct() %>%
    pull(recipe_level)
  
  for (i in 1:dim(recipe_dat)[1]){
    
    temp_dat <- recipe_dat[i,]
    product_temp <- recipe_dat$ing_name
    quantity_temp <- recipe_dat$sub_needed
    
    recursive_task_table(data, product_temp, quantity_temp)
  }
}

#gpt suggestion
recursive_task_table <- function(data, product, quantity, level = 0) {
  # Step 1: Get the recipe rows for the product
  recipe_dat <- data %>%
    filter(product_name == product) %>%
    mutate(
      machines_needed = quantity / production_per_machines,
      sub_needed = machines_needed * ing_amount,
      target_product = product,
      required_quantity = quantity,
      recipe_level = level
    )
  
  # Step 2: Initialize result with current recipe
  result <- recipe_dat
  
  # Step 3: Recurse for each ingredient
  for (i in seq_len(nrow(recipe_dat))) {
    ing <- recipe_dat$ing_name[i]
    sub_qty <- recipe_dat$sub_needed[i]
    
    # Check if this ingredient is itself a product (i.e., has a recipe)
    if (ing %in% data$product_name) {
      sub_result <- recursive_task_table(data, ing, sub_qty, level + 1)
      result <- bind_rows(result, sub_result)
    }
  }
  
  return(result)
}
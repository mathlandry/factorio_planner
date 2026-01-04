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
    select(name, machine_name, production_per_machine) %>%
    distinct(machine_name, .keep_all = TRUE)
  
  stopifnot(length(unique(input$machine_name)) == nrow(input))
  
  machines_dat <- machines %>% 
    filter(accessible == 1) %>% 
    select(machine_name) %>%
    distinct()
  
  input_machines <- input %>% 
    left_join(machines_dat, by = "machine_name") %>%
    mutate(
      number_required = production_goal / production_per_machine,
      whole = case_when(
        production_goal %% production_per_machine > 0 ~ 0,
        TRUE ~ 1
      ),
      electric = case_when(
        grepl("electric", machine_name) ~ 1,
        TRUE ~ 0
      )
    ) %>%
    arrange(desc(whole), number_required, desc(electric))

  return(input_machines$machine_name[1])
}

#function that takes in data_all, product_name and quantity
recursive_task_table <- function(data, product, quantity, output = data.frame(
  ingredient_name = character(),
  level = integer(),
  quantity_needed = numeric(),
  recurse_level = integer(),
  ingredient_no = integer()
    ),
  recurseLevel = 0,
  ingredientNo = 0
  ) {
  
  # Get the recipe rows for the product
  recipe_dat <- data %>%
    filter(product_name == product) %>%
    mutate(
      sub_needed = quantity * ing_amount / product_amount,
    )
  
  new_row <- data.frame(
    ingredient_name = product,
    level = recipe_dat$product_level[1],
    quantity_needed = quantity,
    recurse_level = recurseLevel,
    ingredient_no = ingredientNo
    
  )
  
  output <- rbind(output, new_row)
  
  #base case
  if (recipe_dat$product_level[1] == 0) {
    return(output)
  }
  
  #iterate on ingredients
  else {
    for (i in seq_len(nrow(recipe_dat))) {
      
      ing <- recipe_dat$ing_name[[i]]
      prod_level <- recipe_dat$ingredient_level[[i]]
      sub_qty <- recipe_dat$sub_needed[i]
      
      output <- recursive_task_table(data, ing, sub_qty, output, recurseLevel = recurseLevel + 1, ingredientNo = i)
    
    }
  }
  
  output
}

#create a function that iterates on levels and produces the number of machines and inputs necessary
plan_project <- function(data_input = recipes, data_recipes = choices_recipes, data_machines = choices_machines, data_inout = consumed_produced, data_main_bus = main_bus, product, quantity, usebus="N"){
  
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
  
  if (usebus == "Y"){
    to_do <- to_do %>%
      filter(recurse_level < 1)
  }
  
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
      number_machines = if_else(
        product_name == ingredient_name,
        quantity_needed / production_per_machine, NA
      ))
  
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
        number_machines = if_else(
          product_name == ingredient_name,
          quantity_needed / production_per_machine, NA
        ))
  }
  
  #get the total ins and outs from the simple plan
  planned_consumed_produced <- plan_simple_final %>%
    filter(!is.na(number_machines)) %>%
    left_join(data_inout, by = c("name","recipe_no")) %>%
    mutate(
      consumed_sum = round(number_machines*consumed*crafting_speed/recipe_time,1),
      produced_sum = round(number_machines*produced*crafting_speed/recipe_time,1),
      diff = coalesce(produced_sum,0) - coalesce(consumed_sum,0)
    ) %>%
    select(item, consumed_sum, produced_sum, diff) %>%
    group_by(item) %>%
    summarise(consumed_sum = sum(consumed_sum, na.rm = TRUE),
              produced_sum = sum(produced_sum, na.rm = TRUE),
              diff = sum(diff, na.rm = TRUE),
              .groups = "drop")
  
  #update the main bus
  changes <- planned_consumed_produced %>%
    filter(diff != 0) %>%
    select(item, diff)
  
  main_bus <- main_bus %>%
    left_join(changes, by = "item") %>%
    mutate(
      produced = if_else(
        diff > 0,
        coalesce(produced, 0) + diff, produced
      ),
      consumed = if_else(
        diff < 0,
        coalesce(consumed, 0) - diff, consumed
      ),
      available = if_else(
        diff != 0,
        coalesce(produced, 0) - coalesce(consumed, 0), available
      )
    ) %>%
    arrange(desc(available)) %>%
    select(-diff)
  
  return(list(to_do, to_do_simple, plan_detailed_final, plan_simple_final, planned_consumed_produced, main_bus))
  
  #implement a parameter to prioritize number of machines over whole numbers
}
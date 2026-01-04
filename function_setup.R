setup <- function() {
  
  library("rjson")
  library(dplyr)
  library(readxl)
  
  # Load and parse JSON
  json_file <- "C:/Users/Utilisateur/AppData/Roaming/Factorio/script-output/data-raw-dump.json"
  json_data <- fromJSON(file = json_file)
  
  recipes <- json_data[["recipe"]]
  
  filtered_recipes <- Filter(
    function(x) {
      "ingredients" %in% names(x) &&
        is.list(x$ingredients) &&
        length(x$ingredients) > 0
    },
    recipes
  )
  
  machines <- c(
    json_data[["assembling-machine"]],
    json_data[["furnace"]]
  )
  
  # Ingredients loop
  ingredients_dat <- data.frame()
  
  for (i in 1:length(filtered_recipes)) {
    
    rec_temp <- data.frame()
    recipe_name <- names(filtered_recipes[i])
    
    for (j in 1:length(filtered_recipes[[i]][["ingredients"]])) {
      
      ing_temp <- data.frame()
      ing_temp[1, "name"] <- recipe_name
      
      ing_temp <- mutate(
        ing_temp,
        ing_no     = j,
        ing_name   = filtered_recipes[[i]][["ingredients"]][[j]][["name"]],
        ing_amount = as.numeric(
          filtered_recipes[[i]][["ingredients"]][[j]][["amount"]]
        ),
        ing_type   = filtered_recipes[[i]][["ingredients"]][[j]][["type"]]
      )
      
      rec_temp <- rbind(rec_temp, ing_temp)
    }
    
    ingredients_dat <- rbind(ingredients_dat, rec_temp)
  }
  
  rm(i, j, recipe_name, ing_temp, rec_temp)
  
  # Results loop
  results_dat <- data.frame()
  
  for (i in 1:length(filtered_recipes)) {
    
    rec_temp <- data.frame()
    recipe_name <- names(filtered_recipes[i])
    
    for (j in 1:length(filtered_recipes[[i]][["results"]])) {
      
      res_temp <- data.frame()
      res_temp[1, "name"] <- recipe_name
      res_temp[1, "recipe_no"] <- i
      
      res_temp[1, "recipe_type"] <- if ("type" %in% names(filtered_recipes[[i]])) {
        filtered_recipes[[i]][["type"]]
      } else {
        ""
      }
      
      res_temp[1, "recipe_category"] <- if ("category" %in% names(filtered_recipes[[i]])) {
        filtered_recipes[[i]][["category"]]
      } else {
        "crafting"
      }
      
      res_temp[1, "recipe_time"] <- if ("energy_required" %in% names(filtered_recipes[[i]])) {
        filtered_recipes[[i]][["energy_required"]]
      } else {
        0.5
      }
      
      res_temp[1, "ByproductsFL"] <- if (
        length(filtered_recipes[[i]][["results"]]) > 1
      ) {
        "Y"
      } else {
        "N"
      }
      
      res_temp <- mutate(
        res_temp,
        product_no     = j,
        product_name   = filtered_recipes[[i]][["results"]][[j]][["name"]],
        product_amount = as.numeric(
          filtered_recipes[[i]][["results"]][[j]][["amount"]]
        ),
        product_type   = filtered_recipes[[i]][["results"]][[j]][["type"]]
      )
      
      rec_temp <- rbind(rec_temp, res_temp)
    }
    
    results_dat <- rbind(results_dat, rec_temp)
  }
  
  rm(i, j, recipe_name, res_temp, rec_temp)
  
  # Machines loop
  machines_dat <- data.frame()
  
  for (i in 1:length(machines)) {
    
    mac_temp <- data.frame()
    machine_name <- names(machines[i])
    
    for (j in 1:length(machines[[i]][["crafting_categories"]])) {
      
      cra_temp <- data.frame()
      cra_temp[1, "machine_name"] <- machine_name
      cra_temp[1, "recipe_category"] <- machines[[i]][["crafting_categories"]][[j]]
      cra_temp[1, "crafting_speed"] <- machines[[i]][["crafting_speed"]]
      
      mac_temp <- rbind(mac_temp, cra_temp)
    }
    
    machines_dat <- rbind(machines_dat, mac_temp)
  }
  
  rm(i, j, machine_name, cra_temp, mac_temp)
  
  # Combine all data
  all_dat <- full_join(
    results_dat,
    ingredients_dat,
    by = "name",
    relationship = "many-to-many"
  ) %>%
    left_join(
      machines_dat,
      by = "recipe_category",
      relationship = "many-to-many"
    ) %>%
    arrange(
      recipe_type,
      recipe_category,
      recipe_no,
      product_no,
      machine_name,
      ing_no
    )
  
  all_dat_levels <- filter(all_dat, recipe_category != "recycling") %>%
    mutate(
      machine_name   = if_else(name == "rocket-part", "rocket-silo", machine_name),
      crafting_speed = if_else(name == "rocket-part", 1, crafting_speed)
    )
  
  # Add custom-made level 0 recipes
  basic <- read_excel("base_resources.xlsx")
  
  all_dat_levels <- all_dat_levels %>%
    mutate(
      ingredient_level = NA,
      product_level    = NA,
      recipe_level     = NA
    ) %>%
    rbind(basic)
  
  # Level calculation loop
  levels_loop <- list()
  all_dat_levels_loop <- all_dat_levels
  n <- 0
  
  while (any(is.na(all_dat_levels_loop$recipe_level)) & n < 10) {
    
    levels_loop[[n + 1]] <- filter(
      all_dat_levels_loop,
      product_level == n &
        (name != "scrap-recycling" | product_name == "holmium-ore")
    ) %>%
      mutate(
        item  = product_name,
        level = product_level
      ) %>%
      select(item, level) %>%
      distinct() %>%
      as.data.frame()
    
    if (nrow(levels_loop[[n + 1]]) == 0) {
      warning(paste0(
        "No items found at product_level == ", n,
        ". Stopping level resolution early."
      ))
      break
    }
    
    all_dat_levels_loop <- left_join(
      all_dat_levels_loop,
      levels_loop[[n + 1]],
      by = c("ing_name" = "item")
    ) %>%
      mutate(
        ingredient_level = if_else(is.na(ingredient_level), level, ingredient_level)
      ) %>%
      select(-level) %>%
      left_join(
        levels_loop[[n + 1]],
        by = c("product_name" = "item")
      ) %>%
      mutate(
        product_level = if_else(is.na(product_level), level, product_level)
      ) %>%
      select(-level) %>%
      group_by(name) %>%
      mutate(
        recipe_level = if (any(recipe_no > 1000)) {
          recipe_level
        } else if (any(is.na(ingredient_level))) {
          NA_real_
        } else {
          max(ingredient_level) + 1
        }
      ) %>%
      ungroup() %>%
      mutate(
        production_per_machine =
          if_else(recipe_time <= 0, NA_real_, crafting_speed * product_amount / recipe_time)
      )
    
    all_dat_levels_loop <- mutate(
      all_dat_levels_loop,
      product_level = if_else(is.na(product_level), recipe_level, product_level)
    )
    
    n <- n + 1
  }
  
  rm(n)
  
  final_dat <- all_dat_levels_loop
  
  # Compile all level items
  levels_all <- bind_rows(levels_loop) %>%
    distinct() %>%
    arrange(level)
  
  dup_levels <- levels_all[
    duplicated(levels_all$item) |
      duplicated(levels_all$item, fromLast = TRUE),
  ]
  
  # Check problematic recipes
  problem_recipes <- all_dat_levels_loop %>%
    filter(!is.na(recipe_level), !is.na(ingredient_level)) %>%
    filter(ingredient_level > recipe_level)
  
  if (nrow(problem_recipes) > 0) {
    print("⚠️ Problematic recipes found:")
    print(problem_recipes)
  }
  
  # Unresolved recipes
  unresolved <- all_dat_levels_loop %>%
    filter(recipe_no < 1000) %>%
    filter(is.na(recipe_level)) %>%
    distinct()
  
  if (nrow(unresolved) > 0) {
    cat("⚠️ There are", nrow(unresolved), "recipes still unresolved")
    print(unresolved)
  }
  
  # Identify missing ingredients
  unresolved_ings <- all_dat_levels_loop %>%
    filter(!ing_name %in% product_name) %>%
    filter(is.na(recipe_level) & recipe_no < 1000) %>%
    select(ing_name) %>%
    distinct()
  
  if (nrow(unresolved_ings) > 0) {
    cat("⚠️ There are", nrow(unresolved_ings), "ingredients still unresolved")
    print(unresolved_ings)
  }
  
  # Initialize a main bus dataframe
  main_bus <- levels_all %>%
    mutate(
      produced  = NA,
      consumed  = NA,
      available = NA
    )
  
  # Create choice sets
  choices_machines <- all_dat_levels_loop %>%
    select(machine_name, recipe_category, crafting_speed) %>%
    distinct() %>%
    arrange(machine_name, recipe_category, crafting_speed) %>%
    mutate(
      accessible = if_else(
        machine_name %in% c(
          "assembling-machine-1",
          "assembling-machine-2",
          "centrifuge",
          "chemical-plant",
          "crusher",
          "electric-furnace",
          "oil-refinery",
          "rocket-silo",
          "steel-furnace",
          "stone-furnace",
          "asteroid-collector",
          "electric-mining-drill",
          "nuclear-reactor",
          "offshore-pump",
          "pumpjack"
        ),
        1,
        0
      )
    )
  
  choices_recipes <- all_dat_levels_loop %>%
    select(product_name, name) %>%
    distinct() %>%
    arrange(product_name, name) %>%
    group_by(product_name) %>%
    mutate(choices = n()) %>%
    ungroup() %>%
    mutate(
      preferred = if_else(product_name == name | choices == 1, 1, 0)
    ) %>%
    #add exceptions
    change_recipe("carbonic-asteroid-chunk", "carbonic-asteroid-crushing") %>%
    change_recipe("fluoroketone-cold", "fluoroketone-cooling") %>%
    change_recipe("fluoroketone-hot", "fluoroketone") %>%
    change_recipe("heavy-oil", "advanced-oil-processing") %>%
    change_recipe("ice", "oxide-asteroid-crushing") %>%
    change_recipe("light-oil", "advanced-oil-processing") %>%
    change_recipe("metallic-asteroid-chunk", "metallic-asteroid-crushing") %>%
    change_recipe("nutrients", "nutrients-from-fish") %>%
    change_recipe("oxide-asteroid-chunk", "oxide-asteroid-crushing") %>%
    change_recipe("petroleum-gas", "advanced-oil-processing") %>%
    change_recipe("raw-fish", "fish-breeding") %>%
    change_recipe("solid-fuel", "solid-fuel-from-petroleum-gas") %>%
    change_recipe("spoilage", "iron-bacteria") %>%
    change_recipe("uranium-235", "kovarex-enrichment-process") %>%
    change_recipe("uranium-238", "kovarex-enrichment-process")
  
  choices_recipes_dup <- choices_recipes %>%
    group_by(product_name, preferred) %>%
    filter(n() > 1 & preferred == 1) %>%
    ungroup()
  
  if (nrow(choices_recipes_dup) > 0) {
    cat(
      "⚠️ There are",
      nrow(choices_recipes_dup),
      "duplicate recipe choices"
    )
    print(choices_recipes_dup)
  }
  
  choices_recipes_remaining <- choices_recipes %>%
    group_by(product_name) %>%
    filter(all(preferred == 0)) %>%
    ungroup()
  
  if (nrow(choices_recipes_remaining) > 0) {
    cat(
      "⚠️ There are",
      nrow(choices_recipes_remaining),
      "products without a chosen recipe"
    )
    print(choices_recipes_remaining)
  }
  
  outputs <- final_dat %>%
    select(
      name,
      recipe_no,
      recipe_level,
      product_no,
      product_name,
      product_amount
    ) %>%
    distinct() %>%
    mutate(
      produced = product_amount,
      item     = product_name,
      consumed = NA
    ) %>%
    select(name, recipe_no, recipe_level, item, produced, consumed)
  
  inputs <- final_dat %>%
    select(
      name,
      recipe_no,
      recipe_level,
      ing_no,
      ing_name,
      ing_amount
    ) %>%
    distinct() %>%
    mutate(
      consumed = ing_amount,
      item     = ing_name,
      produced = NA
    ) %>%
    select(name, recipe_no, recipe_level, item, produced, consumed)
  
  consumed_produced <- rbind(outputs, inputs) %>%
    filter(!is.na(item)) %>%
    group_by(name, recipe_no, recipe_level, item) %>%
    summarise(
      consumed = if (all(is.na(consumed))) NA_real_ else na.omit(consumed)[1],
      produced = if (all(is.na(produced))) NA_real_ else na.omit(produced)[1],
      .groups  = "drop"
    ) %>%
    arrange(recipe_level, name)
  
  return(
    list(
      main_bus,
      choices_machines,
      choices_recipes,
      final_dat,
      consumed_produced
    )
  )
}

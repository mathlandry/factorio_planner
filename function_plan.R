# create a function that iterates on levels and produces the number of machines and inputs necessary
plan_project <- function(
    data_input = recipes,
    data_recipes = choices_recipes,
    data_machines = choices_machines,
    data_inout = consumed_produced,
    product,
    quantity,
    usebus = "N",
    priority = "whole"
) {

  # reduce the recipe rows by joining with the two choices datasets
  data_recipes <- data_recipes %>%
    filter(preferred == 1)

  data_pre_curated <- data_input %>%
    right_join(data_recipes, by = c("product_name", "name"))

  data_curated <- data_pre_curated %>%
    distinct(name, product_name, ing_name, .keep_all = TRUE)

  level_max <- data_curated %>%
    filter(product_name == product) %>%
    select(product_name, recipe_level) %>%
    distinct(product_name, recipe_level) %>%
    pull(recipe_level)

  if (length(level_max) == 0 || is.na(level_max)) {
    warning(paste0("No recipe_level found for product '", product, "'"))
  }

  # start by iterating downward and making a to do list
  to_do <- recursive_task_table(
    data_curated,
    product,
    quantity
  ) %>%
    mutate(
      ingredient_hierarchy =
        paste0(strrep("-", 4 * recurse_level), ingredient_name)
    )

  if (usebus == "Y") {
    to_do <- to_do %>%
      filter(recurse_level < 1)
  }

  to_do_simple <- to_do %>%
    group_by(ingredient_name, level) %>%
    summarise(
      quantity_needed = sum(quantity_needed, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(level))

  plan_simple <- to_do_simple %>%
    mutate(desc_col = n():1) %>%
    left_join(
      data_recipes,
      by = c("ingredient_name" = "product_name")
    ) %>%
    select(
      ingredient_name,
      level,
      quantity_needed,
      desc_col,
      name
    ) %>%
    left_join(data_pre_curated, by = c("name")) %>%
    select(
      ingredient_name,
      level,
      quantity_needed,
      desc_col,
      name,
      recipe_no,
      product_no,
      product_name,
      product_amount,
      recipe_time,
      machine_name,
      crafting_speed,
      production_per_machine
    ) %>%
    distinct() %>%
    mutate(
      number_machines = if_else(
        product_name == ingredient_name,
        quantity_needed / production_per_machine,
        NA
      )
    )

  plan_detailed <- to_do %>%
    mutate(desc_col = n():1) %>%
    left_join(
      data_recipes,
      by = c("ingredient_name" = "product_name")
    ) %>%
    select(
      ingredient_name,
      level,
      quantity_needed,
      recurse_level,
      ingredient_no,
      ingredient_hierarchy,
      desc_col,
      name
    ) %>%
    left_join(
      data_pre_curated,
      by = c("name"),
      relationship = "many-to-many"
    ) %>%
    select(
      ingredient_name,
      level,
      quantity_needed,
      recurse_level,
      ingredient_no,
      ingredient_hierarchy,
      desc_col,
      name,
      recipe_no,
      product_no,
      product_name,
      product_amount,
      recipe_time,
      machine_name,
      crafting_speed,
      production_per_machine
    ) %>%
    distinct() %>%
    mutate(
      ID = paste0(name, "_", desc_col),
      number_machines = if_else(
        product_name == ingredient_name,
        quantity_needed / production_per_machine,
        NA
      )
    )

  # iterate on recipes in plan_simple then choose with select_machines()
  plan_simple_final <- plan_simple

  for (recipe in na.omit(unique(plan_simple$name))) {

    needed <- plan_simple_final %>%
      filter(name == recipe, product_name == ingredient_name) %>%
      pull(quantity_needed) %>%
      first()

    if (is.na(needed) || length(needed) == 0) {
      stop(paste0("plan_project: needed is NA for recipe '", recipe, "'"))
    }

    chosen <- select_machine(
      plan_simple_final,
      data_machines,
      recipe,
      needed,
      priority = priority
    )

    if (sum(plan_simple_final$name == recipe & plan_simple_final$machine_name == chosen, na.rm = TRUE) == 0) {
      stop(paste0("plan_project: chosen machine not present for recipe '", recipe, "': ", chosen))
    }

    plan_simple_final <- plan_simple_final %>%
      filter(
        machine_name == chosen | name != recipe
      )
  }

  # do the same for the detailed plan
  plan_detailed_final <- plan_detailed

  for (ident in na.omit(unique(plan_detailed$ID))) {

    rows <- plan_detailed_final %>%
      filter(ID == ident)

    needed <- rows %>%
      filter(product_name == ingredient_name) %>%
      pull(quantity_needed) %>%
      first()

    if (is.na(needed) || length(needed) == 0) {
      stop(paste0("plan_project: needed is NA for ID '", ident, "'"))
    }

    recipe <- rows %>%
      filter(ID == ident) %>%
      pull(name) %>%
      first()

    chosen <- select_machine(
      rows,
      data_machines,
      recipe,
      needed,
      priority = priority
    )

    if (sum(plan_detailed_final$ID == ident & plan_detailed_final$machine_name == chosen, na.rm = TRUE) == 0) {
      stop(paste0("plan_project: chosen machine not present for ID '", ident, "': ", chosen))
    }

    plan_detailed_final <- plan_detailed_final %>%
      filter(
        machine_name == chosen | ID != ident
      ) %>%
      mutate(
        number_machines = if_else(
          product_name == ingredient_name,
          quantity_needed / production_per_machine,
          NA
        )
      )
  }

  # get the total ins and outs from the simple plan
  planned_consumed_produced <- plan_simple_final %>%
    filter(!is.na(number_machines)) %>%
    left_join(data_inout, by = c("name", "recipe_no")) %>%
    mutate(
      consumed_sum =
        round(number_machines * consumed * crafting_speed / recipe_time, 1),
      produced_sum =
        round(number_machines * produced * crafting_speed / recipe_time, 1),
      diff =
        coalesce(produced_sum, 0) - coalesce(consumed_sum, 0)
    ) %>%
    select(item, consumed_sum, produced_sum, diff) %>%
    group_by(item) %>%
    summarise(
      consumed_sum = sum(consumed_sum, na.rm = TRUE),
      produced_sum = sum(produced_sum, na.rm = TRUE),
      diff = sum(diff, na.rm = TRUE),
      .groups = "drop"
    )

  # update the main bus
  changes <- planned_consumed_produced %>%
    filter(diff != 0) %>%
    select(item, diff)

  main_bus <- main_bus %>%
    left_join(changes, by = "item") %>%
    mutate(
      produced = if_else(
        !is.na(diff) & diff > 0,
        coalesce(produced, 0) + diff,
        produced
      ),
      consumed = if_else(
        !is.na(diff) & diff < 0,
        coalesce(consumed, 0) - diff,
        consumed
      ),
      available = if_else(
        !is.na(diff) & diff != 0,
        coalesce(produced, 0) - coalesce(consumed, 0),
        available
      )
    ) %>%
    arrange(desc(available)) %>%
    select(-diff)

  return(
    list(
      to_do,
      to_do_simple,
      plan_detailed_final,
      plan_simple_final,
      planned_consumed_produced,
      main_bus
    )
  )
}

change_recipe <- function(data, product, recipe) {
  input <- data

  output <- input %>%
    mutate(
      preferred = case_when(
        product_name == product & name == recipe ~ 1,
        product_name == product ~ 0,
        TRUE ~ preferred
      )
    )

  return(output)
}

change_machine <- function(data, machine, value = 1) {
  input <- data

  output <- input %>%
    mutate(
      accessible = case_when(
        machine_name == machine ~ value,
        TRUE ~ accessible
      )
    )

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

select_machine <- function(
    data,
    machines,
    recipe,
    production_goal,
    priority = "whole"
) {

  if (is.na(recipe)) {
    stop("select_machine: recipe is NA")
  }

  if (!priority %in% c("whole", "speed")) {
    stop(paste0(
      "select_machine: invalid priority '",
      priority,
      "'. Must be 'whole' or 'speed'."
    ))
  }

  input <- data %>%
    filter(name == recipe) %>%
    select(name, machine_name, production_per_machine) %>%
    distinct(machine_name, .keep_all = TRUE)

  stopifnot(length(unique(input$machine_name)) == nrow(input))

  machines_dat <- machines %>%
    filter(accessible == 1) %>%
    select(machine_name) %>%
    distinct()

  if (priority == "whole") {
    input_machines <- input %>%
      inner_join(machines_dat, by = "machine_name") %>%
      mutate(
        number_required = production_goal / production_per_machine,
        whole = case_when(
          abs(production_goal / production_per_machine - round(production_goal / production_per_machine)) < 1e-9 ~ 1,
          TRUE ~ 0
        ),
        electric = case_when(
          grepl("electric", machine_name) ~ 1,
          TRUE ~ 0
        )
      ) %>%
      arrange(desc(whole), number_required, desc(electric))
  } else if (priority == "speed") {
    input_machines <- input %>%
      inner_join(machines_dat, by = "machine_name") %>%
      mutate(
        number_required = production_goal / production_per_machine,
        whole = case_when(
          abs(production_goal / production_per_machine - round(production_goal / production_per_machine)) < 1e-9 ~ 1,
          TRUE ~ 0
        ),
        electric = case_when(
          grepl("electric", machine_name) ~ 1,
          TRUE ~ 0
        )
      ) %>%
      arrange(number_required, desc(whole), desc(electric))
  }

  if (!exists("input_machines") || nrow(input_machines) == 0) {
    stop(paste0(
      "No accessible machines found for recipe: ", recipe,
      " (priority=", priority, ")"
    ))
  }

  return(input_machines$machine_name[1])
}

# function that takes in data_all, product_name and quantity
recursive_task_table <- function(
    data,
    product,
    quantity,
    output = data.frame(
      ingredient_name = character(),
      level           = integer(),
      quantity_needed = numeric(),
      recurse_level   = integer(),
      ingredient_no   = integer()
    ),
    recurseLevel = 0,
    ingredientNo = 0
) {

  # Get the recipe rows for the product
  recipe_dat <- data %>%
    filter(product_name == product) %>%
    mutate(
      sub_needed = quantity * ing_amount / product_amount
    )

  if (nrow(recipe_dat) == 0) {
    warning(paste0(
      "recursive_task_table: No recipe rows found for product '",
      product,
      "'. Returning current output."
    ))
    return(output)
  }

  new_row <- data.frame(
    ingredient_name = product,
    level           = recipe_dat$product_level[1],
    quantity_needed = quantity,
    recurse_level   = recurseLevel,
    ingredient_no   = ingredientNo
  )

  output <- rbind(output, new_row)

  # base case
  if (recipe_dat$product_level[1] == 0) {
    return(output)
  }

  # iterate on ingredients
  else {
    for (i in seq_len(nrow(recipe_dat))) {

      ing <- recipe_dat$ing_name[[i]]
      prod_level <- recipe_dat$ingredient_level[[i]]
      sub_qty <- recipe_dat$sub_needed[i]

      if (is.na(prod_level)) {
        warning(paste0(
          "recursive_task_table: ingredient_level is NA for ingredient '",
          ing,
          "' under product '",
          product,
          "'."
        ))
      }

      output <- recursive_task_table(
        data,
        ing,
        sub_qty,
        output,
        recurseLevel = recurseLevel + 1,
        ingredientNo = i
      )
    }
  }

  output
}

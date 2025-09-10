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
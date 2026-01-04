source("function_change.R")
source("function_setup.R")


setup_results <- setup()

main_bus <- setup_results[[1]]
choices_machines <- setup_results[[2]]
choices_recipes <- setup_results[[3]]
recipes <- setup_results[[4]]
consumed_produced <- setup_results[[5]]

rm(setup_results)

#test the function
#debugonce(plan_project)
test <- plan_project(product = "bulk-inserter", quantity = 14, usebus="Y")
to_do <- test[[1]]
to_do_simple <- test[[2]]
plan_detailed_final <- test[[3]]
plan_simple_final <- test[[4]]
planned_consumed_produced <- test[[5]]
main_bus <- test[[6]]

test <- plan_project(product = "bulk-inserter", quantity = 14, usebus="Y")
main_bus <- test[[6]]

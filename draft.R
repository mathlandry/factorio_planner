library("rjson")
library(dplyr)
library(readxl)

json_file <- "C:/Users/ml_ml/AppData/Roaming/Factorio/script-output/data-raw-dump.json"
json_data <- fromJSON(file=json_file)
recipes <- json_data[["recipe"]]
filtered_recipes <- Filter(function(x) "ingredients" %in% names(x) && is.list(x$ingredients) && length(x$ingredients) >0, recipes)
machines <- c(json_data[["assembling-machine"]],json_data[["furnace"]])

#ingredients loop
ingredients_dat <- data.frame()
for (i in 1:length(filtered_recipes)){
  rec_temp <- data.frame()
  recipe_name <- names(filtered_recipes[i])
  for (j in 1:length(filtered_recipes[[i]][["ingredients"]])){
    ing_temp <- data.frame()
    ing_temp[1,"name"] <- recipe_name 
    ing_temp <- mutate(ing_temp, ing_no = j,ing_name = filtered_recipes[[i]][["ingredients"]][[j]]["name"], ing_amount = filtered_recipes[[i]][["ingredients"]][[j]]["amount"], ing_type = filtered_recipes[[i]][["ingredients"]][[j]]["type"])
    rec_temp <- rbind(rec_temp,ing_temp)
  }
  ingredients_dat <- rbind(ingredients_dat,rec_temp)
}

rm(i,j,recipe_name,ing_temp,rec_temp)

#results loop
results_dat <- data.frame()
for (i in 1:length(filtered_recipes)){
  rec_temp <- data.frame()
  recipe_name <- names(filtered_recipes[i])
  for (j in 1:length(filtered_recipes[[i]][["results"]])){
    res_temp <- data.frame()
    
    res_temp[1,"name"] <- recipe_name 
    
    res_temp[1,"recipe_no"] <- i 
    
    if ("type" %in% names(filtered_recipes[[i]])){
      res_temp[1,"recipe_type"] <- filtered_recipes[[i]][["type"]]
    }
    else {
      res_temp[1,"recipe_type"] <- ""
    }
    
    if ("category" %in% names(filtered_recipes[[i]])){
      res_temp[1,"recipe_category"] <- filtered_recipes[[i]][["category"]]
    }
    
    else {
      res_temp[1,"recipe_category"] <- "crafting"
    }
    
    if ("energy_required" %in% names(filtered_recipes[[i]])){
      res_temp[1,"recipe_time"] <- filtered_recipes[[i]][["energy_required"]]
    }
    
    else {
      res_temp[1,"recipe_time"] <- 0.5
    }
    
    if (length(filtered_recipes[[i]][["results"]]) > 1){
      res_temp[1,"ByproductsFL"] = "Y"
    }
    
    else {
      res_temp[1,"ByproductsFL"] = "N"
    }
    
    res_temp <- mutate(res_temp, product_no = j,product_name = filtered_recipes[[i]][["results"]][[j]]["name"], product_amount = filtered_recipes[[i]][["results"]][[j]]["amount"], product_type = filtered_recipes[[i]][["results"]][[j]]["type"])
    rec_temp <- rbind(rec_temp,res_temp)
  }
  results_dat <- rbind(results_dat,rec_temp)
}

rm(i,j,recipe_name,res_temp,rec_temp)

#machines loop
machines_dat <- data.frame()
for (i in 1:length(machines)){
  mac_temp <- data.frame()
  machine_name <- names(machines[i])
  for (j in 1:length(machines[[i]][["crafting_categories"]])){
    cra_temp <- data.frame()
    
    cra_temp[1,"machine_name"] <- machine_name 
    cra_temp[1,"recipe_category"] <- machines[[i]][["crafting_categories"]][[j]]
    cra_temp[1,"crafting_speed"] <- machines[[i]][["crafting_speed"]]
    
    mac_temp <- rbind(mac_temp,cra_temp)
  }
  machines_dat <- rbind(machines_dat,mac_temp)
}

rm(i,j,machine_name,cra_temp,mac_temp)

all_dat <- full_join(results_dat,ingredients_dat, by = "name",relationship = "many-to-many") %>% left_join(machines_dat, by = "recipe_category",relationship = "many-to-many") %>% arrange(recipe_type, recipe_category, recipe_no, product_no, machine_name, ing_no) 
all_dat_levels <- filter(all_dat, recipe_category != "recycling") %>% mutate(machine_name = if_else(name == "rocket-part","rocket-silo",machine_name)) %>% mutate(crafting_speed = if_else(name == "rocket-part", 1, crafting_speed))

#add custom-made level 0 recipes
basic <- read_excel("base_resources.xlsx")

all_dat_levels <- all_dat_levels %>% mutate(ingredient_level = NA, product_level = NA, recipe_level = NA) %>% rbind(basic)

#big loop
levels_loop <- c()
all_dat_levels_loop <- all_dat_levels
n <- 0
while(any(is.na(all_dat_levels$recipe_level)) & n < 10){
  levels_loop[[n+1]] <- filter(all_dat_levels_loop, product_level == n) %>%  mutate(item = product_name, level = product_level) %>% select(item,level) %>% distinct() %>% as.data.frame()

  all_dat_levels_loop <- left_join(all_dat_levels_loop, levels_loop[[n+1]], by= c("ing_name" = "item")) %>% mutate(ingredient_level = if_else(is.na(ingredient_level), level, ingredient_level))  %>% select(-level) %>% left_join(levels_loop[[n+1]], by= c("product_name" = "item")) %>% mutate(product_level = if_else(is.na(product_level), level, product_level)) %>% select(-level) %>% 
    group_by(name) %>%  mutate(
      recipe_level = if (any(is.na(ingredient_level))) {
        NA_real_
      } else {
        max(ingredient_level) + 1
      }
    ) %>%
    ungroup()
  
  all_dat_levels_loop <- mutate(all_dat_levels_loop, product_level = if_else(is.na(product_level),recipe_level,product_level))
  n <- n + 1
}
rm(n)


#put all level lists together and make sure no items are in 2 levels
levels_all <- rbind(levels_loop[[1]],levels_loop[[2]],levels_loop[[3]],levels_loop[[4]],levels_loop[[5]],levels_loop[[6]],levels_loop[[7]],levels_loop[[8]],levels_loop[[9]]) %>% distinct() %>% arrange(level)

dup_levels <- levels_all[duplicated(levels_all$item)|duplicated(levels_all$item, fromLast = TRUE),]

# check no recipe has ingredient_level > recipe_level
problem_recipes <- all_dat_levels_loop %>%
  filter(!is.na(recipe_level), !is.na(ingredient_level)) %>%
  filter(ingredient_level > recipe_level)

if (nrow(problem_recipes) > 0) {
  print("⚠️ Problematic recipes found:")
  print(problem_recipes)
}


# some recipes have missing levels
unresolved <- all_dat_levels_loop  %>% filter(recipe_no<1000) %>% 
  filter(is.na(recipe_level)) %>%
  select(name, ing_name, product_name) %>%
  distinct()

if (nrow(unresolved) > 0) {
  cat("⚠️ There are", nrow(unresolved), "recipes still unresolved")
  print(unresolved)
}

#identify which ingredients are missing
unresolved_ings <- all_dat_levels_loop %>% filter(!ing_name %in% product_name) %>% filter(is.na(recipe_level)&recipe_no<1000) %>% select(ing_name) %>% distinct()

# library(igraph)
# 
# edges <- all_dat_levels_loop %>%
#   filter(!is.na(ing_name), !is.na(product_name)) %>%
#   select(from = ing_name, to = product_name) %>%
#   distinct()
# 
# g <- graph_from_data_frame(edges)
# plot(g, vertex.label.cex = 0.6, layout = layout_with_kk)
# plot(g, vertex.label.cex = 0.6, layout = layout_with_fr)
# plot(g, vertex.label.cex = 0.6, layout = layout_with_drl)




#initialize a main bus dataframe
main_bus <- data.frame()

#will need to make a machine/recipe choice dataset
choices_machines <- all_dat_levels_loop %>% select(machine_name, recipe_category, crafting_speed) %>% distinct() %>% arrange(machine_name, recipe_category, crafting_speed) %>% mutate(accessible = if_else(machine_name %in% c("assembling-machine-1","assembling-machine-2","centrifuge","chemical-plant","crusher","electric-furnace","oil-refinery","rocket-silo","steel-furnace","stone-furnace"), 1, 0))
choices_recipes <- all_dat_levels_loop %>% select(product_name, name) %>% distinct() %>% arrange(product_name, name) %>% group_by(product_name) %>% mutate(choices = n()) %>% ungroup() %>% mutate(preferred = if_else(product_name == name|choices == 1, 1, 0))
choices_recipes_remaining <- choices_recipes %>%
  group_by(product_name) %>%
  filter(all(preferred == 0)) %>%
  ungroup()


#create a function that iterates on levels and produces the number of machines and inputs necessary
plan_project <- function(recipe,number,usebus="Y"){
  
  #start by iterating downward and making a to do list
  for (i in max(levels_all$level):1){
    
  }
}

#then go upward and get info from big table
#output an in/out table then update the main bus table
#create an excel table of level recipes to add to big table (uranium mining may be an issue)

library(tidyverse)
library(FishLife)
library(taxize)

possible_species <- unique(paste(FishLife::database$Z_ik$Genus,FishLife::database$Z_ik$Species, sep = ' '))

safe_sci2comm <- purrr::safely(taxize::sci2comm)

common_names <- map(possible_species, safe_sci2comm)

found_common_name <- map_lgl(common_names, ~is.null(.x$error))




viable_common_names <- map(common_names[found_common_name],
                           'result') %>% 
  map_dbl(~length(.x[[1]])) %>% 
  map_lgl(~.x == 1)

really_viable_names <- map(common_names[found_common_name],"result") %>% 
  keep(viable_common_names)


viable_scinames <- map_chr(really_viable_names, names)

viable_commnamnes <- map_chr(really_viable_names, ~.x[[1]])


viable_fishlife <- tibble(scientific_name = viable_scinames,
                          common_name = viable_commnamnes)


viable_fishlife_list <- map(viable_fishlife$scientific_name,~.x) %>% 
  set_names(viable_fishlife$common_name)


write_rds(viable_fishbase, path = here::here("data","viable_fishlife_names.rds"))

write_rds(viable_fishlife_list, path = here::here("data","viable_fishlife_list.rds"))
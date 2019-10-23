# remotes::install_github("danovando/spasm")
library(shiny)
library(tidyverse)
library(spasm)
library(shinyalert)
library(FishLife)
function(input,output, session){
  
  plot_theme <- hrbrthemes::theme_ipsum(base_size = 14,
                                        axis_title_size = 16,
                                        base_family = "Fira Sans")
  
  

  
  theme_set(plot_theme)
  
  capitalize <- function(string) {
    substr(string, 1, 1) <- toupper(substr(string, 1, 1))
    string
  }
  
  num_patches <- 25
  
  target_catch <-  0
  
  sim <- observeEvent(input$go,
                {
                    fish <- create_fish(
                      scientific_name = input$sciname,
                      query_fishlife = T,
                      mat_mode = "length",
                      time_step = 1,
                      sigma_r = input$sigma_r,
                      rec_ac = input$rec_ac,
                      price = 10,
                      price_cv = 0,
                      price_ac = 0,
                      price_slope = 0,
                      steepness = input$steepness,
                      r0 = 100,
                      density_movement_modifier = 1 - input$dd_adult_movement,
                      adult_movement = round((input$adult_movement) / 100 * num_patches / 2),
                      larval_movement = round((input$adult_movement) / 100 * num_patches / 2),
                      density_dependence_form = input$dd_form)
                    
                    if (input$fleet_model == "constant-catch"){

                      shinyalert("Heads up!", "Constant catch is not well supported in the shiny app, proceeed with caution. If things crash try reducing initial fishing mortality rate", type = "warning")
            
                      # approximate b0, then calculate catch as a fraction of b0 set by f/m
                      b0 <- sum(fish$r0 * exp(-fish$m * (0:fish$max_age)) * fish$weight_at_age)
                      
                      target_catch = (1 - exp(-input$f_v_m * fish$m)) * (fish$vbk * b0 /4)
                      
                    }
                    # if (input$fleet_model == "open-access"){
                    #   shinyalert("Heads up!", "Open-access dynamics are not well supported in the shiny app yet, proceeed with caution.", type = "warning")
                    #   
                    # }
                    fleet <- create_fleet(
                      fish = fish,
                      max_cr_ratio = input$max_cr_ratio,
                      cost_cv =  0,
                      cost_ac = 0,
                      cost_slope = 0,
                      q_cv = 0,
                      q_ac = 0,
                      q_slope = 0,
                      fleet_model = input$fleet_model,
                      sigma_effort = 0,
                      length_50_sel = input$sel * fish$length_mature,
                      q = 0.001,
                      initial_effort = (input$f_v_m * fish$m * num_patches) / 0.001,
                      target_catch = target_catch,
                      profit_lags =  1,
                      beta = 1,
                      max_perc_change_f = 2,
                      effort_allocation = input$spatial_allocation,
                      mpa_reaction = input$mpa_reaction
                    )
                    withProgress(message = "Running MPA simulations...", value = 0, {
                      experiment <- spasm::mpa_counterfactual(
                      fish = fish,
                      fleet = fleet,
                      year_mpa = input$year_mpa,
                      mpa_size = input$mpa_size/100,
                      sim_years = 50,
                      burn_years = 1,
                      num_patches = num_patches,
                      random_mpas = input$random_mpas,
                      min_size = 1 - input$min_size,
                      mpa_habfactor = input$mpa_habfactor,
                      sprinkler = input$sprinkler
                    )
                    incProgress(1/1,detail = "See plots in 'Results' tab" )
                    })
                    
                    raw <- experiment$raw_outcomes %>%
                      select(year,
                             patch,
                             biomass,
                             biomass_caught,
                             profits,
                             effort,
                             experiment,
                             mpa) %>%
                      rename(catch = biomass_caught) %>%
                      gather(metric, value, -year,-patch,-experiment,-mpa) %>%
                      group_by(year, patch, metric, experiment) %>%
                      summarise(value = sum(value),
                                mpa = unique(mpa)) %>%
                      group_by(year, patch) %>%
                      mutate(mpa = unique(mpa[experiment == "with-mpa"])) %>%
                      ungroup() %>%
                      group_by(metric) %>%
                      mutate(value = value / max(value)) %>%
                      ungroup() %>%
                      spread(experiment, value) %>%
                      ungroup() %>%
                      mutate(delta = `with-mpa` / `no-mpa`,
                             ref = 1)
                    
                    delta_sum <- raw %>%
                      mutate(years_protected = year - input$year_mpa) %>% 
                      select(-year) %>% 
                      group_by(years_protected, metric) %>%
                      summarise(no_mpa = sum(`no-mpa`),
                                with_mpa = sum(`with-mpa`)) %>%
                      ungroup() %>%
                      # gather(experiment, value, no_mpa:with_mpa) %>% 
                      group_by(metric) %>% 
                      # mutate(pre_mpa = value[years_protected == 0]) %>% 
                      ungroup() %>% 
                      mutate(scaled_value = plyr::round_any((with_mpa - no_mpa) / no_mpa,.00001))
                    
                    
                    raw_sum <- raw %>%
                      mutate(years_protected = year - input$year_mpa) %>% 
                      select(-year) %>% 
                      group_by(years_protected, metric) %>%
                      summarise(no_mpa = sum(`no-mpa`),
                                with_mpa = sum(`with-mpa`)) %>%
                      ungroup() %>%
                      gather(experiment, value, no_mpa:with_mpa) %>%
                      group_by(metric, experiment) %>% 
                      mutate(pre_mpa = value[years_protected == 0]) %>%
                      ungroup() 
                    
                    # ribbon <- raw_sum %>%
                    #   group_by(years_protected, metric) %>%
                    #   summarise(ymin = min(scaled_value),
                    #             ymax = max(scaled_value),
                    #             delta = scaled_value[experiment == "with_mpa"] - scaled_value[experiment == "no_mpa"]) %>%
                    #   ungroup() %>%
                    #   group_by(metric) %>%
                    #   mutate(max_delta = last(delta)) %>%
                    #   ungroup()
                    # 
                    # local_vars <- ls()
                    # 
                    # results$sim <-   purrr::map(local_vars, ~get(.x)) %>% 
                    #   purrr::set_names(local_vars)
                    
                    output$summary_plot <- renderPlot({
                      
                      # str(results$sim$raw_sum)
                      # 
                      delta_sum %>%
                        filter(years_protected > -5) %>%
                        ungroup() %>%
                        ggplot() +
                        geom_hline(aes(yintercept = 0), linetype = 2) + 
                        geom_line(aes(years_protected, scaled_value, color = scaled_value), size = 1.5, show.legend = FALSE) +
                        # geom_ribbon(data = ribbon %>%  filter(years_protected > -5), aes(
                        #   years_protected,
                        #   ymin = ymin,
                        #   ymax = ymax,
                        #   fill = max_delta
                        # ),
                        # alpha = 0.5,
                        # show.legend = FALSE) +
                        facet_wrap( ~ metric, scales = "free_y",
                                    labeller = labeller(metric = tools::toTitleCase)) +
                        scale_fill_gradient(low = "tomato", high = "steelblue") +
                        hrbrthemes::scale_y_percent() +
                        theme(
                          axis.title.y = element_blank(),
                          panel.spacing = unit(1,"lines")) +
                        labs(x = "Years With MPA", y = "% MPA Effect") +
                        scale_color_gradient2(low = "tomato", mid = 'grey', high = "blue", midpoint = 0)
                    }) # close summary plot  
                    
                    output$static_doughnut <- renderPlot({
                      
                      raw %>%
                        filter(metric %in% c("biomass","effort"),
                               year == max(year)) %>%
                        ggplot() +
                        geom_col(
                          aes(patch, `with-mpa`, fill = mpa),
                          width = 1
                        ) +
                        geom_line(aes(patch, `no-mpa`), size = 2, color = "black") +
                        facet_wrap(~ metric, labeller = labeller(.default = capitalize)) +
                        coord_polar() +
                        theme(axis.text.x = element_blank(),
                              axis.title.x = element_blank(),
                              axis.title.y = element_blank(),
                              axis.text.y = element_blank(),
                              legend.position = "top") +
                        ggsci::scale_fill_npg(labels = c("Outside MPA","Inside MPA"), name = "") + 
                        labs(
                             caption = "Pie represents the spatial biomass and effort in the fishery at equilibrium. \nBlack line shows values of bioamss and effort in absence of the MPA")
                    }) # close static_doughnut
                    
                    output$raw_summary_plot <- renderPlot({
                      
                      raw_ribbon <- raw_sum %>%
                        group_by(years_protected, metric) %>%
                        summarise(ymin = min(value),
                                  ymax = max(value),
                                  delta = value[experiment == "with_mpa"] - value[experiment == "no_mpa"]) %>%
                        ungroup() %>%
                        group_by(metric) %>%
                        mutate(max_delta = last(delta)) %>%
                        ungroup()
                      
                      raw_sum %>%
                        ungroup() %>%
                        ggplot() +
                        geom_line(aes(years_protected, value, color = experiment), size = 1.5) +
                        geom_ribbon(data = raw_ribbon, aes(
                          years_protected,
                          ymin = ymin,
                          ymax = ymax,
                          fill = max_delta
                        ),
                        alpha = 0.5,
                        show.legend = FALSE) +
                        facet_wrap( ~ metric, scales = "free_y",
                                    labeller = labeller(metric = tools::toTitleCase)) +
                        scale_fill_gradient(low = "tomato", high = "steelblue") +
                        theme(
                          axis.title.y = element_blank(),
                          panel.spacing = unit(1,"lines")) +
                        labs(x = "Years With MPA") +
                        ggsci::scale_color_npg(labels = c("Without MPAs","With MPAs"), name = '')
                      
                      
                    }) # close static_doughnut
                    
                    updateTabItems(session, inputId="tabs", selected = "results")
                    
                    
                }) # close observe event
  
  

  
  
  
} # close server function
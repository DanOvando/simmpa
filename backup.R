plot_theme <- hrbrthemes::theme_ipsum(base_size = 14,
                                      axis_title_size = 16,
                                      base_family = "Fira Sans")

theme_set(plot_theme)

capitalize <- function(string) {
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  string
}

year_mpa <- 25

f_v_m <-  2

num_patches <- 25

fish <-
  create_fish(
    scientific_name = "Atractoscion nobilis",
    query_fishlife = T,
    mat_mode = "length",
    time_step = 1,
    sigma_r = 0,
    price = 10,
    price_cv = 0,
    price_ac = 0,
    price_slope = 0,
    steepness = 0.6,
    r0 = 100,
    rec_ac = 0,
    density_movement_modifier = 0,
    adult_movement = round(input$adult_movement / 100 * num_patches),
    larval_movement = 0,
    density_dependence_form = 3
  )

fleet <- create_fleet(
  fish = fish,
  cost_cv =  0,
  cost_ac = 0,
  cost_slope = 0,
  q_cv = 0,
  q_ac = 0,
  q_slope = 0,
  fleet_model = "constant-effort",
  target_catch = 200,
  sigma_effort = 0,
  length_50_sel = 0.002 * fish$length_mature,
  initial_effort = (f_v_m * fish$m ) / fleet$q,
  profit_lags =  1,
  beta = 1,
  max_cr_ratio = 0.8,
  max_perc_change_f = 2,
  effort_allocation = 'profit-gravity',
  b_ref_oa = .9,
  mpa_reaction = "leave"
)


experiment <- spasm::mpa_counterfactual(
  fish = fish,
  fleet = fleet,
  year_mpa = year_mpa,
  mpa_size = .5,
  sim_years = 50,
  burn_years = 1,
  num_patches = num_patches,
  random_mpas = FALSE,
  min_size = 0.1
)

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

animated_doughnut <- raw %>%
  filter(metric %in% c("biomass","effort")) %>%
  ggplot() +
  geom_col(
    aes(patch, `with-mpa`, fill = mpa),
    width = 1
  ) +
  geom_line(aes(patch, `no-mpa`), size = 2,
            color = "black") +
  facet_wrap(~ metric, labeller = labeller(.default = capitalize)) +
  coord_polar() +
  gganimate::transition_time(year) +
  gganimate::ease_aes('linear') +
  labs(title = 'Year: {frame_time}', x = "'",  y = "") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "top") +
  ggsci::scale_fill_npg(labels = c("Witout MPAs","With MPAs"), name = "")

raw_sum <- raw %>%
  mutate(years_protected = year - year_mpa) %>% 
  filter(years_protected > -5) %>%
  select(-year) %>% 
  group_by(years_protected, metric) %>%
  summarise(no_mpa = sum(`no-mpa`),
            with_mpa = sum(`with-mpa`)) %>%
  ungroup() %>%
  gather(experiment, value, no_mpa:with_mpa) %>% 
  group_by(metric, experiment) %>% 
  mutate(pre_mpa = value[years_protected == 0]) %>% 
  ungroup() %>% 
  mutate(value = plyr::round_any(value / pre_mpa - 1,.00001))

ribbon <- raw_sum %>%
  group_by(years_protected, metric) %>%
  summarise(ymin = min(value),
            ymax = max(value),
            delta = value[experiment == "with_mpa"] - value[experiment == "no_mpa"]) %>%
  ungroup() %>%
  group_by(metric) %>%
  mutate(max_delta = last(delta)) %>%
  ungroup()

output$summary_plot <- renderPlot({raw_sum %>%
    ungroup() %>%
    ggplot() +
    geom_line(aes(years_protected, value, color = experiment), size = 1.5) +
    geom_ribbon(data = ribbon, aes(
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
    hrbrthemes::scale_y_percent() +
    theme(
      axis.title.y = element_blank(),
      panel.spacing = unit(1,"lines")) +
    labs(x = "Years With MPA") +
    ggsci::scale_color_npg(labels = c("Without MPAs","With MPAs"), name = '')
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
    geom_line(aes(patch, `no-mpa`), size = 2,
              color = "black") +
    facet_wrap(~ metric, labeller = labeller(.default = capitalize)) +
    coord_polar() +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "top") +
    ggsci::scale_fill_npg(labels = c("Outside MPA","Inside MPA"), name = "") + 
    labs(title = "Spatial distribution of biomass and effort",
         caption = "Pie shows patches in space at equilibrium. \nBlack line shows values of bioamss and effort in absence of the MPA")
}) # close static_doughnut

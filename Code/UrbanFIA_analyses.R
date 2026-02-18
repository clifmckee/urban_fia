---
title: "Urban FIA community ecology analysis"
author: "Clifton McKee, Meghan Avolio "
date: "`r Sys.Date()`"


## Load packages and functions

# Load packages
library(knitr)
library(tidyverse)
library(cowplot)
library(readxl)
library(janitor)
library(sf)
library(vegan)
library(ggrepel)
library(iNEXT)
library(codyn)
library(gridExtra)
library(ggpubr)

# Useful functions
"%ni%" <- Negate("%in%")

### Data import

# Importing data ----------------------------------------------------------

# Import main urban tree data file and clean some columns
trees <- read_csv("Data/cities_2023_trees_w_Rochester.csv") %>% 
  rename(RowID = `...1`) %>% 
  mutate(city = str_sub(EVALID, 1, -9),
         city = case_when(city == "MinneapolMN" ~ "MinneapolisMN",
                          city == "PhiladelphPA" ~ "PhiladelphiaPA",
                          city == "SpringfielMO" ~ "SpringfieldMO",
                          TRUE ~ city))

# Filter to just the live trees
live_trees <- trees %>% 
  filter(STATUSCD == "Live",
         SUBP %ni% c(11,12,13, 14))

# Filter to just the standing dead trees
dead_trees <- trees %>% 
  filter(STATUSCD == "Dead",
         SUBP %ni% c(11,12,13, 14))

# Import city land use data file and clean some columns
landuse <- read_excel("Data/cities_2023_ownership_landuse_w_Rochester.xlsx",
                      sheet = "Cities") %>% 
  mutate(city = case_when(city == "MinneapolMN" ~ "MinneapolisMN",
                          city == "PhiladelphPA" ~ "PhiladelphiaPA",
                          city == "SpringfielMO" ~ "SpringfieldMO",
                          TRUE ~ city))

# Import city plots and condition and clean some columns
plot_cond <- read_excel("Data/cities_2023_ownership_landuse_w_Rochester.xlsx",
                        sheet = "Cond") %>% 
  mutate(city = str_sub(EVALID, 1, -9),
         city = case_when(city == "MinneapolMN" ~ "MinneapolisMN",
                          city == "PhiladelphPA" ~ "PhiladelphiaPA",
                          city == "SpringfielMO" ~ "SpringfieldMO",
                          TRUE ~ city))


# Import database of US cities with coordinates
city_coords <- read_csv("Data/City coordinates/uscities.csv") %>% 
  mutate(camelname = paste0(str_remove_all(city, "[\\.\\s]"), state_id))

# Filter the city coordinates to the cities with urban tree data
tree_city_coords <- city_coords %>% 
  filter(camelname %in% landuse$city)

# Merged city data with coordinates
tree_city_merge <- full_join(x = landuse,
                             y = tree_city_coords,
                             by = c("city" = "camelname"))

# Merged city plot data with coordinates
city_plots_merge <- full_join(x = plot_cond,
                              y = tree_city_coords,
                              by = c("city" = "camelname")) %>% 
  mutate(OWNGRPCD = case_when(OWNGRPCD == "Unknown water" ~ "Water",
                              TRUE ~ OWNGRPCD))

# Merged trees data with coordinates
trees_coords_merge <- full_join(x = live_trees,
                                y = tree_city_coords,
                                by = c("city" = "camelname")) %>% 
  mutate(OWNGRPCD = case_when(OWNGRPCD == "Unknown water" ~ "Water",
                              TRUE ~ OWNGRPCD))
#age
city_age<-read.csv('Data/city_age.csv')

# climate data
climate<-read_csv("Data/city_climate.csv")

## Ariditiy index
ai<-read_csv('Data/city_ai.csv')

climate2<-climate %>% 
  left_join(ai)

pairs(climate2[,4:6])


# Abundance and basal area of trees ---------------------------------------

#### Ownership of live trees

# Plot the proportion of city trees by ownership
propowner<-trees_coords_merge %>% 
  group_by(city, lat, lng, density, OWNGRPCD) %>% 
  summarize(n = n()) 

fig2a<-propowner%>% 
  ggplot() +
  geom_bar(aes(x = reorder(city, density, decreasing = FALSE),
               y = n,
               fill=factor(OWNGRPCD, levels = c( "OtherFederal","StateLocal", "Private", "Water"))), 
           position = "fill", stat = 'identity') +
  scale_fill_manual(name = "Ownership", limits=c('OtherFederal', 'StateLocal', 'Private', 'Water'), labels=c('Federal', 'State or Local', 'Private', 'Water'), values=c('#0072B2', '#56B4E9', '#E69F00', '#000000')) +
  labs(x = "City (ordered by longitude)", y = "Proportion of city trees") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'top')
fig2a
#ggsave("Results/prop_trees_ownership.pdf", height = 5, width = 7)


#### Number of planted trees

# What number of trees in each city are planted?
propPlanted <- trees_coords_merge %>% 
  mutate(IS_PLANTED = case_when(is.na(IS_PLANTED) & 
                                  FIA_LANDUSE == "Forest land" ~ 2,
                                TRUE ~ IS_PLANTED),
         planted_def = case_when(IS_PLANTED == 1 ~ "Planted",
                                 IS_PLANTED == 2 ~ "Natural Regeneration",
                                 IS_PLANTED == 3 ~ "Not Sure")) %>% 
  group_by(city, planted_def) %>% 
  summarize(n = n(), .groups = "drop") %>% 
  inner_join(y = tree_city_merge) %>% 
  inner_join(y = city_ecoreg) %>% 
  group_by(city) %>% 
  mutate(proportion = round(n/sum(n), 2))

Fig2b<-propPlanted %>% 
  ggplot() +
  geom_col(aes(x = reorder(city, lng, decreasing = FALSE),
               y = proportion,
               fill = factor(planted_def, levels=c('Not Sure','Natural Regeneration', 'Planted'))),
           position = "stack") +
  scale_fill_manual(name = "Planting Status", values=c("#000000","#009E73", "#7B3294" )) +
  labs(x = "City (ordered by longitude)", y = "Proportion of city trees") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'top') #+
Fig2b

fig2<-ggarrange(fig2a, Fig2b, labels=c('A', 'B'), ncol=1)

ggsave("Results/Figure2_Feb18.jpeg", height = 7, width = 5)


# #### Ownership of live trees BASAL AREA
# # Plot the proportion of city trees by ownership
# trees_coords_merge %>%   
#   mutate(basal_area = 0.00064516*pi*(DIA/2)^2) %>% 
#   group_by(city, lat, lng, OWNGRPCD) %>% 
#   summarize(sumBA = sum(basal_area)) %>% 
#   ggplot() +
#   geom_col(aes(x = reorder(city, lng, decreasing = FALSE),
#                y = sumBA,
#                fill = OWNGRPCD),
#            position = "fill") +
#   scale_fill_viridis_d(name = "Ownership", option = "H") +
#   labs(x = "City (ordered by longitude)", y = "Proportion of city trees basal area") +
#   theme_bw(base_size = 10) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ggsave("Results/prop_trees_ownership_BasalArea.pdf", height = 5, width = 7)
# 
# #### Number of planted trees BASAL area
# 
# # What basal area of trees in each city are planted?
# planted_trees_tableBA <- trees_coords_merge %>% 
#   mutate(basal_area = 0.00064516*pi*(DIA/2)^2) %>% 
#   mutate(IS_PLANTED = case_when(is.na(IS_PLANTED) & 
#                                   FIA_LANDUSE == "Forest land" ~ 2,
#                                 TRUE ~ IS_PLANTED),
#          planted_def = case_when(IS_PLANTED == 1 ~ "planted",
#                                  IS_PLANTED == 2 ~ "natural regeneration",
#                                  IS_PLANTED == 3 ~ "not sure")) %>% 
#   group_by(city, planted_def) %>% 
#   summarize(sumBA = sum(basal_area), .groups = "drop") %>% 
#   inner_join(y = tree_city_merge) %>% 
#   inner_join(y = city_ecoreg) %>% 
#   group_by(city) %>% 
#   mutate(proportion = round(sumBA/sum(sumBA), 2))
# 
# planted_trees_tableBA %>% 
#   ggplot() +
#   geom_col(aes(x = reorder(city, lng, decreasing = FALSE),
#                y = proportion,
#                fill = planted_def),
#            position = "stack") +
#   scale_fill_viridis_d(name = "Planted", option = "D") +
#   labs(x = "City (ordered by longitude)", y = "Planted status of trees Basal area") +
#   theme_bw(base_size = 10) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = "bottom") #+
# #facet_grid(cols = vars(classification_3), scales = "free_x", space = "free_x")

##impact of density and age on prop

propowner2<-propowner %>% 
  group_by(city) %>% 
  mutate(tot=sum(n), prop=n/tot) %>% 
  left_join(city_age)

summary(lm(prop~density, data=subset(propowner2, OWNGRPCD=='Private')))

ggplot(data=subset(propowner2, OWNGRPCD=='Private'), aes(x=density, y=prop))+
  geom_point()


summary(lm(prop~est, data=subset(propowner2, OWNGRPCD=='Private')))

summary(lm(proportion~density, data=subset(propPlanted, planted_def=='Planted')))



# estimating diversity ----------------------------------------------------



#### Species accumulation curves - 

# Convert tree data to community dataset, pooled by plots within city
tree_community_plots <- live_trees %>% 
  group_by(city, PLOTID, scientific) %>% 
  summarize(count = n()) %>% 
  pivot_wider(names_from = scientific, values_from = count) %>% 
  replace(is.na(.), 0)

tree_community_plots_all <- live_trees %>% 
  group_by(city, PLOTID, scientific) %>% 
  summarize(count = n()) %>% 
  pivot_wider(names_from = scientific, values_from = count) %>% 
  replace(is.na(.), 0)

# Calculate species accumulation curves across cities
poolaccum_fun <- function(df, city, nperm, measure) {
  set.seed(20250527)
  sp <- poolaccum(df[df$city == city, -c(1, 2)],
                  permutations = nperm, minsize = 3)
  sp_t <- data.frame(t(sp[[measure]]))
  sp_summary <- tibble(
    city = city,
    N = sp$N,
    mean = sapply(sp_t, \(x) mean(x, na.rm = TRUE)),
    median = sapply(sp_t, \(x) median(x, na.rm = TRUE)),
    lower = sapply(sp_t, \(x) quantile(x, probs = 0.025, na.rm = TRUE)),
    upper = sapply(sp_t, \(x) quantile(x, probs = 0.975, na.rm = TRUE)),
    stdev = sapply(sp_t, \(x) sd(x, na.rm = TRUE))
  ) %>% 
    drop_na()
  return(sp_summary)
}
poolaccum_S <- NULL
for(i in 1:length(tree_city_merge$city)){
  poolaccum_S <- rbind(poolaccum_S,
                       poolaccum_fun(tree_community_plots,
                                     tree_city_merge$city[i], 100, "S"))
}
poolaccum_S <- clean_names(poolaccum_S) %>% 
  inner_join(y = city_ecoreg)

# Plot species accumulation curves by city
poolaccum_S %>% 
  ggplot() +
  geom_ribbon(aes(x = n, ymin = lower, ymax = upper,
                  group = city, fill = classification_3), alpha = 0.1) +
  geom_line(aes(x = n, y = mean,
                group = city, color = classification_3)) +
  geom_text_repel(data = poolaccum_S %>% 
                    group_by(city) %>% 
                    slice_tail(n = 1),
                  aes(label = paste0(city, " = ", mean),
                      x = n,
                      y = mean),
                  nudge_x = 5,
                  min.segment.length = 2,
                  size = 2.5,
                  max.overlaps = 22) +
  scale_color_brewer(name = "Group", palette = "Dark2") +
  scale_fill_brewer(name = "Group", palette = "Dark2") +
  labs(x = "Number of plots with live trees", y = "Tree species") +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom")


#### Species area curves

# Calculate plots as proportion of city area
poolaccum_S_area <- poolaccum_S %>% 
  inner_join(landuse) %>% 
  mutate(cumul_plot_area_m2 = n * 672.45352234021,
         city_area_m2 = city_acres * 4046.8564224,
         prop_city_area = cumul_plot_area_m2 / city_area_m2)

# Plot species accumulation curves by city as a proportion of city area
poolaccum_S_area %>% 
  ggplot() +
  geom_ribbon(aes(x = prop_city_area, ymin = lower, ymax = upper,
                  group = city, fill = classification_3), alpha = 0.1) +
  geom_line(aes(x = prop_city_area, y = mean,
                group = city, color = classification_3)) +
  geom_text_repel(data = poolaccum_S_area %>% 
                    group_by(city) %>% 
                    slice_tail(n = 1),
                  aes(label = paste0(city, " = ", mean),
                      x = prop_city_area,
                      y = mean),
                  nudge_x = 0.0001,
                  min.segment.length = 0.0001,
                  size = 2.5,
                  max.overlaps = 22) +
  scale_color_brewer(name = "Group", palette = "Dark2") +
  scale_fill_brewer(name = "Group", palette = "Dark2") +
  labs(x = "Proportion of total city area sampled in plots", y = "Tree species") +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom")


#### Extrapolated species richness


# Extrapolate species diversity from pooled plots within cities
pooled_accum <- with(tree_community_plots[, c(1, 2)],
                     specpool(tree_community_plots[, -c(1, 2)],
                              city,
                              smallsample = TRUE)) %>%
  rownames_to_column("city") %>% 
  rename(Species.est = Species,
         chao.est = chao,
         jack1.est = jack1,
         jack2.est = jack2,
         boot.est = boot)%>% 
  left_join(climate)

pooled_accum_long <- pooled_accum %>% 
  pivot_longer(Species.est:boot.se,
               names_to = c("estimate", "measure"), names_sep = "\\.",
               values_to = "value") %>% 
  pivot_wider(names_from = measure, values_from = value) 

# Plot extrapolated species richness indices by city
pooled_accum_long %>% 
  ggplot() +
  geom_col(aes(x = city, y = est)) +
  geom_errorbar(aes(x = reorder(city, est, median, decreasing = TRUE),
                    ymin = est - se, ymax = est + se), width = 0) +
  facet_wrap(~estimate) +
  labs(x = "City (ordered by median richness)", y = "Species richness estimate") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8))

# Table
pooled_accum %>% 
  kable(digits = 1)
write_csv(pooled_accum, "Results/pooled_species_richness.csv")

#pairs plot
pairs(pooled_accum[,2:9])

#plot with climate
ggplot(data=pooled_accum, aes(x=MAP_mm, y=jack1.est))+
  geom_point()+
  geom_errorbar(aes(ymin=jack1.est-jack1.se, ymax = jack1.est+jack1.se))
ggplot(data=pooled_accum, aes(x=Tmin_C, y=chao.est))+
  geom_point()
```
###rarified richness (sampling same number of plots)

```{r rarifeid richness specaccum}

#code from co-pilot

# split into a named list of community matrices
city_mats <- split(tree_community_plots[, -c(1,2)], tree_community_plots$city) 

#run specaccum for each city seperately
accum_list <- lapply(city_mats, function(mat) {
  specaccum(mat, method = "random", permutations = 999)
})

#only plots with trees are included here, so the minplots does not work because this is all plots
minplots<-min(tree_city_merge$plot_count_Curr)
min_sites <- min(sapply(city_mats, nrow))

# pull out richness at 56 plots for all cities
#okay, we think maybe we want to rarefy by species, this seems so low


extract_vals <- function(acc, min_sites) {
  idx <- which(acc$sites == min_sites)
  data.frame(
    rrichness = acc$richness[idx],
    SD       = acc$sd[idx],
    Lower95  = acc$richness[idx] - 1.96 * acc$sd[idx],
    Upper95  = acc$richness[idx] + 1.96 * acc$sd[idx]
  )
}

df_rarified <- do.call(rbind,
                       lapply(names(accum_list), function(city) {
                         out <- extract_vals(accum_list[[city]], min_sites)
                         out$city <- city
                         out
                       })
) 

#compare rarefied vs extrapolated

rich_explore<-df_rarified %>% 
  left_join(pooled_accum, by='city') %>% 
  left_join(climate2)

plot(rich_explore$rrichness, rich_explore$chao.est)

comparePlot<-rich_explore %>% 
  select(city, MAP_mm, Tmin_C, rrichness, chao.est, SD, chao.se) %>% 
  pivot_longer(names_to='rich_est', values_to='rich_val', rrichness:chao.est) %>% 
  pivot_longer(names_to='rich_err', values_to = 'err_val', SD:chao.se) %>% 
  mutate(drop=ifelse(rich_est=="rrichness"&rich_err=='chao.se', 1, 
                     ifelse(rich_est=='chao.est'&rich_err=='SD', 1, 0))) %>% 
  filter(drop==0)

# ggplot(data=comparePlot, aes(x=Tmin_C, y=rich_val, color=rich_est))+
#   geom_point()+
#   geom_errorbar(aes(ymin=rich_val-err_val, ymax = rich_val+err_val))+
#   facet_grid(~rich_est)+
#   geom_smooth(method = 'lm')
# 
# ggplot(data=comparePlot, aes(x=MAP_mm, y=rich_val, color=rich_est))+
#   geom_point()+
#   geom_errorbar(aes(ymin=rich_val-err_val, ymax = rich_val+err_val))+
#   facet_grid(~rich_est)+
#   geom_smooth(method = 'lm')
# 
# 
# 
# ggplot(data=rich_explore, aes(x=Tmin_C, y=rrichness))+
#   geom_point()+
#   geom_errorbar(aes(ymin=rrichness-SD, ymax = rrichness+SD))+
#   geom_smooth(method = 'lm')
# 
# ggsave("Results/rrich_Temp.pdf", height = 5, width = 7)
# 
# ggplot(data=rich_explore, aes(x=MAP_mm, y=rrichness))+
#    geom_point()+
#   geom_errorbar(aes(ymin=rrichness-SD, ymax = rrichness+SD))+
#   geom_smooth(method = 'lm')
# 
# ggsave("Results/rrich_MAP.pdf", height = 5, width = 7)
```





#### Species richness (codyn)

```{r sp-richness}
# Relative proportions of species
cities_abund <- live_trees %>% 
  group_by(city, scientific) %>% 
  count() %>% 
  group_by(city) %>% 
  mutate(relative_prop = n/sum(n))
# Richness and evenness
city_diversity <- community_structure(cities_abund,
                                      replicate.var = "city",
                                      abundance.var = "relative_prop",
                                      metric = "Evar") %>% 
  inner_join(y = tree_city_merge) %>% 
  inner_join(y = city_ecoreg)

# Plot richness
city_diversity  %>% 
  ggplot() +
  geom_col(aes(x = reorder(city, lng, decreasing = FALSE),
               y = richness)) +
  labs(x = "City (ordered by longitude)", y = "Species richness") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(cols = vars(classification_3), scales = "free_x", space = "free_x")
ggsave("Results/species_richness.pdf", height = 5, width = 7)

# Plot richness
city_diversity %>% 
  select(city, classification_3, richness, Evar) %>% 
  kable()
write_csv(city_diversity, "Results/city_diversity.csv")
```

####comparing all ways of doing richness
```{r rich compare}

rich_compare<-df_rarified %>% 
  left_join(pooled_accum, by='city') %>% 
  left_join(city_diversity, by='city') %>% 
  select(city, richness, rrichness, chao.est, jack1.est, SD, chao.se, jack1.se) %>% 
  rename(ObservedRichness=richness,
         RarefiedRichnes=rrichness,
         ChaoEstimated=chao.est,
         JacknifedEstimated=jack1.est)

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  test <- cor.test(x,y) 
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 1),
                   symbols = c("*", " "))
  
  
  text(0.5, 0.5, txt, cex = 2)
  text(0.8, 0.5, Signif, cex=5, col="red")
}

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, ...)
}


pairs(rich_compare[,2:5])
pairs(rich_compare[,2:5], upper.panel = panel.cor,diag.panel = panel.hist, cex.axis = 2)


rich_climate<-rich_compare %>% 
  left_join(climate2)

summary(lm(ObservedRichness~ai, data=rich_climate))
summary(lm(ObservedRichness~MAP_mm, data=rich_climate))
summary(lm(ObservedRichness~Tmin_C, data=rich_climate))

ggplot(data=rich_climate, aes(x=Tmin_C, y=ChaoEstimated))+
  geom_point()


test<-sanant %>% 
  group_by(scientific) %>% 
  summarize(n=length(DIA))

```

##### Species Richness, Evenness, and Beta (rarefied by ownership and planted)


```{r sp-richness_analyses}
#need to find plot data and make public/private columns and merge with city, same for planted, not planted

tree_community_plots_public <- live_trees %>% 
  filter(OWNGRPCD %in% c('OtherFederal','StateLocal')) %>% 
  group_by(city, PLOTID, scientific) %>% 
  summarize(count = n()) %>% 
  pivot_wider(names_from = scientific, values_from = count, values_fill = 0)

tree_community_plots_private <- live_trees %>% 
  filter(OWNGRPCD %in% c('Private')) %>% 
  group_by(city, PLOTID, scientific) %>% 
  summarize(count = n()) %>% 
  pivot_wider(names_from = scientific, values_from = count, values_fill = 0)

tree_community_plots_planted <- live_trees %>% 
  filter(IS_PLANTED==1) %>% 
  group_by(city, PLOTID, scientific) %>% 
  summarize(count = n()) %>% 
  pivot_wider(names_from = scientific, values_from = count, values_fill = 0)

tree_community_plots_spont <- live_trees %>% 
  filter(IS_PLANTED==2) %>% 
  group_by(city, PLOTID, scientific) %>% 
  summarize(count = n()) %>% 
  pivot_wider(names_from = scientific, values_from = count, values_fill = 0)


#code from co-pilot

# split into a named list of community matrices
city_mats <- split(tree_community_plots_planted[, -c(1,2)], tree_community_plots_planted$city) 

#run specaccum for each city seperately
accum_list <- lapply(city_mats, function(mat) {
  specaccum(mat, method = "random", permutations = 999)
})
#get min sites
min_sites <- min(sapply(city_mats, nrow))
#extract for this min site for all cities
extract_vals <- function(acc, min_sites) {
  idx <- which(acc$sites == min_sites)
  data.frame(
    rrich_planted = acc$richness[idx])
}

rare_planted <- do.call(rbind,
                        lapply(names(accum_list), function(city) {
                          out <- extract_vals(accum_list[[city]], min_sites)
                          out$city <- city
                          out
                        })
) 

rare_planted <-rare_planted %>% 
  mutate(metric='rarefied richness',
         planted='planted') %>% 
  rename(div=rrich_planted)


# split into a named list of community matrices
city_mats <- split(tree_community_plots_spont[, -c(1,2)], tree_community_plots_spont$city) 

#run specaccum for each city seperately
accum_list <- lapply(city_mats, function(mat) {
  specaccum(mat, method = "random", permutations = 999)
})
#get min sites
min_sites <- min(sapply(city_mats, nrow))
#extract for this min site for all cities
extract_vals <- function(acc, min_sites) {
  idx <- which(acc$sites == min_sites)
  data.frame(
    rrich_spont = acc$richness[idx])
}

rare_spont <- do.call(rbind,
                      lapply(names(accum_list), function(city) {
                        out <- extract_vals(accum_list[[city]], min_sites)
                        out$city <- city
                        out
                      })
) 
rare_spont <-rare_spont %>% 
  mutate(metric='rarefied richness',
         planted='spontaneous')%>% 
  rename(div=rrich_spont)


city_mats <- split(tree_community_plots_public[, -c(1,2)], tree_community_plots_public$city) 

#run specaccum for each city seperately
accum_list <- lapply(city_mats, function(mat) {
  specaccum(mat, method = "random", permutations = 999)
})
#get min sites
min_sites <- min(sapply(city_mats, nrow))
#extract for this min site for all cities
extract_vals <- function(acc, min_sites) {
  idx <- which(acc$sites == min_sites)
  data.frame(
    rrich_public = acc$richness[idx])
}

rare_public <- do.call(rbind,
                       lapply(names(accum_list), function(city) {
                         out <- extract_vals(accum_list[[city]], min_sites)
                         out$city <- city
                         out
                       })
) 
rare_public <-rare_public %>% 
  mutate(metric='rarefied richness',
         ownership='public')%>% 
  rename(div=rrich_public)

city_mats <- split(tree_community_plots_private[, -c(1,2)], tree_community_plots_private$city) 

#run specaccum for each city seperately
accum_list <- lapply(city_mats, function(mat) {
  specaccum(mat, method = "random", permutations = 999)
})
#get min sites
min_sites <- min(sapply(city_mats, nrow))
#extract for this min site for all cities
extract_vals <- function(acc, min_sites) {
  idx <- which(acc$sites == min_sites)
  data.frame(
    rrich_private = acc$richness[idx])
}

rare_private <- do.call(rbind,
                        lapply(names(accum_list), function(city) {
                          out <- extract_vals(accum_list[[city]], min_sites)
                          out$city <- city
                          out
                        })
) 
rare_private <-rare_private %>% 
  mutate(metric='rarefied richness',
         ownership='private')%>% 
  rename(div=rrich_private)


####evenneess
cities_abund_public <- live_trees %>% 
  filter(OWNGRPCD %in% c('OtherFederal','StateLocal')) %>% 
  group_by(city, scientific) %>% 
  count()

cities_abund_private <- live_trees %>% 
  filter(OWNGRPCD %in% c('Private')) %>% 
  group_by(city, scientific) %>% 
  count()

cities_abund_planted <- live_trees %>% 
  filter(IS_PLANTED == 1) %>% 
  group_by(city, scientific) %>% 
  count()

cities_abund_spont <- live_trees %>% 
  filter(IS_PLANTED == 2) %>% 
  group_by(city, scientific) %>% 
  count()

# Richness and evenness, run for each dataset
even_planted <- community_structure(cities_abund_planted,
                                    replicate.var = "city",
                                    abundance.var = "n",
                                    metric = "Evar") %>% 
  rename(div=Evar) %>% 
  select(-richness) %>% 
  mutate(metric='evenness', planted='planted')
even_spont <- community_structure(cities_abund_spont,
                                  replicate.var = "city",
                                  abundance.var = "n",
                                  metric = "Evar") %>% 
  rename(div=Evar)%>% 
  select(-richness) %>% 
  mutate(metric='evenness', planted='spontaneous')
even_public <- community_structure(cities_abund_public,
                                   replicate.var = "city",
                                   abundance.var = "n",
                                   metric = "Evar") %>% 
  rename(div=Evar)%>% 
  select(-richness) %>% 
  mutate(metric='evenness', ownership='public')

even_private <- community_structure(cities_abund_private,
                                    replicate.var = "city",
                                    abundance.var = "n",
                                    metric = "Evar") %>% 
  rename(div=Evar)%>% 
  select(-richness) %>% 
  mutate(metric='evenness', ownership='private')


##beta diversity as measured by BC dissim
city_list<-unique(live_trees$city)

beta_private<-data.frame()
for (i in 1:length(city_list)){
  
  subset<-tree_community_plots_private %>% 
    filter(city==city_list[i])
  
  bc<-mean(vegdist(subset[,-c(1,2)], methods='bray', upper=F, diag = F))
  
  df<-data.frame(city=city_list[i], div=bc, metric='beta_diversity', ownership='private')
  
  beta_private<-beta_private %>% 
    bind_rows(df)
}

beta_public<-data.frame()
for (i in 1:length(city_list)){
  
  subset<-tree_community_plots_public %>% 
    filter(city==city_list[i])
  
  bc<-mean(vegdist(subset[,-c(1,2)], methods='bray', upper=F, diag = F))
  
  df<-data.frame(city=city_list[i], div=bc, metric='beta_diversity', ownership='public')
  
  beta_public<-beta_public %>% 
    bind_rows(df)
}

beta_planted<-data.frame()
for (i in 1:length(city_list)){
  
  subset<-tree_community_plots_planted %>% 
    filter(city==city_list[i])
  
  bc<-mean(vegdist(subset[,-c(1,2)], methods='bray', upper=F, diag = F))
  
  df<-data.frame(city=city_list[i], div=bc, metric='beta_diversity', planted='planted')
  
  beta_planted<-beta_planted %>% 
    bind_rows(df)
}

beta_spont<-data.frame()
for (i in 1:length(city_list)){
  
  subset<-tree_community_plots_spont %>% 
    filter(city==city_list[i])
  
  bc<-mean(vegdist(subset[,-c(1,2)], methods='bray', upper=F, diag = F))
  
  df<-data.frame(city=city_list[i], div=bc, metric='beta_diversity', planted='spontaneous')
  
  beta_spont<-beta_spont %>% 
    bind_rows(df)
}


diversity_owner<-rare_private %>% 
  bind_rows(rare_public) %>% 
  bind_rows(even_public) %>% 
  bind_rows(even_private) %>% 
  bind_rows(beta_public) %>% 
  bind_rows(beta_private) %>% 
  left_join(city_coords,  by = c("city" = "camelname"))

div_owner_means<-diversity_owner %>% 
  group_by(ownership, metric) %>% 
  summarise(mval=mean(div), sd=sd(div), n=length(div)) %>% 
  mutate(se=sd/sqrt(n))


ggplot(data=subset(diversity_owner, metric=='rarefied richness')) +
  geom_col(aes(x = reorder(city, lng, decreasing = FALSE),
               y = div,
               fill = ownership),
           position = "dodge") +
  scale_fill_viridis_d(name = "Ownership", option = "D") +
  labs(x = "City (ordered by longitude)", y = "Rarefied Richness") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") #+


a<-ggplot(data=div_owner_means, aes(x=ownership, y=mval))+
  geom_bar(stat='identity')+
  geom_errorbar(aes(ymin=mval-se, ymax=mval+se), width=0.2)+
  facet_wrap(~metric, ncol=1, scales='free_y')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab('Mean Diversity Value')+
  xlab('Ownership')
a

diversity_planted<-rare_planted %>% 
  bind_rows(rare_spont) %>% 
  bind_rows(even_planted) %>% 
  bind_rows(even_spont) %>% 
  bind_rows(beta_planted) %>% 
  bind_rows(beta_spont)

div_plant_means<-diversity_planted %>% 
  group_by(planted, metric) %>% 
  summarise(mval=mean(div), sd=sd(div), n=length(div)) %>% 
  mutate(se=sd/sqrt(n))

b<-ggplot(data=div_plant_means, aes(x=planted, y=mval))+
  geom_bar(stat='identity')+
  geom_errorbar(aes(ymin=mval-se, ymax=mval+se), width=0.2)+
  facet_wrap(~metric, ncol=1, scales='free_y')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab('Mean Diversity Value')+
  xlab('Planted Status')+
  annotate('text', x=1.5, y=Inf, vjust=1, label="*", size=8)
b

div_fig<-grid.arrange(a,b, nrow=1)

ggsave("Results/diversity_plots.pdf", div_fig, height = 5, width = 7)

div_own_tests<-diversity_owner %>% 
  pivot_wider(names_from = 'ownership', values_from = 'div')

#ttests
with(subset(div_own_tests, metric=='rarefied richness'), t.test(private, public, paired=T))# sig

with(subset(div_own_tests, metric=='evenness'), t.test(private, public, paired=T))# not sig

with(subset(div_own_tests, metric=='beta_diversity'), t.test(private, public, paired=T))#not sig

div_plant_tests<-diversity_planted %>% 
  pivot_wider(names_from = 'planted', values_from = 'div')

#ttests
with(subset(div_plant_tests, metric=='rarefied richness'), t.test(planted, spontaneous, paired=T))#sig

with(subset(div_plant_tests, metric=='evenness'), t.test(planted, spontaneous, paired=T))#sig

with(subset(div_plant_tests, metric=='beta_diversity'), t.test(planted, spontaneous, paired=T))#sig



```


#### Species evenness (codyn: Evar)

```{r sp-evenness-evar}
# Plot evenness
city_diversity  %>% 
  ggplot() +
  geom_col(aes(x = reorder(city, lng, decreasing = FALSE),
               y = Evar)) +
  labs(x = "City (ordered by longitude)", y = "Species evenness (Evar)") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(cols = vars(classification_3), scales = "free_x", space = "free_x")
ggsave("Results/species_evenness.pdf", height = 5, width = 7)
```

##### Species Richness by climate (rarefied by ownership and planted)


```{r sp-richness_climate}
div_ownership<-diversity_owner %>% 
  left_join(climate2)

div_plant<-diversity_planted %>% 
  left_join(climate2)

summary(lm(div~ai, data=subset(div_plant, metric=='rarefied richness')))


summary(lm(div~MAP_mm*planted, data=subset(div_plant, metric=='rarefied richness')))

summary(lm(div~Tmin_C*planted, data=subset(div_plant, metric=='rarefied richness')))

summary(lm(div~ai*planted, data=subset(div_plant, metric=='rarefied richness')))
summary(lm(div~ai*ownership, data=subset(div_ownership, metric=='rarefied richness')))

ggplot(data=filter(div_ownership, metric=='rarefied richness'), aes(x=ai, y=div, colour = ownership))+
  geom_point()+
  geom_smooth(method = 'lm')

c<-ggplot(data=filter(div_plant, metric=='rarefied richness'), aes(x=ai, y=div, colour = planted))+
  geom_point()+
  geom_smooth(method = 'lm')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = 'top')+
  ylab('Rarefied Richness')+
  xlab('Aridity Index')+
  scale_color_manual(name='', values=c('blue2', 'darkgreen'))


ggplot(data=filter(div_ownership, metric=='rarefied richness'), aes(x=Tmin_C, y=div, colour = ownership))+
  geom_point()+
  geom_smooth(method = 'lm')

d<-ggplot(data=filter(div_ownership, metric=='rarefied richness'), aes(x=ai, y=div, colour = ownership))+
  geom_point()+
  geom_smooth(method = 'lm')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = 'top')+
  ylab('Rarefied Richness')+
  xlab('Aridity Index')+
  scale_color_manual(name='', values=c('purple', 'blue'))
d

climatefig<-grid.arrange(c,d, nrow=1)

ggsave("Results/richess_climate.pdf", climatefig, height = 5, width = 7)

```

#### Extrapolated diversity (iNEXT)

```{r iNEXT-extrapolation, fig.width=18, fig.height=15}
# Make a function to format data as species incidence
cities_sp_div <- function(df, name){
  sp_incid <- df %>% 
    filter(city == name) %>% 
    group_by(PLOTID, scientific) %>% 
    summarize(abund = n(), .groups = "drop") %>% 
    pivot_wider(names_from = PLOTID, values_from = abund) %>% 
    replace(is.na(.), 0) %>% 
    column_to_rownames(var = "scientific") %>% 
    mutate(across(everything(), \(x) as.numeric(x > 0)))
  return(sp_incid)
}
# Format species data for each city
cities_incid_list <- list()
for(i in 1:length(tree_city_merge$city)){
  cities_incid_list[[i]] <- cities_sp_div(df = live_trees, name = tree_city_merge$city[i])
}
cities_incid_list <- setNames(cities_incid_list, tree_city_merge$city)

# Calculate Hill numbers 0, 1, and 2 across a range of sampling plot numbers (100 bootstrap iterations)
size_range = seq(50, 1000, 50)
set.seed(20250720)
cities_iNEXT <- iNEXT(cities_incid_list, q = c(0, 1, 2), datatype="incidence_raw", size = size_range, nboot = 100)
ggiNEXT(cities_iNEXT, type = 1, se=TRUE,
        facet.var = "Assemblage",
        color.var = "Order.q") +
  facet_wrap(~Assemblage, ncol = 5) +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Results/extrapolated_diversity.pdf", height = 15, width = 18)
```

#### Extrapolated diversity by city

```{r chao-estimators-city, fig.width=9, fig.height=9}
# Use Chao estimator to extrapolate max richness
cities_chao_richness <- ChaoRichness(x = cities_incid_list, datatype = "incidence_raw", conf = 0.95) %>% 
  rownames_to_column(var = "city") %>% 
  clean_names() %>% 
  inner_join(y = tree_city_merge) %>% 
  inner_join(y = city_ecoreg)
write_csv(cities_chao_richness, "Results/Chao_estimators_extrapolated_richness.csv")
# Plot
chao_estimators_A <- cities_chao_richness %>% 
  ggplot() +
  geom_col(aes(x = reorder(city, lng, decreasing = FALSE),
               y = estimator)) +
  geom_errorbar(aes(x = reorder(city, lng, decreasing = FALSE),
                    ymin = x95_percent_lower,
                    ymax = x95_percent_upper),
                width = 0) +
  labs(x = "City (ordered by longitude)", y = "Chao extrapolated richness") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(cols = vars(classification_3), scales = "free_x", space = "free_x")

# Use Chao estimator to extrapolate max Shannon diversity
cities_chao_shannon <- ChaoShannon(x = cities_incid_list, datatype = "incidence_raw", conf = 0.95, transform = TRUE, B = 100) %>% 
  rownames_to_column(var = "city") %>% 
  clean_names() %>% 
  inner_join(y = tree_city_merge) %>% 
  inner_join(y = city_ecoreg)
write_csv(cities_chao_shannon, "Results/Chao_estimators_extrapolated_shannon.csv")
# Plot
chao_estimators_B <- cities_chao_shannon %>% 
  ggplot() +
  geom_col(aes(x = reorder(city, lng, decreasing = FALSE),
               y = estimator)) +
  geom_errorbar(aes(x = reorder(city, lng, decreasing = FALSE),
                    ymin = x95_percent_lower,
                    ymax = x95_percent_upper),
                width = 0) +
  labs(x = "City (ordered by longitude)", y = "Chao extrapolated Shannon diversity") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(cols = vars(classification_3), scales = "free_x", space = "free_x")

# Use Chao estimator to extrapolate max Simpson diversity
cities_chao_simpson <- ChaoSimpson(x = cities_incid_list, datatype = "incidence_raw", conf = 0.95, transform = TRUE, B = 100) %>% 
  rownames_to_column(var = "city") %>% 
  clean_names() %>% 
  inner_join(y = tree_city_merge) %>% 
  inner_join(y = city_ecoreg)
write_csv(cities_chao_simpson, "Results/Chao_estimators_extrapolated_simpson.csv")
# Plot
chao_estimators_C <- cities_chao_simpson %>% 
  ggplot() +
  geom_col(aes(x = reorder(city, lng, decreasing = FALSE),
               y = estimator)) +
  geom_errorbar(aes(x = reorder(city, lng, decreasing = FALSE),
                    ymin = x95_percent_lower,
                    ymax = x95_percent_upper),
                width = 0) +
  labs(x = "City (ordered by longitude)", y = "Chao extrapolated Simpson diversity") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(cols = vars(classification_3), scales = "free_x", space = "free_x")

# Combine plots
plot_grid(chao_estimators_A, chao_estimators_B, chao_estimators_C, nrow = 3)
ggsave("Results/chao_estimators.pdf", height = 9, width = 9)
```

#### Extrapolated diversity at 250 plots

```{r iNEXT-extrapolation-250, fig.width=7, fig.height=7}
# Estimate diversity indices at across a range of sampling plots (100 bootstrap replicates)
set.seed(20250720)
extrapolate_cities <- estimateD(cities_incid_list, q = c(0,1,2), datatype = "incidence_raw",
                                base="size", level = seq(50, 1000, 50), nboot = 100, conf=0.95)
write_csv(extrapolate_cities, "Results/extrapolated_hill_numbers.csv")

# Identify the number of plots where the average city has reached 95% of the extrapolated diversity
extrapolate_cities %>% 
  filter(SC >= 0.95) %>% 
  group_by(Assemblage) %>% 
  summarize(min_sample = min(t, na.rm = TRUE)) %>% 
  pull(min_sample) %>% 
  mean()

# Extrapolated Hill numbers for 250 plots
set.seed(20250720)
cities_250 <- extrapolate_cities %>% 
  filter(t == 250) %>% 
  inner_join(y = tree_city_merge, by = c("Assemblage" = "city")) %>% 
  inner_join(y = city_ecoreg, by = c("Assemblage" = "city"))

# Plot Hill numbers by city
cities_250 %>% 
  ggplot() +
  geom_col(aes(x = reorder(Assemblage, lng, decreasing = FALSE),
               y = qD)) +
  geom_errorbar(aes(x = reorder(Assemblage, lng, decreasing = FALSE),
                    ymin = qD.LCL,
                    ymax = qD.UCL),
                width = 0) +
  labs(x = "City (ordered by longitude)", y = "Extrapolated diversity") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(rows = vars(Order.q),
             cols = vars(classification_3), scales = "free", space = "free_x")
ggsave("Results/Hill_numbers_250plots.pdf", height = 7, width = 7)
```

#### Extrapolated diversity by land area at 250 plots

```{r iNEXT-land-250, fig.width=7, fig.height=7}
# Calculate diversity by city land area
area_250 <- cities_250 %>% 
  mutate(cumul_plot_area_m2 = t * 672.45352234021,
         city_area_m2 = city_acres * 4046.8564224,
         prop_city_area = cumul_plot_area_m2 / city_area_m2)
# Plot
area_250 %>% 
  ggplot() +
  geom_text(aes(x = prop_city_area, y = qD,
                color = classification_3, label = Assemblage),
            size = 2) +
  scale_color_brewer(name = "Group", palette = "Dark2") +
  labs(x = "Proportion of city area (square meters) sampled", y = "Extrapolated diversity") +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom") +
  facet_grid(rows = vars(Order.q), scales = "free_y")
ggsave("Results/Hill_numbers_by_city_area_250plots.pdf", height = 7, width = 7)
```

#### Extrapolated diversity at 500 plots

```{r iNEXT-extrapolation-500, fig.width=7, fig.height=7}
# Extrapolated Hill numbers for 500 plots
set.seed(20250720)
cities_500 <- extrapolate_cities %>% 
  filter(t == 500) %>% 
  inner_join(y = tree_city_merge, by = c("Assemblage" = "city")) %>% 
  inner_join(y = city_ecoreg, by = c("Assemblage" = "city"))

# Plot Hill numbers by city
cities_500 %>% 
  ggplot() +
  geom_col(aes(x = reorder(Assemblage, lng, decreasing = FALSE),
               y = qD)) +
  geom_errorbar(aes(x = reorder(Assemblage, lng, decreasing = FALSE),
                    ymin = qD.LCL,
                    ymax = qD.UCL),
                width = 0) +
  labs(x = "City (ordered by longitude)", y = "Extrapolated diversity") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(rows = vars(Order.q),
             cols = vars(classification_3), scales = "free", space = "free_x")
ggsave("Results/Hill_numbers_500plots.pdf", height = 7, width = 7)
```

#### Extrapolated diversity by land area at 500 plots

```{r iNEXT-land-500, fig.width=7, fig.height=7}
# Calculate diversity by city land area
area_500 <- cities_500 %>% 
  mutate(cumul_plot_area_m2 = t * 672.45352234021,
         city_area_m2 = city_acres * 4046.8564224,
         prop_city_area = cumul_plot_area_m2 / city_area_m2)
# Plot
area_500 %>% 
  ggplot() +
  geom_text(aes(x = prop_city_area, y = qD,
                color = classification_3, label = Assemblage),
            size = 2) +
  scale_color_brewer(name = "Group", palette = "Dark2") +
  labs(x = "Proportion of city area (square meters) sampled", y = "Extrapolated diversity") +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom") +
  facet_grid(rows = vars(Order.q), scales = "free_y")
ggsave("Results/Hill_numbers_by_city_area_500plots.pdf", height = 7, width = 7)
```

### Forest plots {.tabset}

#### Ownership of plots in forest land

```{r ownership-forest-plots}
# For forest land only, how many plots are state, federal, or private?
city_plots_merge %>% 
  filter(FIA_LANDUSE %in% c("Forest land", "Rangeland/Chaparral")) %>% 
  group_by(city, lat, lng, OWNGRPCD) %>% 
  summarize(n = n_distinct(PLOTID), .groups = "drop") %>% 
  ggplot() +
  geom_col(aes(x = reorder(city, lng, decreasing = FALSE),
               y = n,
               fill = OWNGRPCD),
           position = "fill") +
  scale_fill_viridis_d(name = "Ownership", option = "H") +
  labs(x = "City (ordered by longitude)", y = "Proportion of city plots in forest") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Ownership of forest plots
city_plots_merge %>% 
  filter(FIA_LANDUSE %in% c("Forest land", "Rangeland/Chaparral")) %>% 
  group_by(city, lat, lng, OWNGRPCD) %>% 
  summarize(n = n_distinct(PLOTID), .groups = "drop") %>% 
  arrange(OWNGRPCD) %>% 
  pivot_wider(names_from = OWNGRPCD, values_from = n) %>% 
  arrange(lng) %>% 
  select(-lat, -lng) %>% 
  kable()
```

#### Ownership of live trees in forest land

```{r ownership-forest-trees}
# For forest land only, how many live trees are state, federal, or private?
trees_coords_merge %>% 
  filter(FIA_LANDUSE %in% c("Forest land", "Rangeland/Chaparral")) %>% 
  group_by(city, lat, lng, OWNGRPCD) %>% 
  summarize(n = n(), .groups = "drop") %>% 
  ggplot() +
  geom_col(aes(x = reorder(city, lng, decreasing = FALSE),
               y = n,
               fill = OWNGRPCD),
           position = "fill") +
  scale_fill_viridis_d(name = "Ownership", option = "H") +
  labs(x = "City (ordered by longitude)", y = "Proportion of city trees in forest") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Ownership of forest trees
trees_coords_merge %>% 
  filter(FIA_LANDUSE %in% c("Forest land", "Rangeland/Chaparral")) %>% 
  group_by(city, lat, lng, OWNGRPCD) %>% 
  summarize(n = n(), .groups = "drop") %>% 
  arrange(OWNGRPCD) %>% 
  pivot_wider(names_from = OWNGRPCD, values_from = n) %>% 
  arrange(lng) %>% 
  select(-lat, -lng) %>% 
  kable()
```

#### Amount of forest area by total land area

```{r land-area-forest-area}
# Does the amount of forest area scale with total city size?
tree_city_merge %>% 
  replace_na(list(`Forest land` = 0, `Rangeland/Chaparral` = 0)) %>% 
  mutate(natural = `Forest land` + `Rangeland/Chaparral`) %>% 
  ggplot(aes(x = city_acres, y = natural)) +
  geom_point() +
  labs(x = "City land area", y = "Natural land area\n(forest, rangeland, and chaparral)") +
  theme_bw(base_size = 10)

# Table
tree_city_merge %>% 
  replace_na(list(`Forest land` = 0, `Rangeland/Chaparral` = 0)) %>% 
  mutate(natural = `Forest land` + `Rangeland/Chaparral`) %>% 
  arrange(lng) %>% 
  select(city, city_acres, `Forest land`, `Rangeland/Chaparral`, natural) %>% 
  kable()
```

#### Number of forest plots by total land area

```{r land-area-forest-plots}
# Does the number of forest plots scale with total city size?
city_plots_merge %>% 
  filter(FIA_LANDUSE %in% c("Forest land", "Rangeland/Chaparral")) %>% 
  group_by(city) %>% 
  summarize(n = n_distinct(PLOTID), .groups = "drop") %>% 
  inner_join(y = tree_city_merge) %>% 
  ggplot(aes(x = city_acres, y = n)) +
  geom_point() +
  labs(x = "City land area", y = "Natural land plots\n(forest, rangeland, and chaparral)") +
  theme_bw(base_size = 10)

# Table
city_plots_merge %>% 
  filter(FIA_LANDUSE %in% c("Forest land", "Rangeland/Chaparral")) %>% 
  group_by(city) %>% 
  summarize(n = n_distinct(PLOTID), .groups = "drop") %>% 
  inner_join(y = tree_city_merge) %>% 
  select(city, city_acres, n) %>% 
  kable()
```

### Compositional diversity {.tabset}

#### Rank abundance curves

```{r rank-abundance}
# Rank abundance of species in each city
rank_abundance <- live_trees %>% 
  group_by(city, scientific) %>% 
  count() %>% 
  group_by(city) %>% 
  mutate(prop = n/sum(n),
         rank = min_rank(desc(prop))) %>% 
  arrange(rank, .by_group = TRUE) %>% 
  mutate(row_number = row_number())

# Classification of planted trees (if not on maintained land, assume NA = "not sure")
planted_trees <- live_trees %>% 
  mutate(IS_PLANTED = case_when(is.na(IS_PLANTED) & 
                                  FIA_LANDUSE == "Forest land" ~ 2,
                                TRUE ~ IS_PLANTED),
         planted_def = case_when(IS_PLANTED == 1 ~ "planted",
                                 IS_PLANTED == 2 ~ "natural regeneration",
                                 IS_PLANTED == 3 ~ "not sure")) %>% 
  group_by(city, scientific, planted_def) %>% 
  count() %>% 
  pivot_wider(names_from = planted_def, values_from = n)

# Merge rank abundance with planted classification
merge_rank <- full_join(x = rank_abundance,
                        y = planted_trees) %>% 
  pivot_longer(`natural regeneration`:`not sure`) %>% 
  replace_na(list(value = 0)) %>% 
  mutate(name = factor(name,
                       levels = c("planted", "natural regeneration", "not sure")),
         prop2 = value/n * prop)
write_csv(merge_rank, "Results/rank_abundance_planted.csv")

# Plot rank abundance curves
rank_abundance %>% 
  full_join(city_ecoreg) %>% 
  filter(row_number <= 10) %>% 
  ggplot(aes(x = factor(row_number), y = prop, group = city, color = classification_3)) +
  geom_line(linewidth = 0.1) +
  geom_smooth(method = "gam", aes(group = classification_3), se = FALSE) +
  scale_color_brewer(name = "Group", palette = "Dark2") +
  labs(x = "Species rank", y = "Relative abundance") +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom")
ggsave("Results/rank_abundance_curves_top10.pdf", height = 5, width = 7)
```

#### Rank abundance, top 10 species

```{r rank-top-10, fig.height = 11, fig.width = 11}
# Plot rank abundance of top 10 species by city
merge_rank %>% 
  inner_join(y = city_ecoreg) %>% 
  filter(row_number <= 10) %>%
  ggplot() +
  geom_col(aes(x = factor(row_number),
               y = prop2, fill = name)) +
  geom_text(aes(x = factor(row_number),
                y = prop, label = scientific),
            size = 1.75,
            angle = 90,
            hjust = 0,
            nudge_y = 0.02) +
  scale_x_discrete(name = "Species rank") +
  scale_y_continuous(name = "Relative abundance",
                     limits = c(0, 0.8), breaks = seq(0, 0.8, 0.2)) + 
  scale_fill_viridis_d(name = "Status", option = "D") +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom") +
  facet_wrap(~city+classification_3, scales = "free_x", ncol = 6)
ggsave("Results/rank_abundance_planted_top10.pdf", height = 11, width = 11)
```

#### Rank abundance curves, basal area

```{r rank-abundance-basal}
# Rank abundance of species by basal area in each city
rank_abundance_basal <- live_trees %>% 
  mutate(basal_area = 0.00064516*pi*(DIA/2)^2) %>% 
  group_by(city, scientific) %>% 
  summarize(total_basal = sum(basal_area, na.rm = TRUE)) %>% 
  group_by(city) %>% 
  mutate(rank = min_rank(desc(total_basal))) %>% 
  arrange(rank, .by_group = TRUE) %>% 
  mutate(row_number = row_number())

# Classification of planted trees (if not on maintained land, assume NA = "not sure")
planted_trees_basal <- live_trees %>% 
  mutate(IS_PLANTED = case_when(is.na(IS_PLANTED) & 
                                  FIA_LANDUSE == "Forest land" ~ 2,
                                TRUE ~ IS_PLANTED),
         planted_def = case_when(IS_PLANTED == 1 ~ "planted",
                                 IS_PLANTED == 2 ~ "natural regeneration",
                                 IS_PLANTED == 3 ~ "not sure"),
         basal_area = 0.00064516*pi*(DIA/2)^2) %>% 
  group_by(city, scientific, planted_def) %>% 
  summarize(total_basal = sum(basal_area, na.rm = TRUE)) %>% 
  pivot_wider(names_from = planted_def, values_from = total_basal)

# Merge rank abundance with planted classification
merge_rank_basal <- full_join(x = rank_abundance_basal,
                              y = planted_trees_basal) %>% 
  pivot_longer(`natural regeneration`:`not sure`) %>% 
  replace_na(list(value = 0)) %>% 
  mutate(name = factor(name,
                       levels = c("planted", "natural regeneration", "not sure")))
write_csv(merge_rank, "Results/rank_abundance_planted_basal.csv")

# Plot rank abundance curves
rank_abundance_basal %>% 
  full_join(city_ecoreg) %>% 
  filter(row_number <= 10) %>% 
  ggplot(aes(x = factor(row_number), y = total_basal, group = city, color = classification_3)) +
  geom_line(linewidth = 0.1) +
  geom_smooth(method = "gam", aes(group = classification_3), se = FALSE) +
  scale_color_brewer(name = "Group", palette = "Dark2") +
  labs(x = "Species rank", y = "Total basal area") +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom")
ggsave("Results/rank_abundance_curves_top10_basal.pdf", height = 5, width = 7)
```

#### Rank abundance, top 10 species, basal area

```{r rank-top-10-basal, fig.height = 11, fig.width = 11}
# Plot rank abundance of top 10 species by city
merge_rank_basal %>% 
  inner_join(y = city_ecoreg) %>% 
  filter(row_number <= 10) %>% 
  ggplot() +
  geom_col(aes(x = factor(row_number),
               y = value, fill = name)) +
  geom_text(aes(x = factor(row_number),
                y = total_basal, label = scientific),
            size = 1.75,
            angle = 90,
            hjust = 0,
            nudge_y = 0.5) +
  scale_x_discrete(name = "Species rank") +
  scale_y_continuous(name = "Total basal area",
                     limits = c(0, 75), breaks = seq(0, 75, 15)) + 
  scale_fill_viridis_d(name = "Status", option = "D") +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom") +
  facet_wrap(~city+classification_3, scales = "free_x", ncol = 6)
ggsave("Results/rank_abundance_planted_top10_basal.pdf", height = 11, width = 11)
```

#### Compositional differences

```{r MDS}
# Community composition of each city
city_comm <- live_trees %>% 
  group_by(city, scientific) %>% 
  count() %>% 
  pivot_wider(names_from = scientific, values_from = n) %>%
  replace(is.na(.), 0) %>%
  column_to_rownames(var = "city")

# Perform MDS on community diversity
set.seed(20250720)
bray_mds <- metaMDS(city_comm, distance = "bray")
bray_mds_cities <- data.frame(bray_mds$points) %>% 
  rownames_to_column(var = "city") %>% 
  inner_join(y = city_ecoreg)
bray_mds_species <- data.frame(bray_mds$species) %>% 
  rownames_to_column(var = "scientific")

# Plot
ggplot() +
  geom_text(data = bray_mds_cities, aes(x = MDS1, y = MDS2, label = city,
                                        color = classification_3),
            size = 2) +
  geom_point(data = bray_mds_species, aes(x = MDS1, y = MDS2),
             shape = 1, size = 0.5,
             position = position_jitter(width = 0.05, height = 0.05)) +
  scale_color_brewer(name = "Group", palette = "Dark2") +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom")
ggsave("Results/metaMDS_species_composition.pdf", height = 5, width = 7)
```

#### Compositional differences (forest)

```{r MDS-forest}
# Community composition of each city
city_comm_forest <- live_trees %>% 
  filter(FIA_LANDUSE %in% c("Forest land", "Rangeland/Chaparral")) %>% 
  group_by(city, scientific) %>% 
  count() %>% 
  pivot_wider(names_from = scientific, values_from = n) %>%
  replace(is.na(.), 0) %>%
  column_to_rownames(var = "city")

# Perform MDS on community diversity
set.seed(20250720)
bray_mds_forest <- metaMDS(city_comm_forest, distance = "bray")
bray_mds_cities_forest <- data.frame(bray_mds_forest$points) %>% 
  rownames_to_column(var = "city") %>% 
  inner_join(y = city_ecoreg)
bray_mds_species_forest <- data.frame(bray_mds_forest$species) %>% 
  rownames_to_column(var = "scientific")

# Plot
ggplot() +
  geom_text(data = bray_mds_cities_forest, aes(x = MDS1, y = MDS2, label = city,
                                               color = classification_3),
            size = 2) +
  geom_point(data = bray_mds_species_forest, aes(x = MDS1, y = MDS2),
             shape = 1, size = 0.5,
             position = position_jitter(width = 0.05, height = 0.05)) +
  scale_color_brewer(name = "Group", palette = "Dark2") +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom")
ggsave("Results/metaMDS_species_composition_forest.pdf", height = 5, width = 7)
```

#### Compositional differences (planted)

```{r MDS-non-forest}
# Community composition of each city
city_comm_nonforest <- live_trees %>% 
  filter(IS_PLANTED == 1) %>% 
  group_by(city, scientific) %>% 
  count() %>% 
  pivot_wider(names_from = scientific, values_from = n) %>%
  replace(is.na(.), 0) %>%
  column_to_rownames(var = "city")

# Perform MDS on community diversity
set.seed(20250720)
bray_mds_nonforest <- metaMDS(city_comm_nonforest, distance = "bray")
bray_mds_cities_nonforest <- data.frame(bray_mds_nonforest$points) %>% 
  rownames_to_column(var = "city") %>% 
  inner_join(y = city_ecoreg)
bray_mds_species_nonforest <- data.frame(bray_mds_nonforest$species) %>% 
  rownames_to_column(var = "scientific")

# Plot
ggplot() +
  geom_text(data = bray_mds_cities_nonforest, aes(x = MDS1, y = MDS2, label = city,
                                                  color = classification_3),
            size = 2) +
  geom_point(data = bray_mds_species_nonforest, aes(x = MDS1, y = MDS2),
             shape = 1, size = 0.5,
             position = position_jitter(width = 0.05, height = 0.05)) +
  scale_color_brewer(name = "Group", palette = "Dark2") +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom")
ggsave("Results/metaMDS_species_composition_nonforest.pdf", height = 5, width = 7)
```

### Public vs. private land {.tabset}

#### Species richness (codyn)

```{r public-private-richness}
# Relative proportions of species
cities_abund_public <- live_trees %>% 
  mutate(own2 = case_when(OWNGRPCD != "Private" ~ "Public",
                          TRUE ~ OWNGRPCD)) %>% 
  group_by(city, own2, scientific) %>% 
  count() %>% 
  group_by(city, own2) %>% 
  mutate(relative_prop = n/sum(n))
# Richness and evenness
public_diversity <- community_structure(
  cities_abund_public,
  time.var = "own2",
  replicate.var = "city",
  abundance.var = "relative_prop",
  metric = "Evar"
) %>%
  inner_join(y = tree_city_merge) %>%
  inner_join(y = city_ecoreg)

# Plot richness
public_diversity %>% 
  ggplot() +
  geom_col(aes(x = reorder(city, lng, decreasing = FALSE),
               y = richness, fill = own2),
           position = "dodge") +
  scale_fill_manual(name = "Ownership", values = c("black", "grey50")) +
  labs(x = "City (ordered by longitude)", y = "Species richness") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  facet_grid(cols = vars(classification_3), scales = "free_x", space = "free_x")
ggsave("Results/species_richness_public_v_private.pdf", height = 5, width = 7)

# Plot richness
public_diversity %>% 
  select(city, classification_3, own2, richness, Evar) %>% 
  kable()
write_csv(public_diversity, "Results/public_v_private_diversity.csv")
```

#### Species evenness (codyn: Evar)

```{r public-private-sp-evenness-evar}
# Plot evenness
public_diversity  %>% 
  ggplot() +
  geom_col(aes(x = reorder(city, lng, decreasing = FALSE),
               y = Evar, fill = own2),
           position = "dodge") +
  scale_fill_manual(name = "Ownership", values = c("black", "grey50")) +
  labs(x = "City (ordered by longitude)", y = "Species evenness (Evar)") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  facet_grid(cols = vars(classification_3), scales = "free_x", space = "free_x")
ggsave("Results/species_evenness_public_v_private.pdf", height = 5, width = 7)
```

#### Beta diversity

```{r public-private-beta-div}
# Make a function to calculate beta diversity for a given city and ownership type
public_beta_div <- function(df, name, own){
  beta_div <- df %>% 
    mutate(own2 = case_when(OWNGRPCD != "Private" ~ "Public",
                            TRUE ~ OWNGRPCD)) %>%
    filter(city == name, own2 == own) %>% 
    group_by(PLOTID, scientific) %>% 
    summarize(abund = n(), .groups = "drop") %>% 
    pivot_wider(names_from = scientific, values_from = abund) %>% 
    replace(is.na(.), 0) %>% 
    column_to_rownames(var = "PLOTID") %>% 
    betadiver(method = "w") %>% 
    as.numeric()
  return(beta_div)
}
# Calculate mean beta diversity for each city and ownership type
public_beta <- expand.grid(city = tree_city_merge$city, own2 = c("Private", "Public"))
for(i in 1:nrow(public_beta)){
  public_beta$mean_beta[i] <- public_beta_div(df = live_trees,
                                              name = public_beta[i, 1],
                                              own = public_beta[i, 2]) %>% 
    mean()
}

# Plot mean beta diversity
public_beta %>%
  inner_join(y = tree_city_merge) %>%
  inner_join(y = city_ecoreg) %>% 
  ggplot() +
  geom_col(aes(x = reorder(city, lng, decreasing = FALSE), y = mean_beta, fill = own2),
           position = "dodge") +
  scale_fill_manual(name = "Ownership", values = c("black", "grey50")) +
  labs(x = "City (ordered by longitude)", y = "Mean beta diversity") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  facet_grid(cols = vars(classification_3), scales = "free_x", space = "free_x")
ggsave("Results/mean_beta_diversity_public_v_private.pdf", height = 5, width = 7)

# Table
public_beta %>% 
  inner_join(y = city_ecoreg) %>% 
  select(city, classification_3, own2, mean_beta) %>% 
  kable()
write_csv(public_beta, "Results/mean_beta_diversity_public_v_private.csv")
```

#### Total trees

```{r public-total-trees}
# Count the total trees by ownership type
total_public <- live_trees %>% 
  mutate(own2 = case_when(OWNGRPCD != "Private" ~ "Public",
                          TRUE ~ OWNGRPCD)) %>% 
  group_by(city, own2) %>% 
  count() %>%
  inner_join(y = tree_city_merge) %>%
  inner_join(y = city_ecoreg) %>% 
  group_by(city) %>% 
  mutate(prop_trees=n/sum(n))

# Plot total trees by ownership type
Fig2D<-total_public %>% 
  ggplot() +
  geom_col(aes(x = reorder(city, population, decreasing = FALSE),
               y = prop_trees, fill = own2)) +
  scale_fill_manual(name = "Ownership", values = c("black", "grey50")) +
  labs(x = "City (ordered by longitude)", y = "Total trees") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") #+
#facet_grid(cols = vars(classification_3), scales = "free_x", space = "free_x")
ggsave("Results/total_trees_public_v_private.pdf", height = 5, width = 7)

# Table
total_public %>% 
  select(city, classification_3, own2, n) %>% 
  kable()
write_csv(total_public, "Results/total_trees_public_v_private.csv")
```

#### Basal area

```{r public-basal_area}
# Count the total basal area by ownership type
basal_public <- live_trees %>% 
  mutate(own2 = case_when(OWNGRPCD != "Private" ~ "Public",
                          TRUE ~ OWNGRPCD),
         basal_area = 0.00064516*pi*(DIA/2)^2) %>% 
  group_by(city, own2) %>% 
  summarize(total_basal = sum(basal_area, na.rm = TRUE)) %>% 
  inner_join(y = tree_city_merge) %>%
  inner_join(y = city_ecoreg)%>% 
  mutate(prop_basal=total_basal/sum(total_basal))

# Plot total basal area by ownership type
Fig2C<-basal_public %>% 
  ggplot() +
  geom_col(aes(x = reorder(city, population, decreasing = FALSE),
               y = prop_basal, fill = own2)) +
  scale_fill_manual(name = "Ownership", values = c("black", "grey50")) +
  labs(x = "City (ordered by longitude)", y = "Basal area (square meters)") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") #+
#facet_grid(cols = vars(classification_3), scales = "free_x", space = "free_x")
ggsave("Results/basal_area_public_v_private.pdf", height = 5, width = 7)

# Table
basal_public %>% 
  select(city, classification_3, own2, total_basal) %>% 
  kable()
write_csv(basal_public, "Results/basal_area_public_v_private.csv")
```

#### Size profiles

```{r public-size-profiles, fig.width=11, fig.height=8.5}
# Density plots for DBH on public v private land
live_trees %>% 
  mutate(own2 = case_when(OWNGRPCD != "Private" ~ "Public",
                          TRUE ~ OWNGRPCD))  %>% 
  inner_join(y = tree_city_merge) %>%
  inner_join(y = city_ecoreg) %>% 
  ggplot() +
  geom_density(aes(x = DIA, color = own2)) +
  scale_color_manual(name = "Ownership", values = c("black", "grey50")) +
  scale_x_continuous(breaks = seq(5, 65, 5)) +
  labs(x = "Diameter at breast height (inches)", y = "Density") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  facet_grid(cols = vars(classification_3), scales = "free_x", space = "free_x") +
  facet_wrap(~city + classification_3)
ggsave("Results/size_density_profiles_public_v_private.pdf", height = 8.5, width = 11)
```

#### Size classes

```{r public-size-classes, fig.width=7, fig.height=7}
# Calculate numbers of trees in different size bins across ownership types
public_size_classes <- live_trees %>% 
  mutate(own2 = case_when(OWNGRPCD != "Private" ~ "Public",
                          TRUE ~ OWNGRPCD),
         size_class = cut(DIA, breaks = c(0, 5, 7, 9, 11, Inf), right = FALSE)) %>% 
  group_by(city, own2, size_class) %>% 
  count() %>% 
  inner_join(y = tree_city_merge) %>%
  inner_join(y = city_ecoreg)

# Plot
public_size_classes %>% 
  ggplot() +
  geom_col(aes(x = reorder(city, lng, decreasing = FALSE),
               y = n, fill = own2),
           position = "dodge") +
  scale_fill_manual(name = "Ownership", values = c("black", "grey50")) +
  labs(x = "City (ordered by longitude)", y = "Trees in size class") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  facet_grid(rows = vars(size_class),
             cols = vars(classification_3), scales = "free_x", space = "free_x")
ggsave("Results/size_classes_public_v_private.pdf", height = 7, width = 7)

# Table
public_size_classes_table <- public_size_classes %>% 
  select(city, classification_3, own2, size_class, n) %>% 
  pivot_wider(names_from = size_class, values_from = n)
public_size_classes_table %>% 
  kable()
write_csv(public_size_classes_table, "Results/basal_area_public_v_private.csv")
```

#### Total standing dead

```{r public-total-dead}
# Numbers of standing dead trees by ownership type
public_dead <- dead_trees %>% 
  mutate(own2 = case_when(OWNGRPCD != "Private" ~ "Public",
                          TRUE ~ OWNGRPCD)) %>% 
  group_by(city, own2) %>% 
  count() %>% 
  inner_join(y = tree_city_merge) %>%
  inner_join(y = city_ecoreg)

# Plot dead trees by ownership type
public_dead %>% 
  ggplot() +
  geom_col(aes(x = reorder(city, lng, decreasing = FALSE),
               y = n, fill = own2),
           position = "dodge") +
  scale_fill_manual(name = "Ownership", values = c("black", "grey50")) +
  labs(x = "City (ordered by longitude)", y = "Standing dead trees") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  facet_grid(cols = vars(classification_3), scales = "free_x", space = "free_x")
ggsave("Results/standing_dead_public_v_private.pdf", height = 5, width = 7)

# Output table
public_dead %>% 
  select(city, classification_3, own2, n) %>% 
  kable()
write_csv(public_dead, "Results/standing_dead_public_v_private.csv")
```

#### Standing dead basal area

```{r public-dead-basal}
# Count the total basal area by ownership type
public_dead_basal <- dead_trees %>% 
  mutate(own2 = case_when(OWNGRPCD != "Private" ~ "Public",
                          TRUE ~ OWNGRPCD),
         basal_area = 0.00064516*pi*(DIA/2)^2) %>% 
  group_by(city, own2) %>% 
  summarize(total_basal = sum(basal_area, na.rm = TRUE)) %>% 
  inner_join(y = tree_city_merge) %>%
  inner_join(y = city_ecoreg)

# Plot total basal area by ownership type
public_dead_basal %>% 
  ggplot() +
  geom_col(aes(x = reorder(city, lng, decreasing = FALSE),
               y = total_basal, fill = own2),
           position = "dodge") +
  scale_fill_manual(name = "Ownership", values = c("black", "grey50")) +
  labs(x = "City (ordered by longitude)", y = "Basal area (square meters) of standing dead") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  facet_grid(cols = vars(classification_3), scales = "free_x", space = "free_x")
ggsave("Results/standing_dead_basal_area_public_v_private.pdf", height = 5, width = 7)

# Table
public_dead_basal %>% 
  select(city, classification_3, own2, total_basal) %>% 
  kable()
write_csv(public_dead_basal, "Results/basal_area_public_v_private.csv")
```

### Planted vs. not planted {.tabset}

#### Species richness (codyn)

```{r planted-richness}
# Relative proportions of species
cities_abund_planted <- live_trees %>% 
  mutate(IS_PLANTED = case_when(is.na(IS_PLANTED) & 
                                  FIA_LANDUSE == "Forest land" ~ 2,
                                TRUE ~ IS_PLANTED),
         planted_def = case_when(IS_PLANTED == 1 ~ "planted",
                                 IS_PLANTED == 2 ~ "natural regeneration",
                                 IS_PLANTED == 3 ~ "not sure")) %>% 
  group_by(city, planted_def, scientific) %>% 
  count() %>% 
  group_by(city, planted_def) %>% 
  mutate(relative_prop = n/sum(n))
# Richness and evenness
planted_diversity <- community_structure(
  cities_abund_planted,
  time.var = "planted_def",
  replicate.var = "city",
  abundance.var = "relative_prop",
  metric = "Evar"
) %>%
  inner_join(y = tree_city_merge) %>%
  inner_join(y = city_ecoreg)

# Plot richness
planted_diversity  %>% 
  filter(planted_def != "not sure") %>% 
  ggplot() +
  geom_col(aes(x = reorder(city, lng, decreasing = FALSE),
               y = richness, fill = planted_def),
           position = "dodge") +
  scale_fill_manual(name = "Planted", values = c("black", "grey50")) +
  labs(x = "City (ordered by longitude)", y = "Species richness") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  facet_grid(cols = vars(classification_3), scales = "free_x", space = "free_x")
ggsave("Results/species_richness_planted_v_not_planted.pdf", height = 5, width = 7)

# Plot richness
planted_diversity %>% 
  select(city, classification_3, planted_def, richness, Evar) %>% 
  kable()
write_csv(public_diversity, "Results/planted_v_not_planted_diversity.csv")
```

#### Species evenness (codyn: Evar)

```{r planted-sp-evenness-evar}
# Plot evenness
planted_diversity %>%
  filter(planted_def != "not sure") %>% 
  ggplot() +
  geom_col(aes(x = reorder(city, lng, decreasing = FALSE),
               y = Evar, fill = planted_def),
           position = "dodge") +
  scale_fill_manual(name = "Planted", values = c("black", "grey50")) +
  labs(x = "City (ordered by longitude)", y = "Species evenness (Evar)") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  facet_grid(cols = vars(classification_3), scales = "free_x", space = "free_x")
ggsave("Results/species_evenness_planted_v_not_planted.pdf", height = 5, width = 7)
```

#### Beta diversity

```{r planted-beta-div}
# Make a function to calculate beta diversity for a given city and planted status
planted_beta_div <- function(df, name, planted){
  beta_div <- df %>% 
    mutate(IS_PLANTED = case_when(is.na(IS_PLANTED) & 
                                    FIA_LANDUSE == "Forest land" ~ 2,
                                  TRUE ~ IS_PLANTED),
           planted_def = case_when(IS_PLANTED == 1 ~ "planted",
                                   IS_PLANTED == 2 ~ "natural regeneration",
                                   IS_PLANTED == 3 ~ "not sure")) %>%
    filter(city == name, planted_def == planted) %>% 
    group_by(PLOTID, scientific) %>% 
    summarize(abund = n(), .groups = "drop") %>% 
    pivot_wider(names_from = scientific, values_from = abund) %>% 
    replace(is.na(.), 0) %>% 
    column_to_rownames(var = "PLOTID") %>% 
    betadiver(method = "w") %>% 
    as.numeric()
  return(beta_div)
}
# Calculate mean beta diversity for each city and planted status
planted_beta <- expand.grid(city = tree_city_merge$city, planted = c("planted", "natural regeneration"))
for(i in 1:nrow(planted_beta)){
  planted_beta$mean_beta[i] <- planted_beta_div(df = live_trees,
                                                name = planted_beta[i, 1],
                                                planted = planted_beta[i, 2]) %>% 
    mean()
}

# Plot mean beta diversity
planted_beta %>%
  inner_join(y = tree_city_merge) %>%
  inner_join(y = city_ecoreg) %>% 
  ggplot() +
  geom_col(aes(x = reorder(city, lng, decreasing = FALSE), y = mean_beta, fill = planted),
           position = "dodge") +
  scale_fill_manual(name = "Planted", values = c("black", "grey50")) +
  labs(x = "City (ordered by longitude)", y = "Mean beta diversity") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  facet_grid(cols = vars(classification_3), scales = "free_x", space = "free_x")
ggsave("Results/mean_beta_diversity_planted_v_not_planted.pdf", height = 5, width = 7)

# Table
planted_beta %>% 
  inner_join(y = city_ecoreg) %>% 
  select(city, classification_3, planted, mean_beta) %>% 
  kable()
write_csv(public_beta, "Results/mean_beta_diversity_planted_v_not_planted.csv")
```

#### Total trees

```{r planted-total-trees}
# Count the total trees by planted status
total_planted <- live_trees %>% 
  mutate(IS_PLANTED = case_when(is.na(IS_PLANTED) & 
                                  FIA_LANDUSE == "Forest land" ~ 2,
                                TRUE ~ IS_PLANTED),
         planted_def = case_when(IS_PLANTED == 1 ~ "planted",
                                 IS_PLANTED == 2 ~ "natural regeneration",
                                 IS_PLANTED == 3 ~ "not sure")) %>% 
  group_by(city, planted_def) %>% 
  count() %>%
  inner_join(y = tree_city_merge) %>%
  inner_join(y = city_ecoreg)

# Plot total trees by planted status
total_planted %>% 
  filter(planted_def != "not sure") %>% 
  ggplot() +
  geom_col(aes(x = reorder(city, lng, decreasing = FALSE),
               y = n, fill = planted_def),
           position = "dodge") +
  scale_fill_manual(name = "Planted", values = c("black", "grey50")) +
  labs(x = "City (ordered by longitude)", y = "Total trees") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  facet_grid(cols = vars(classification_3), scales = "free_x", space = "free_x")
ggsave("Results/total_trees_planted_v_not_planted.pdf", height = 5, width = 7)

# Table
total_planted %>% 
  select(city, classification_3, planted_def, n) %>% 
  kable()
write_csv(total_planted, "Results/total_trees_planted_v_not_planted.csv")
```

#### Basal area

```{r planted-basal_area}
# Count the total basal area by planted status
basal_planted <- live_trees %>% 
  mutate(IS_PLANTED = case_when(is.na(IS_PLANTED) & 
                                  FIA_LANDUSE == "Forest land" ~ 2,
                                TRUE ~ IS_PLANTED),
         planted_def = case_when(IS_PLANTED == 1 ~ "planted",
                                 IS_PLANTED == 2 ~ "natural regeneration",
                                 IS_PLANTED == 3 ~ "not sure"),
         basal_area = 0.00064516*pi*(DIA/2)^2) %>% 
  group_by(city, planted_def) %>% 
  summarize(total_basal = sum(basal_area, na.rm = TRUE)) %>% 
  inner_join(y = tree_city_merge) %>%
  inner_join(y = city_ecoreg) %>% 
  group_by(city) %>% 
  mutate(basal_prop = total_basal/sum(total_basal))

# Plot total basal area by planted status
Fig2A<-basal_planted %>% 
  filter(planted_def != "not sure") %>% 
  ggplot() +
  geom_col(aes(x = reorder(city, population, decreasing = FALSE),
               y = basal_prop, fill = planted_def)) +
  scale_fill_viridis_d(name = "Planted", option = "D") +
  labs(x = "City (ordered by longitude)", y = "Basal area (square meters)") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") #+
#facet_grid(cols = vars(classification_3), scales = "free_x", space = "free_x")
ggsave("Results/basal_area_planted_v_not_planted.pdf", height = 5, width = 7)

# Table
basal_planted %>% 
  select(city, classification_3, planted_def, total_basal) %>% 
  kable()
write_csv(basal_planted, "Results/basal_area_planted_v_not_planted.csv")
```

#### Size profiles

```{r planted-size-profiles}
# Density plots for DBH on public v private land
live_trees %>% 
  mutate(IS_PLANTED = case_when(is.na(IS_PLANTED) & 
                                  FIA_LANDUSE == "Forest land" ~ 2,
                                TRUE ~ IS_PLANTED),
         planted_def = case_when(IS_PLANTED == 1 ~ "planted",
                                 IS_PLANTED == 2 ~ "natural regeneration",
                                 IS_PLANTED == 3 ~ "not sure")) %>% 
  inner_join(y = tree_city_merge) %>%
  inner_join(y = city_ecoreg) %>% 
  filter(planted_def != "not sure") %>% 
  ggplot() +
  geom_density(aes(x = DIA, color = planted_def)) +
  scale_color_manual(name = "Planted", values = c("black", "grey50")) +
  scale_x_continuous(breaks = seq(5, 65, 5)) +
  labs(x = "Diameter at breast height (inches)", y = "Density") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  facet_grid(cols = vars(classification_3), scales = "free_x", space = "free_x") +
  facet_wrap(~city + classification_3)
ggsave("Results/size_density_profiles_planted_v_not_planted.pdf", height = 8.5, width = 11)
```

#### Size classes

```{r planted-size-classes}
# Calculate numbers of trees in different size bins across planted status
planted_size_classes <- live_trees %>% 
  mutate(IS_PLANTED = case_when(is.na(IS_PLANTED) & 
                                  FIA_LANDUSE == "Forest land" ~ 2,
                                TRUE ~ IS_PLANTED),
         planted_def = case_when(IS_PLANTED == 1 ~ "planted",
                                 IS_PLANTED == 2 ~ "natural regeneration",
                                 IS_PLANTED == 3 ~ "not sure"),
         size_class = cut(DIA, breaks = c(0, 5, 7, 9, 11, Inf), right = FALSE)) %>% 
  group_by(city, planted_def, size_class) %>% 
  count() %>% 
  inner_join(y = tree_city_merge) %>%
  inner_join(y = city_ecoreg)

# Plot
planted_size_classes %>% 
  filter(planted_def != "not sure") %>% 
  ggplot() +
  geom_col(aes(x = reorder(city, lng, decreasing = FALSE),
               y = n, fill = planted_def),
           position = "dodge") +
  scale_fill_manual(name = "Planted", values = c("black", "grey50")) +
  labs(x = "City (ordered by longitude)", y = "Trees in size class") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  facet_grid(rows = vars(size_class),
             cols = vars(classification_3), scales = "free_x", space = "free_x")
ggsave("Results/size_classes_planted_v_not_planted.pdf", height = 7, width = 7)

# Table
planted_size_classes_table <- planted_size_classes %>% 
  select(city, classification_3, planted_def, size_class, n) %>% 
  pivot_wider(names_from = size_class, values_from = n)
planted_size_classes_table %>% 
  kable()
write_csv(planted_size_classes_table, "Results/basal_area_public_v_private.csv")
```

#### Total standing dead

```{r planted-total-dead}
# Numbers of standing dead trees by planted status
planted_dead <- dead_trees %>% 
  mutate(IS_PLANTED = case_when(is.na(IS_PLANTED) & 
                                  FIA_LANDUSE == "Forest land" ~ 2,
                                is.na(IS_PLANTED) & 
                                  FIA_LANDUSE != "Forest land" ~ 3,
                                TRUE ~ IS_PLANTED),
         planted_def = case_when(IS_PLANTED == 1 ~ "planted",
                                 IS_PLANTED == 2 ~ "natural regeneration",
                                 IS_PLANTED == 3 ~ "not sure")) %>% 
  group_by(city, planted_def) %>% 
  count() %>% 
  inner_join(y = tree_city_merge) %>%
  inner_join(y = city_ecoreg)

# Plot dead trees by planted status
planted_dead %>% 
  ggplot() +
  geom_col(aes(x = reorder(city, lng, decreasing = FALSE),
               y = n, fill = planted_def),
           position = "stack") +
  scale_fill_viridis_d(name = "Planted", option = "D") +
  labs(x = "City (ordered by longitude)", y = "Standing dead trees") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  facet_grid(cols = vars(classification_3), scales = "free_x", space = "free_x")
ggsave("Results/standing_dead_planted_v_not_planted.pdf", height = 5, width = 7)

# Output table
planted_dead %>% 
  select(city, classification_3, planted_def, n) %>% 
  kable()
write_csv(public_dead, "Results/standing_dead_public_v_private.csv")
```

#### Standing dead basal area

```{r planted-dead-basal}
# Count the total basal area by planted status
planted_dead_basal <- dead_trees %>% 
  mutate(IS_PLANTED = case_when(is.na(IS_PLANTED) & 
                                  FIA_LANDUSE == "Forest land" ~ 2,
                                is.na(IS_PLANTED) & 
                                  FIA_LANDUSE != "Forest land" ~ 3,
                                TRUE ~ IS_PLANTED),
         planted_def = case_when(IS_PLANTED == 1 ~ "planted",
                                 IS_PLANTED == 2 ~ "natural regeneration",
                                 IS_PLANTED == 3 ~ "not sure"),
         basal_area = 0.00064516*pi*(DIA/2)^2) %>% 
  group_by(city, planted_def) %>% 
  summarize(total_basal = sum(basal_area, na.rm = TRUE)) %>% 
  inner_join(y = tree_city_merge) %>%
  inner_join(y = city_ecoreg)

# Plot total basal area by planted status
planted_dead_basal %>% 
  ggplot() +
  geom_col(aes(x = reorder(city, lng, decreasing = FALSE),
               y = total_basal, fill = planted_def),
           position = "stack") +
  scale_fill_viridis_d(name = "Planted", option = "D") +
  labs(x = "City (ordered by longitude)", y = "Basal area (square meters) of standing dead") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  facet_grid(cols = vars(classification_3), scales = "free_x", space = "free_x")
ggsave("Results/standing_dead_basal_area_planted_v_not_planted.pdf", height = 5, width = 7)

# Table
planted_dead_basal %>% 
  select(city, classification_3, planted_def, total_basal) %>% 
  kable()
write_csv(planted_dead_basal, "Results/basal_area_planted_v_not_planted.csv")
```

### Session info

```{r session-info}
sessionInfo()
```

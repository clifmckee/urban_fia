# "Urban FIA community ecology analysis"
# authors: "Clifton McKee, Meghan Avolio "

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

theme_set(theme_bw(12))

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


# Abundance of trees by ownership and planted Fig 2 ---------------------------------------

#### Ownership of live trees

# Plot the proportion of city trees by ownership
propowner<-trees_coords_merge %>% 
  group_by(city, lat, lng, density, OWNGRPCD) %>% 
  summarize(n = n()) 

percents_for_nancy<-propowner %>% 
  group_by(city) %>% 
  summarise(tot=sum(n)) %>% 
  left_join(propowner) %>% 
  mutate(percent=n/tot)

mean(subset(percents_for_nancy, OWNGRPCD=='Private')$percent)

fig2a<-propowner%>% 
  ggplot() +
  geom_bar(aes(x = reorder(city, lng, decreasing = FALSE),
               y = n,
               fill=factor(OWNGRPCD, levels = c( "OtherFederal","StateLocal", "Private", "Water"))), 
           position = "fill", stat = 'identity') +
  scale_fill_manual(name = "Ownership", limits=c('OtherFederal', 'StateLocal', 'Private', 'Water'), labels=c('Federal', 'State or Local', 'Private', 'Water'), values=c('#0072B2', '#56B4E9', '#E69F00', '#000000')) +
  labs(x = "City (ordered by longitude)", y = "Proportion of trees") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'top')
fig2a


#### Number of planted trees

# What number of trees in each city are planted?
propPlanted <- trees_coords_merge %>% 
  mutate(IS_PLANTED = case_when(is.na(IS_PLANTED) & 
                                  FIA_LANDUSE == "Forest land" ~ 2,
                                TRUE ~ IS_PLANTED),
         planted_def = case_when(IS_PLANTED == 1 ~ "Planted",
                                 IS_PLANTED == 2 ~ "Natural Regeneration",
                                 IS_PLANTED == 3 ~ "Unknown")) %>% 
  group_by(city, planted_def) %>% 
  summarize(n = n(), .groups = "drop") %>% 
  inner_join(y = tree_city_merge) %>% 
  group_by(city) %>% 
  mutate(proportion = round(n/sum(n), 2))

mean(subset(propPlanted, planted_def=='Planted')$proportion)


Fig2b<-propPlanted %>% 
  ggplot() +
  geom_col(aes(x = reorder(city, lng, decreasing = FALSE),
               y = proportion,
               fill = factor(planted_def, levels=c('Unknown','Natural Regeneration', 'Planted'))),
           position = "stack") +
  scale_fill_manual(name = "Planted Status", values=c("#000000","#009E73", "#7B3294" )) +
  labs(x = "City (ordered by longitude)", y = "Proportion of trees") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'top') #+
Fig2b

fig2<-ggarrange(fig2a, Fig2b, labels=c('A', 'B'), ncol=1)

ggsave("Results/Figure2.jpeg", height = 8, width = 5)


# ##impact of density and age on prop
# 
# propowner2<-propowner %>% 
#   group_by(city) %>% 
#   mutate(tot=sum(n), prop=n/tot) %>% 
#   left_join(city_age)
# 
# summary(lm(prop~density, data=subset(propowner2, OWNGRPCD=='Private')))
# 
# ggplot(data=subset(propowner2, OWNGRPCD=='Private'), aes(x=density, y=prop))+
#   geom_point()
# 
# 
# summary(lm(prop~est, data=subset(propowner2, OWNGRPCD=='Private')))
# 
# summary(lm(proportion~density, data=subset(propPlanted, planted_def=='Planted')))



# species accumulation ----------------------------------------------------

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



# Extrapolated richness ---------------------------------------------------

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



# Making Figure 3 ---------------------------------------------------------

poolaccum_S <- clean_names(poolaccum_S) %>% 
  left_join(climate2)

for_estimated<-poolaccum_S %>% 
  group_by(city) %>% 
  filter(n==max(n))

extimated_toplot<-pooled_accum_long %>% 
  filter(estimate=='chao') %>% 
  mutate(n_end=200) %>% 
  left_join(for_estimated)

# Plot species accumulation curves by city
accumulationPlot<-poolaccum_S %>% 
  ggplot() +
  geom_ribbon(aes(x = n, ymin = lower, ymax = upper,
                  group = city), alpha = 0.1) +
  geom_line(aes(x = n, y = mean,
                group = city, color=MAP_mm)) +
  scale_color_viridis_c(name = "MAP (mm)", option = "viridis")+

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
  labs(x = "Number of plots with live trees", y = "Number of tree species") +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# Plot species accumulation curves by city WITH EXTRAPOLATION
estmated_plot<-poolaccum_S %>% 
  ggplot() +
  # geom_ribbon(aes(x = n, ymin = lower, ymax = upper,
  #                 group = city), alpha = 0.1) +
  geom_line(aes(x = n, y = mean,
                group = city, color=MAP_mm)) +
    geom_segment(data = extimated_toplot,
               aes(x = n, y = mean,
                   xend = n_end, yend = est,
                   group = city, color=MAP_mm),
               linetype = "dashed") +
  geom_point(data = extimated_toplot,
             aes(x = n_end, y = est,
                 group = city, color=MAP_mm),
             size = 2.5) +
  
  geom_text_repel(data = extimated_toplot,
                   aes(label = paste0(city, " = ", round(est, 0)),
                      x = n_end+5,
                      y = est),
                  nudge_x = 5,
                  min.segment.length = 2,
                  size = 2.5,
                  max.overlaps = 22) +
  
  scale_color_viridis_c(name = "MAP (mm)", option = "viridis")+
  
  labs(x = "Number of plots with live trees", y = "Number of tree species") +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor=element_blank())

fig3<-ggarrange(accumulationPlot, estmated_plot, labels=c('A', 'B'), common.legend = T, ncol=1)

ggsave("Results/Figure3.jpeg", height = 10, width = 8)


# comparing extrapolations ------------------------------------------------

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




# Rarefied Richness and richness comparions-------------------------------------------------------

###rarified richness (sampling same number of plots)
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

#### Species richness (codyn)

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
  inner_join(y = tree_city_merge)

# # Plot richness
# city_diversity  %>% 
#   ggplot() +
#   geom_col(aes(x = reorder(city, lng, decreasing = FALSE),
#                y = richness)) +
#   labs(x = "City (ordered by longitude)", y = "Species richness") +
#   theme_bw(base_size = 10) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   facet_grid(cols = vars(classification_3), scales = "free_x", space = "free_x")
# ggsave("Results/species_richness.pdf", height = 5, width = 7)

# # Plot richness
# city_diversity %>% 
#   select(city, classification_3, richness, Evar) %>% 
#   kable()
# write_csv(city_diversity, "Results/city_diversity.csv")
```

####comparing all ways of doing richness

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


# Richness, Evenness and Beta div by ownership and planted ----------------

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
  left_join(city_coords,  by = c("city" = "camelname")) %>% 
  mutate(metric2=factor(metric, levels=c('rarefied richness', 'evenness', 'beta_diversity')))

diversity_planted<-rare_planted %>% 
  bind_rows(rare_spont) %>% 
  bind_rows(even_planted) %>% 
  bind_rows(even_spont) %>% 
  bind_rows(beta_planted) %>% 
  bind_rows(beta_spont) %>% 
  left_join(city_coords,  by = c("city" = "camelname")) %>% 
  mutate(metric2=factor(metric, levels=c('rarefied richness', 'evenness', 'beta_diversity')))


# Making figure 4 ---------------------------------------------------------

labels<-c('beta_diversity'='Beta Diversity',
          'evenness' = 'Evenness' ,
          'rarefied richness' = 'Rarefied Richness')
a<-
ggplot(data=diversity_owner) +
  geom_col(aes(x = reorder(city, lng, decreasing = FALSE),
               y = div,
               fill = ownership),
           position = "dodge") +
  scale_fill_manual(name = "Ownership", values=c('#E69F00','#0072B2'), labels=c('Private', 'Public')) +
  labs(x = "City (ordered by longitude)", y = "Diversity") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top", panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  facet_wrap(~metric2, ncol=1, scales='free_y', labeller=labeller(metric2=labels))

a

b<-
ggplot(data=diversity_planted) +
  geom_col(aes(x = reorder(city, lng, decreasing = FALSE),
               y = div,
               fill = planted),
           position = "dodge") +
  scale_fill_manual(name = "Planted Status", values=c("#7B3294","#009E73"), labels=c('Planted', 'Natural Regeneration')) +
  labs(x = "City (ordered by longitude)", y = "Diversity") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top", panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  facet_wrap(~metric2, ncol=1, scales='free_y', labeller=labeller(metric2=labels))
b

div_fig<-grid.arrange(a,b, nrow=1)

ggsave('Results\\Diversity_BigFig.jpeg', div_fig, width=10, height=10, units='in')

# 
# div_plant_means<-diversity_planted %>% 
#   group_by(planted, metric) %>% 
#   summarise(mval=mean(div), sd=sd(div), n=length(div)) %>% 
#   mutate(se=sd/sqrt(n))
# 
# div_owner_means<-diversity_owner %>% 
#   group_by(ownership, metric) %>% 
#   summarise(mval=mean(div), sd=sd(div), n=length(div)) %>% 
#   mutate(se=sd/sqrt(n))
# 
# a<-ggplot(data=div_owner_means, aes(x=ownership, y=mval))+
#   geom_bar(stat='identity')+
#   geom_errorbar(aes(ymin=mval-se, ymax=mval+se), width=0.2)+
#   facet_wrap(~metric, ncol=1, scales='free_y')+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#   ylab('Mean Diversity Value')+
#   xlab('Ownership')
# a
# 
# 
# b<-ggplot(data=div_plant_means, aes(x=planted, y=mval))+
#   geom_bar(stat='identity')+
#   geom_errorbar(aes(ymin=mval-se, ymax=mval+se), width=0.2)+
#   facet_wrap(~metric, ncol=1, scales='free_y')+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#   ylab('Mean Diversity Value')+
#   xlab('Planted Status')+
#   annotate('text', x=1.5, y=Inf, vjust=1, label="*", size=8)
# b
# 
# 
# 
# ggsave("Results/diversity_plots.pdf", div_fig, height = 5, width = 7)

# ttest between ownership and planting types ------------------------------


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


# species richness versus climate Figure 5 -----------------------------------------

div_plant<-diversity_planted %>% 
  left_join(climate2)

div_rare<-df_rarified %>% 
  left_join(climate2)

#run four regression models 
summary(lm(div~MAP_mm, data=subset(div_plant, metric=='rarefied richness'&planted=='spontaneous')))
summary(lm(div~Tmin_C, data=subset(div_plant, metric=='rarefied richness'&planted=='spontaneous')))
summary(lm(div~MAP_mm, data=subset(div_plant, metric=='rarefied richness'&planted=='planted')))
summary(lm(div~Tmin_C, data=subset(div_plant, metric=='rarefied richness'&planted=='planted')))

#overall for a city not by planted status
summary(lm(rrichness~MAP_mm, data=subset(div_rare)))
summary(lm(rrichness~MAP_mm, data=subset(div_rare, MAP_mm>500)))
summary(lm(rrichness~Tmin_C, data=subset(div_rare)))

map_rich_plot<-ggplot(data=filter(div_plant, metric=='rarefied richness'), aes(x=MAP_mm, y=div))+
  geom_point(aes(color=planted))+
  geom_smooth(data=filter(div_plant, metric=='rarefied richness'&planted=='spontaneous'), method = 'lm', color='#009E73')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = 'top')+
  ylab('Rarefied Richness')+
  xlab('Annual Precip. (mm)')+
  scale_color_manual(name = "Planted Status", values=c("#7B3294","#009E73"), labels=c('Planted', 'Natural Regeneration'))
map_rich_plot

temp_rich_plot<-ggplot(data=filter(div_plant, metric=='rarefied richness'), aes(x=Tmin_C, y=div))+
  geom_point(aes(color=planted))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = 'top')+
  ylab('Rarefied Richness')+
  xlab(expression(Minimum~Temperature~(degree*C)))+
  scale_color_manual(name = "Planted Status", values=c("#7B3294","#009E73"), labels=c('Planted', 'Natural Regeneration'))
temp_rich_plot

climatefig<-ggarrange(map_rich_plot, temp_rich_plot, common.legend = T, labels=c('A',"B"))
climatefig

ggsave("Results/Fig5.jpeg", climatefig, height = 5, width = 7)


# NMDS analyses and figure 6 ----------------------------------------------


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
  left_join(climate)
bray_mds_species <- data.frame(bray_mds$species) %>% 
  rownames_to_column(var = "scientific")

# Plot
ggplot() +
  geom_point(data = bray_mds_cities, aes(x = MDS1, y = MDS2,color=MAP_mm), shape = 19, size = 2 )+
  scale_color_viridis_c(name = "MAP (mm)", option = "viridis")+
  geom_text_repel(data = bray_mds_cities, aes(x = MDS1, y = MDS2, label = city),size = 2.5, max.overlaps = 20) +
    #scale_color_brewer(name = "Group", palette = "Dark2") +
  theme_bw(base_size = 10) +
  theme(legend.position = "top", panel.grid.major = element_blank(), panel.grid.minor=element_blank())

ggsave("Results/Figure6.jpeg", height = 5, width = 5)



# top 10 species per city RAC and figure 7 --------------------------------

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

#### Rank abundance, top 10 species

# Plot rank abundance of top 10 species by city
merge_rank %>% 
  filter(row_number <= 10) %>%
  ggplot() +
  geom_col(aes(x = factor(row_number),
               y = prop2, fill = name)) +
  geom_text(aes(x = factor(row_number),
                y = prop, label = scientific),
            size = 3,
            angle = 90,
            hjust = 0,
            nudge_y = 0.02) +
  scale_x_discrete(name = "Species Rank") +
  scale_y_continuous(name = "Relative Abundance",
                     limits = c(0, 0.8), breaks = seq(0, 0.8, 0.2)) + 
  scale_fill_manual(name = "Planted Status", values=c( "#7B3294","#009E73","#000000"), labels=c('Planted', 'Natural Regeneration', 'Unknown')) +
  theme_bw(base_size = 10) +
  theme(legend.position = "top", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~city, ncol = 6)

ggsave("Results/Fig7.jpeg", height = 13, width = 11)



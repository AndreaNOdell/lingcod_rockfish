library(tidyverse)
library(RColorBrewer)
library(latex2exp)
library(PNWColors)
library(ggpubr)
library(tidyverse)
library(patchwork)
load("cleaned_data/lingcod_parms.Rdata")
load("cleaned_data/rockfish_parms.Rdata")

# Functions -----------------------
tag_facet2 <-  function(p, open="", close = ")",
                        tag_pool = letters,
                        x = 0, y = 0.5,
                        hjust = 0, vjust = 0.5, 
                        fontface = 2, ...){
  
  gb <- ggplot_build(p)
  lay <- gb$layout$layout
  nm <- names(gb$layout$facet$params$rows)
  
  tags <- paste0(open,tag_pool[unique(lay$COL)],close)
  
  tl <- lapply(tags, grid::textGrob, x=x, y=y,
               hjust=hjust, vjust=vjust, gp=grid::gpar(fontface=fontface))
  
  g <- ggplot_gtable(gb)
  g <- gtable::gtable_add_rows(g, grid::unit(1,"line"), pos = 0)
  lm <- unique(g$layout[grepl("panel",g$layout$name), "l"])
  g <- gtable::gtable_add_grob(g, grobs = tl, t=1, l=lm)
  grid::grid.newpage()
  grid::grid.draw(g)
}


#fishing_scenarios_pal = c("#56B1F7", "#01579B", "black")
#fishing_scenarios_pal = nmfspalette::nmfs_palette("oceans")(3)
fishing_scenarios_pal = rev(pnw_palette("Starfish",3))
selectivity_pal = c("#b0cbe7", "#a45851")
full_selectivity_pal = c("#5d74a5", "#b0cbe7", "#eba07e", "#a45851")

# 1. Effect of prey-selectivity on yelloweye rockfish equilibrium -----------------
load(file = "results/prey_selectivity/no_prey.RData")
load(file = "results/prey_selectivity/generalist.RData")
load(file = "results/prey_selectivity/intermediate.RData")
load(file = "results/prey_selectivity/specialist.RData")

prey_selectivity_df = as.data.frame(cbind(
  "Spawning Biomass" = c((no_prey$rockfish_avg-no_prey$rockfish_avg)/no_prey$rockfish_avg, 
                         (generalist$rockfish_avg-no_prey$rockfish_avg)/no_prey$rockfish_avg, 
                         (intermediate$rockfish_avg-no_prey$rockfish_avg)/no_prey$rockfish_avg, 
                         (specialist$rockfish_avg-no_prey$rockfish_avg)/no_prey$rockfish_avg),
                                          
  "Stability" = c(-((no_prey$rockfish_cv-no_prey$rockfish_cv)/no_prey$rockfish_cv),
                  -((generalist$rockfish_cv-no_prey$rockfish_cv)/no_prey$rockfish_cv), 
                  -((intermediate$rockfish_cv-no_prey$rockfish_cv)/no_prey$rockfish_cv),
                  -((specialist$rockfish_cv-no_prey$rockfish_cv)/no_prey$rockfish_cv)),
                                          
  "Age Structure" = c((no_prey$rockfish_age-no_prey$rockfish_age)/no_prey$rockfish_age, 
                      (generalist$rockfish_age-no_prey$rockfish_age)/no_prey$rockfish_age, 
                      (intermediate$rockfish_age-no_prey$rockfish_age)/no_prey$rockfish_age,
                      (specialist$rockfish_age-no_prey$rockfish_age)/no_prey$rockfish_age))) %>% 
  mutate(selectivity = c("None", "Generalist", "Intermediate", "Specialist")) %>% 
  pivot_longer(cols = c("Spawning Biomass", "Stability", "Age Structure"), names_to = "outcome", values_to = "value") %>%
  mutate(selectivity = fct_relevel(selectivity, c("None", "Generalist", "Intermediate", "Specialist"))) %>% 
  mutate(outcome = fct_relevel(outcome, c("Spawning Biomass", "Stability", "Age Structure")))

jpeg("plots/final_figures/fig3.tiff", width =18, height = 11, res = 300, units = "cm")
tag_facet2(prey_selectivity_df %>% 
             ggplot(aes(x = selectivity, y = value*100)) +
             theme_classic() +
             geom_hline(yintercept = 0, linetype = 2, color = "grey44") +
             geom_point(aes(fill = selectivity), size = 6, shape = 21, color = "black") +
             facet_wrap( ~ outcome, strip.position = "bottom", scales = "free_x") +
             theme(text = element_text(size = 10),
                   axis.text.x=element_text(angle=45,hjust=1, size = 10),
                   axis.text.y=element_text(size = 10),
                   panel.background = element_rect(fill = "white",
                                                   colour = "white",
                                                   size = 0.5, linewidth = "solid"),
                   panel.border = element_rect(color = "black", fill = NA, size = 0.8),
                   panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                   colour = "gray90"), 
                   panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                                   colour = "gray90"),
                   legend.position = "none",
                   strip.text = element_text(size = 10)) +
             lims(y = c(-80, 5)) +
             scale_fill_manual(values = full_selectivity_pal) +
             labs(x = "", y = "Percent Change Relative to 'None' Scenario")) # +
dev.off()


# 2. Effect of fishing on yelloweye rockfish dynamics -----------------------------------------

# B = 0.05 (low bycatch)
load("results/fishing_scenarios_cont_0724/fishing_extreme_selec.Rdata")
load("results/fishing_scenarios_cont_0724/fishing_base_selec.Rdata")
load("results/fishing_scenarios_cont_0724/fishing.Rdata")

generalist_data = fishing_base_selec
specialist_data = fishing_extreme_selec

# B = 0.1 (RUN ONLY FOR high bycatch scenario) ------------------
load("results/fishing_scenarios_cont_0724/fishing_base_selec_highbycatch.Rdata")
load("results/fishing_scenarios_cont_0724/fishing_extreme_selec_highbycatch.Rdata")
load("results/fishing_scenarios_cont_0724/fishing.Rdata")

generalist_data = fishing_base_selec
specialist_data = fishing_extreme_selec
# ----------------------------------------

# extract data 
generalist_avg = unname(unlist(lapply(generalist_data, `[`, "rockfish_avg")))
generalist_cv = unname(unlist(lapply(generalist_data, `[`, "rockfish_cv")))
generalist_age = unname(unlist(lapply(generalist_data, `[`, "rockfish_age")))

specialist_avg = unname(unlist(lapply(specialist_data, `[`, "rockfish_avg")))
specialist_cv = unname(unlist(lapply(specialist_data, `[`, "rockfish_cv")))
specialist_age = unname(unlist(lapply(specialist_data, `[`, "rockfish_age")))

# Determing F40
lingcod_generalist_avg_df = (lapply(generalist_data, `[`, "lingcod_biomass_ts"))
lincod_generalist_avg = numeric(length(fishing))
for(i in 1:length(lincod_generalist_avg)) {
  lincod_generalist_avg[i] = mean(apply(lingcod_generalist_avg_df[[i]]$lingcod_biomass_ts[,-(1:150)], 1, mean))
}
cbind(SB = round(lincod_generalist_avg/lincod_generalist_avg[1],2), fishing = round(fishing,2))

lingcod_specialist_avg_df = (lapply(specialist_data, `[`, "lingcod_biomass_ts"))
lincod_specialist_avg = numeric(length(fishing))
for(i in 1:length(lincod_specialist_avg)) {
  lincod_specialist_avg[i] = mean(apply(lingcod_specialist_avg_df[[i]]$lingcod_biomass_ts[,-(1:150)], 1, mean))
}
cbind(SB = round(lincod_specialist_avg/lincod_specialist_avg[1],2), fishing = round(fishing,2))

f40 = round(round(fishing,2) / 0.17, 2)

# it looks like SB40% is achieved by F40% = 0.17

# make datasets
df_generalist_avg = cbind((generalist_avg)/generalist_avg[1], fishing, rep("Spawning Biomass", length(fishing)), rep("Generalist"), f40)
df_generalist_cv = cbind(1/((generalist_cv)/generalist_cv[1]), fishing, rep("Stability", length(fishing)), rep("Generalist"), f40)
df_generalist_age = cbind((generalist_age)/generalist_age[1], fishing, rep("Age Structure", length(fishing)), rep("Generalist"), f40)

df_specialist_avg = cbind((specialist_avg)/specialist_avg[1], fishing, rep("Spawning Biomass", length(fishing)), rep("Specialist"), f40)
df_specialist_cv = cbind(1/((specialist_cv)/specialist_cv[1]), fishing, rep("Stability", length(fishing)), rep("Specialist"), f40)
df_specialist_age = cbind((specialist_age)/specialist_age[1], fishing, rep("Age Structure", length(fishing)), rep("Specialist"), f40)

# combine data into one master dataset
df = as.data.frame(rbind(df_generalist_avg, df_generalist_cv, df_generalist_age, df_specialist_avg, df_specialist_cv, df_specialist_age))
colnames(df) <- c("value", "fishing", "variable", "prey_selectivity", "f40")
df$value = as.numeric(df$value)
df$fishing = round(as.numeric(df$fishing),3)
df$f40 = as.numeric(df$f40)

fishing_vals = c(0, 0.47, 1)

df_point = df %>% 
  filter(f40 %in% fishing_vals) %>% 
  mutate("variable" = fct_relevel(variable,
                                  "Spawning Biomass", "Stability", "Age Structure")) %>% 
  mutate(fishing_scenario = f40) %>% 
  mutate(fishing_scenario = replace(fishing_scenario, fishing_scenario == 0.00, "No fishing")) %>% 
  mutate(fishing_scenario = replace(fishing_scenario, fishing_scenario == 0.47, "0.5 F40")) %>% 
  mutate(fishing_scenario = replace(fishing_scenario, fishing_scenario == 1, "F40")) %>% 
  mutate(fishing_scenario = fct_relevel(fishing_scenario, "No fishing", "0.5 F40", "F40"))

# make plot
jpeg("plots/final_figures/fig5.tiff", width =18, height = 11, res = 300, units = "cm")
tag_facet2(df %>% 
             mutate("variable" = fct_relevel(variable,
                                             "Spawning Biomass", "Stability", "Age Structure")) %>%
             mutate(prey_selectivity = fct_relevel(prey_selectivity,
                                                   "Generalist", "Specialist")) %>%
             #filter(prey_selectivity == "Generalist") %>% 
             ggplot(aes(x = f40, y = value, shape = prey_selectivity, linetype = prey_selectivity,
                        group = prey_selectivity)) +
             theme_classic() +
             facet_wrap( ~ variable, strip.position = "bottom", scales = "free_x") +
             geom_line(size = 1) +
             geom_point(data = df_point, mapping = aes(x = f40, y = value,
                                                       fill = fishing_scenario), 
                        size = 5, stroke = 0.5, color = "black") +
             lims(x = c(0,1.5), y = c(0.2, 1.05)) +
             theme(text = element_text(size = 10),
                   axis.text.x=element_text(size = 10),
                   axis.text.y=element_text(hjust=1, size = 10),
                   strip.text = element_text(size = 10),
                   panel.background = element_rect(fill = "white",
                                                   colour = "white",
                                                   size = 0.5, linewidth = "solid"),
                   panel.border = element_rect(color = "black", fill = NA, size = 0.8),
                   panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                   colour = "gray90"), 
                   panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                                   colour = "gray90"),
                   legend.text = element_text(hjust = 0)) +
             scale_shape_manual(values = c(21, 22)) +
             guides(fill = guide_legend(override.aes = list(shape=21))) +
             geom_hline(yintercept = 1, linetype = 2, color = "grey44") +
             scale_fill_manual(values = fishing_scenarios_pal, labels = c("No Fishing", TeX(r'(0.5 $F_{40}$)'), TeX(r'($F_{40}$)'))) +
             labs(x  = TeX(r'(Lingcod F/$F_{40}$)'), y = "Value Relative to 'No Fishing' Scenario", linetype = "Prey Specialization", shape = "Prey Specialization", fill = "Fishing Scenarios", color = "Fishing Scenarios")) 
dev.off()


# 3. Rebuilding Time ----------------------

SBt = 0.4
f_rows = which(f40 %in% fishing_vals)

# generalist
rockfish_generalist_avg_df = lapply(generalist_data, `[`, "rockfish_biomass_ts")
no_fishing_generalist_df <- rockfish_generalist_avg_df[[f_rows[1]]]$rockfish_biomass_ts/apply(rockfish_generalist_avg_df[[f_rows[1]]]$rockfish_biomass_ts[,-(1:150)], 1, mean)
light_fishing_generalist_df <- rockfish_generalist_avg_df[[f_rows[2]]]$rockfish_biomass_ts/apply(rockfish_generalist_avg_df[[f_rows[1]]]$rockfish_biomass_ts[,-(1:150)], 1, mean)
mod_fishing_generalist_df<- rockfish_generalist_avg_df[[f_rows[3]]]$rockfish_biomass_ts/apply(rockfish_generalist_avg_df[[f_rows[1]]]$rockfish_biomass_ts[,-(1:150)], 1, mean)

nofishing_SB40_generalist = as.data.frame(which(no_fishing_generalist_df >= SBt,arr.ind = T)) %>% 
  group_by(row) %>% 
  summarise(t_SB40 = min(col)) %>% 
  mutate(fishing = "none")
median(nofishing_SB40_generalist$t_SB40); sd(nofishing_SB40_generalist$t_SB40)

lightfishing_SB40_generalist = as.data.frame(which(light_fishing_generalist_df >= SBt,arr.ind = T)) %>% 
  group_by(row) %>% 
  summarise(t_SB40 = min(col)) %>% 
  mutate(fishing = "light")
median(lightfishing_SB40_generalist$t_SB40); sd(lightfishing_SB40_generalist$t_SB40)

modfishing_SB40_generalist = as.data.frame(which(mod_fishing_generalist_df >= SBt,arr.ind = T)) %>%
  group_by(row) %>% 
  summarise(t_SB40 = min(col)) %>% 
  mutate(fishing = "moderate")
median(modfishing_SB40_generalist$t_SB40); sd(modfishing_SB40_generalist$t_SB40)

# Specialist
rockfish_specialist_avg_df = lapply(specialist_data, `[`, "rockfish_biomass_ts")
no_fishing_specialist_df <- rockfish_specialist_avg_df[[f_rows[1]]]$rockfish_biomass_ts/apply(rockfish_specialist_avg_df[[f_rows[1]]]$rockfish_biomass_ts[,-(1:150)], 1, mean)
light_fishing_specialist_df <- rockfish_specialist_avg_df[[f_rows[2]]]$rockfish_biomass_ts/apply(rockfish_specialist_avg_df[[f_rows[1]]]$rockfish_biomass_ts[,-(1:150)], 1, mean)
mod_fishing_specialist_df<- rockfish_specialist_avg_df[[f_rows[3]]]$rockfish_biomass_ts/apply(rockfish_specialist_avg_df[[f_rows[1]]]$rockfish_biomass_ts[,-(1:150)], 1, mean)

nofishing_SB40_specialist = as.data.frame(which(no_fishing_specialist_df >= SBt, arr.ind = T)) %>%
  group_by(row) %>% 
  summarise(t_SB40 = min(col)) %>% 
  mutate(fishing = "none")
median(nofishing_SB40_specialist$t_SB40); sd(nofishing_SB40_specialist$t_SB40)

lightfishing_SB40_specialist = as.data.frame(which(light_fishing_specialist_df >= SBt,arr.ind = T)) %>% 
  group_by(row) %>% 
  summarise(t_SB40 = min(col)) %>% 
  mutate(fishing = "light")
median(lightfishing_SB40_specialist$t_SB40); sd(lightfishing_SB40_specialist$t_SB40)

modfishing_SB40_specialist = as.data.frame(which(mod_fishing_specialist_df >= SBt,arr.ind = T)) %>%
  group_by(row) %>% 
  summarise(t_SB40 = min(col)) %>% 
  mutate(fishing = "moderate")
median(modfishing_SB40_specialist$t_SB40); sd(modfishing_SB40_specialist$t_SB40)


# combine datasets
SB40_df_generalist = as.data.frame(rbind(nofishing_SB40_generalist, lightfishing_SB40_generalist, modfishing_SB40_generalist)) %>% 
  mutate(selectivity = "Generalist")
SB40_df_specialist = as.data.frame(rbind(nofishing_SB40_specialist, lightfishing_SB40_specialist, modfishing_SB40_specialist)) %>% 
  mutate(selectivity = "Specialist")
SB40_df = as.data.frame(rbind(SB40_df_generalist,SB40_df_specialist))

tiff("plots/final_figures/fig6.tiff", width =8.5, height = 7, res = 400, units = "cm") 
dodge <- position_dodge(width = 0.7)
SB40_df %>% 
  mutate(selectivity = fct_relevel(selectivity, 
                                   "Generalist", "Specialist")) %>% 
  mutate(fishing = fct_relevel(fishing, "none", "light", "moderate")) %>% 
  ggplot(aes(x=fishing, y=t_SB40, fill = selectivity)) +  #fill = fishing, color = fishing
  geom_boxplot(notch = FALSE, linewidth = 0.3, outlier.size = 0.3) +
  #ylim(0,100) +
  theme_classic() +
  lims(y = c(5,40)) +
  labs(x = "Lingcod Fishing Scenario", y = TeX(r'(Years until Yelloweye reaches 40% $SB_0$)'), fill = "Prey Specialization") + 
  guides(color = FALSE, alpha = FALSE) + 
  theme(text = element_text(size = 6),
        axis.text.x=element_text(size = 6),
        axis.text.y=element_text(size = 6),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linewidth = "solid"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray90"), 
        panel.grid.minor = element_line(size = 0.15, linetype = 'solid',
                                        colour = "gray90"),
        legend.direction = "horizontal", 
        legend.position = c(0.5,0.87),
        legend.box = "horizontal",
        legend.background = element_rect(color = "black", linewidth = 0.25),
        legend.margin = margin(0.05, 0.15, 0.05, 0.15, unit = "cm")) +
  scale_fill_manual(values = selectivity_pal) +
  scale_x_discrete(labels = c("No Fishing", TeX(r'(0.5 $F_{40}$)'), TeX(r'($F_{40}$)'))) 
dev.off()


# 4. Deterministic timeseries of dynamics -----------------

load("results/deterministic/ts_deterministic.RData")
load("results/deterministic/ts_deterministic_specialist.RData")

ts_deterministic = ts_deterministic_specialist

# time-series of spawning biomass for yelloweye and lingcod during historical fishing period
lingcod_ts = rbind(c(colSums(ts_deterministic[[1]]$det.burn.in[1:20,]*lingcod$weight.at.age[1:20]*lingcod$mat.at.age[1:20]) / ts_deterministic[[1]]$lingcod_biomass_ts[1,300], ts_deterministic[[1]]$lingcod_biomass_ts[1,]/ ts_deterministic[[1]]$lingcod_biomass_ts[1,300]),
c(colSums(ts_deterministic[[2]]$det.burn.in[1:20,]*lingcod$weight.at.age[1:20]*lingcod$mat.at.age[1:20]) / ts_deterministic[[1]]$lingcod_biomass_ts[1,300], ts_deterministic[[2]]$lingcod_biomass_ts[1,]/ ts_deterministic[[1]]$lingcod_biomass_ts[1,300]),
c(colSums(ts_deterministic[[3]]$det.burn.in[1:20,]*lingcod$weight.at.age[1:20]*lingcod$mat.at.age[1:20]) / ts_deterministic[[1]]$lingcod_biomass_ts[1,300], ts_deterministic[[3]]$lingcod_biomass_ts[1,]/ ts_deterministic[[1]]$lingcod_biomass_ts[1,300]))

rockfish_ts = cbind(c(((colSums(ts_deterministic[[1]]$det.burn.in[41:nrow(ts_deterministic[[1]]$det.burn.in),]*rockfish$weight.at.age*rockfish$mat.at.age))/2)/ ts_deterministic[[1]]$rockfish_biomass_ts[1,300], 
               ts_deterministic[[1]]$rockfish_biomass_ts[1,]/ ts_deterministic[[1]]$rockfish_biomass_ts[1,300]),
c(((colSums(ts_deterministic[[2]]$det.burn.in[41:nrow(ts_deterministic[[1]]$det.burn.in),]*rockfish$weight.at.age*rockfish$mat.at.age))/2)/ ts_deterministic[[1]]$rockfish_biomass_ts[1,300], 
  ts_deterministic[[2]]$rockfish_biomass_ts[1,]/ ts_deterministic[[1]]$rockfish_biomass_ts[1,300]),
c(((colSums(ts_deterministic[[3]]$det.burn.in[41:nrow(ts_deterministic[[1]]$det.burn.in),]*rockfish$weight.at.age*rockfish$mat.at.age))/2)/ ts_deterministic[[1]]$rockfish_biomass_ts[1,300], 
  ts_deterministic[[3]]$rockfish_biomass_ts[1,]/ ts_deterministic[[1]]$rockfish_biomass_ts[1,300]))

rockfish_ts = as.data.frame(rockfish_ts) %>% 
  mutate(species = "rockfish", year = 1:nrow(rockfish_ts)) 
colnames(rockfish_ts) = c("none", "0.5F40", "F40", "species", "year")
rockfish_ts = as.data.frame(pivot_longer(data = rockfish_ts, cols = 1:3, names_to = "Fishing_Scenario", values_to = "Relative_SB")) %>% 
  mutate(Fishing_Scenario = fct_relevel(Fishing_Scenario,
                                        "none", "0.5F40", "F40")) 


p1 = ggplot(rockfish_ts[rockfish_ts$Relative_SB <= 1.05,], aes(x = year, y = Relative_SB)) +
  geom_line(aes(col = Fishing_Scenario), size = 2) +
  scale_color_manual(values = fishing_scenarios_pal, labels = c("No Fishing", TeX(r'(0.5 $F_{40}$)'), TeX(r'($F_{40}$)'))) + 
  theme(axis.text.x=element_text(size = 10),
        axis.text.y=element_text(size = 10),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linewidth = "solid"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.8),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "gray90"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray90")) +
  lims(y = c(0,1.01)) +
  labs(x = "Years", y = TeX(r'(Yelloweye SB/$SB_{0}$)'), color = "Fishing Scenarios")


# timeseries of fishing mortality
hist.f = 0.4
hist.by = 0.13
f_mort_timeseries = as.data.frame(cbind(year = 1:450, 
                          "none" = c(rep(hist.f,100), rep(fishing_vals[1], 350)),
                          "0.5F40" = c(rep(hist.f,100), rep(0.5, 350)),
                          "F40" = c(rep(hist.f,100), rep(fishing_vals[3], 350))))

all_f_morts = data.frame(x = 100,
                         y = f40,
                         xend = 450,
                         yend = f40)

jpeg("plots/final_figures/F_timeseries_complete.jpeg", units="in", width=5.5, height=3, res = 300)
p2 = f_mort_timeseries %>%
  pivot_longer(2:4, names_to = "fishing_scenario", values_to = "f") %>% 
  mutate(fishing_scenario = fct_relevel(fishing_scenario,
                                        "none", "0.5F40", "F40")) %>% 
  ggplot(aes(x = year, y = f, color = fishing_scenario)) +
  geom_segment(data = all_f_morts, mapping = aes(x=x, y=y, xend=xend, yend=yend), inherit.aes = FALSE, col = "grey29") +
  geom_segment(aes(x=100, y=0.9, xend=100, yend=2.35), inherit.aes = FALSE, col = "grey29") +
  geom_line(size = 2) +
  scale_color_manual(values = fishing_scenarios_pal, labels = c("No Fishing", TeX(r'(0.5 $F_{40}$)'), TeX(r'($F_{40}$)'))) +
  theme(axis.text.x=element_text(size = 10),
        axis.text.y=element_text(size = 10),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linewidth = "solid"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.8),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "gray90"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray90"),
        legend.title = element_text(size = 9)) +
  labs(x = "Years", y = TeX(r'(Lingcod F/$F_{40}$)'), color = "Management-Relevent \n Fishing Scenarios")
dev.off()

tiff("plots/final_figures/fig2.tiff", width =18, height = 9, res = 300, units = "cm")
ggarrange(p2,p1, ncol = 2, nrow = 1, common.legend = TRUE, labels = c("a)", "b)"), font.label = list(size = 12))
dev.off()



# 5. Assess Transient Dynamics ---------------------

load(file = "results/prey_selectivity/no_prey.RData")
load(file = "results/prey_selectivity/generalist.RData")
load(file = "results/prey_selectivity/intermediate.RData")
load(file = "results/prey_selectivity/specialist.RData")

trans_dat = data.frame(Generalist = colMeans(generalist$rockfish_biomass_ts)/max(colMeans(no_prey$rockfish_biomass_ts)),
                       Intermediate = colMeans(intermediate$rockfish_biomass_ts)/max(colMeans(no_prey$rockfish_biomass_ts)),
                       Specialist = colMeans(specialist$rockfish_biomass_ts)/max(colMeans(no_prey$rockfish_biomass_ts)),
                       None = colMeans(no_prey$rockfish_biomass_ts)/max(colMeans(no_prey$rockfish_biomass_ts))) %>%
  mutate(timestep = 1:length(colMeans(no_prey$rockfish_biomass_ts))) %>% 
  pivot_longer(cols = c(1:4), values_to = "relSB", names_to = "Scenario") %>% 
  mutate(selectivity = as.factor(Scenario),
         selectivity = fct_relevel(selectivity, c("None", "Generalist", "Intermediate", "Specialist")))

tiff("plots/final_figures/FigureS1.tiff", units="in", width=4.5, height=3, res = 300)
ggplot(trans_dat, aes(x = timestep, y = relSB)) +
  geom_line(aes(col = selectivity), linewidth = 1.5) + theme_bw() +
  labs(y = TeX(r'(Yelloweye SB/$SB_{0}$)'), col = "Prey Specialization") +
  scale_color_manual(values = full_selectivity_pal) + 
  theme(text = element_text(size = 8)) +
  lims(x = c(0,150))
dev.off()


# Age structure
load("results/deterministic/ts_det_generalist_age.RData")


p1 = data.frame(no_fish = ts_det_generalist_age[[1]]$rockfish_age_str_ts[1,]/ts_det_generalist_age[[1]]$rockfish_age_str_ts[1,355],
                mod_fish = ts_det_generalist_age[[2]]$rockfish_age_str_ts[1,]/ts_det_generalist_age[[1]]$rockfish_age_str_ts[1,355],
                high_fish = ts_det_generalist_age[[3]]$rockfish_age_str_ts[1,]/ts_det_generalist_age[[1]]$rockfish_age_str_ts[1,355]) %>% 
  mutate(timestep = 1:length(ts_det_generalist_age[[3]]$rockfish_age_str_ts[1,])) %>% 
  pivot_longer(c(1:3), names_to = "Fishing", values_to = "AgeStruct") %>% 
  mutate(fishing_scenario = as.factor(Fishing),
         fishing_scenario = fct_relevel(fishing_scenario, "no_fish", "mod_fish", "high_fish")) %>% 
  ggplot(aes(x = timestep, y = AgeStruct)) +
  geom_line(aes(col = fishing_scenario), linewidth = 1.5) +
  theme_bw() + 
  scale_color_manual(values = fishing_scenarios_pal, 
                     labels = c("No Fishing", TeX(r'(0.5 $F_{40}$)'), TeX(r'($F_{40}$)'))) +
  labs(y = "Age Structure relative to 'No Fishing' scenario", col = "Fishing Scenario") + 
  theme(text = element_text(size = 8)) +
  lims(x = c(0,150))


p2 = data.frame(no_fish = ts_det_generalist_age[[1]]$rockfish_biomass_ts[1,]/ts_det_generalist_age[[1]]$rockfish_biomass_ts[1,355],
                mod_fish = ts_det_generalist_age[[2]]$rockfish_biomass_ts[1,]/ts_det_generalist_age[[1]]$rockfish_biomass_ts[1,355],
                high_fish = ts_det_generalist_age[[3]]$rockfish_biomass_ts[1,]/ts_det_generalist_age[[1]]$rockfish_biomass_ts[1,355]) %>% 
  mutate(timestep = 1:length(ts_det_generalist_age[[3]]$rockfish_biomass_ts[1,])) %>% 
  pivot_longer(c(1:3), names_to = "Fishing", values_to = "SB") %>% 
  mutate(fishing_scenario = as.factor(Fishing),
         fishing_scenario = fct_relevel(fishing_scenario, "no_fish", "mod_fish", "high_fish")) %>% 
  ggplot(aes(x = timestep, y = SB)) +
  geom_line(aes(col = fishing_scenario), linewidth = 1.5) +
  theme_bw() + 
  scale_color_manual(values = fishing_scenarios_pal, 
                     labels = c("No Fishing", TeX(r'(0.5 $F_{40}$)'), TeX(r'($F_{40}$)'))) +
  labs(y = "Spawning Biomass relative to 'No Fishing' scenario", col = "Fishing Scenario") + 
  theme(text = element_text(size = 8)) +
  lims(x = c(0,150))

tiff("plots/final_figures/FigureS2.tiff", units="in", width=6, height=3, res = 300)
p2 + p1 + plot_layout(guides = 'collect') + plot_annotation(tag_levels = "a", tag_suffix = ")")
dev.off()



# 6. Mortality sensitivity Analysis -----------------------------------
load("results/deterministic/generalist_baseM.RData")
load("results/deterministic/specialist_baseM.RData")
load("results/deterministic/specialist_95M.RData")
load("results/deterministic/specialist_80M.RData")
dat_M = data.frame(base = specialist_baseM$rockfish_biomass_ts[1,]/generalist_baseM$rockfish_biomass_ts[1,350],
                   M95 = specialist_95M$rockfish_biomass_ts[1,]/generalist_baseM$rockfish_biomass_ts[1,350],
                   M80 = specialist_80M$rockfish_biomass_ts[1,]/generalist_baseM$rockfish_biomass_ts[1,350],
                   generalist = generalist_baseM$rockfish_biomass_ts[1,]/generalist_baseM$rockfish_biomass_ts[1,350],
                   timestep = 1:length(specialist_baseM$rockfish_biomass_ts[1,])) %>% 
  pivot_longer(c(1:4), names_to = "Scenario", values_to = "SB") %>% 
  mutate(Scenario = as.factor(Scenario),
         Scenario = fct_relevel(Scenario, "generalist", "base", "M95", "M80"))

tiff("plots/final_figures/FigureS5.tiff", units="in", width=5, height=3, res = 300)
ggplot(dat_M, aes(x = timestep, y = SB)) +
  geom_line(aes(col = Scenario), linewidth = 2) + theme_bw() +
  scale_color_manual(values = c("#a45851", "#b0cbe7", "#537380", "#33454e"), 
                     labels = c(TeX(r'($M^{Y}$ - Generalist)'), TeX(r'($M^{Y}$ - Specialist)'), 
                                TeX(r'($0.95M^{Y}$ - Specialist)'), TeX(r'($0.8M^{Y}$ - Specialist)'))) +
  labs(y = "Spawning Biomass relative to generalist scenario", 
       col = "Natural Mortality Scenario") +
  theme(text = element_text(size = 8)) +
  lims(x = c(0,150))
dev.off()


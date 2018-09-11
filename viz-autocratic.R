# graphics for autocratic systems

library(rio)
library(tidyverse)

gwf <- import("data/GWF280.dta")

gwf_ts <- import("data/GWFtscs12.dta")

head(gwf)



# average regime spell

ggplot(data = gwf) +
  geom_bar(mapping = aes(x = regtype, y = gwf_spell, fill = regtype), stat = "summary", fun.y = "mean") +
  labs(x = "Regime type", y = "Average regime spell (years)", fill = "Regime type") +
  theme_classic() +
  guides(fill=FALSE) +
  theme(panel.border = element_blank(), text = element_text(size=16)) + 
  scale_y_continuous(breaks = seq(0,65,10))

ggsave("input/regime_spell.png", width = 8, height = 6, unit = "in")  

# regimes types over time

vdem <- read.dta13("/Users/xtanma/Dropbox/autocratic-trust-bias/data/vdem_v8.dta")

vdem %>% 
  select(country_name, country_text_id, year, e_wr_regtype, v2x_polyarchy, v2mecenefm) %>% 
  filter(year >= 1900) 

library(Hmisc) # to get the capitalize function

vdem <- vdem %>% 
  mutate(regt = e_wr_regtype,
         regt = ifelse(regt == "indirect military" | regt == "military-personal", "military", 
                ifelse(regt=="party-personal" | regt=="oligarchy" |regt=="party-military" | regt =="party-military-personal", "party",
                       ifelse(regt=="", NA, regt))), 
         regt = capitalize(regt))


ggplot(vdem %>% filter(!is.na(regt)), aes(year, colour = regt)) +
  geom_freqpoly(binwidth = 1,  size = 1.3) +
  xlim(1946, 2010) +
  labs(x="Year", y="Number of countries", colour = "Regime type") + 
  theme_classic() +
  theme(panel.border = element_blank(), text = element_text(size=16)) +
  
ggsave("input/regimes_timetrend.png", width = 8, height = 6, unit = "in")


# mest aktoritär?

ggplot(vdem  %>% filter(!is.na(regt)), mapping = aes(x = regt, y = v2x_polyarchy, fill = regt)) +
  geom_boxplot() +
  ylim(0, 0.7) +
  labs(x = "Regime type", y = "Level of Democracy (V-Dem)", fill = "Regime type") +
  theme_classic() +
  theme(panel.border = element_blank(), text = element_text(size=16)) +
  guides(fill=FALSE) 
  
ggsave("input/how_democratic.png", width = 8, height = 6, unit = "in")


# mest censur?

ggplot(vdem  %>% filter(!is.na(regt)), mapping = aes(x = regt, y = v2mecenefm, fill = regt)) +
  geom_boxplot() +
  ylim(0, 0.7) +
  labs(x = "Regime type", y = "Level of Censorship (V-Dem)", fill = "Regime type") +
  theme_classic() +
  theme(panel.border = element_blank(), text = element_text(size=16)) +
  guides(fill=FALSE) 

ggsave("regtype_censorship.png", width = 8, height = 6, unit = "in")



  
# vad händer när regimen imploderar? 

dem_col <- c("#e08214", "#8073ac")
 

ggplot(gwf_ts %>% filter(gwf_fail == 1 & gwf_fail_subsregime != 3)) +
  geom_bar(mapping = aes(x = regime_type, fill = as.factor(gwf_fail_subsregime)), position = "dodge") +
  labs(x = "", y = "No. of regime transitions", fill = "Transition to") +
  scale_fill_manual(name ="Transition to:",
                    values = dem_col,
                    breaks = c("1","2"),
                    labels = c("Democracy", "Autocracy")) +
  theme_classic() +
  theme(panel.border = element_blank(), text = element_text(size=16)) 

ggsave("input/regime_transition.png", width = 8, height = 6, unit = "in")


# våldsamt?

death_col <- c('#fed976','#fd8d3c','#f03b20','#bd0026')

ggplot(gwf_ts %>% filter(gwf_fail == 1)) +
  geom_bar(mapping = aes(x = regime_type, fill = as.factor(gwf_fail_violent)), position = "dodge") +
  labs(x = "", y = "No. of regime transitions", fill = "Deaths") +
  theme_classic() +
  theme(panel.border = element_blank(), text = element_text(size=16)) +
  scale_fill_manual(name ="Nr. deaths",
                    values = death_col,
                    breaks = c("1","2","3","4"),
                    labels = c("0", "1 to 25", "26 to 1000", "1000+"))

ggsave("input/transition_violence.png", width = 8, height = 6, unit = "in")

# map

#source(intro-gif.R)



ggplot(data=world_df %>% filter(year==2017)) + 
  geom_polygon(aes(long,lat, group=group, fill=as.factor(v2x_regime))) + 
  scale_fill_manual(name ="",
                    values = row_col,
                    breaks = rev(c("0","1","2","3")),
                    labels = rev(c("Closed Autocracy", "Electoral Autocracy", "Electoral Democracy", "Liberal Democracy"))) +
  #guides(fill=FALSE) +
  xlab("") + 
  ylab("") + 
  labs(title = "") +
  theme(panel.border = element_blank()) + 
  theme(panel.background = element_blank()) + 
  theme(axis.ticks = element_blank()) + 
  theme(axis.text = element_blank())+
  theme(legend.position="top")

ggsave("row_2017.png", width = 8, height = 6, units = "in")



# censorship 

ggplot(data=vdem %>% filter(year==2017), mapping = aes(v2x_polyarchy, v2mecenefi, color=v2mecenefi)) +
  geom_point(size = 2.5) +
  labs(x = "Level of Democracy (V-Dem)", y = "Internet censorship") +
  guides(color=FALSE) +
  theme_classic() +
  theme(panel.border = element_blank(), text = element_text(size=16))

ggsave("input/censorship_internet.png", width = 8, height = 6, units = "in")


ggplot(data=vdem %>% filter(year==2017), mapping = aes(v2x_polyarchy, v2mecenefm, color=v2mecenefi)) +
  geom_point(size = 2.5) +
  labs(x = "Level of Democracy (V-Dem)", y = "Internet censorship") +
  guides(color=FALSE) +
  theme_classic() +
  theme(panel.border = element_blank(), text = element_text(size=16))





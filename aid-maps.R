## Aid Maps
library(rio)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(maptools)
library(cartogram)
library(broom) # from goespatial to data frame

# Get map of Africa
data(wrld_simpl)
afr=wrld_simpl[wrld_simpl$REGION==2,]
plot(afr)

#let make that ggplotable 
afr_df <- tidy(afr)
head(afr_df)
world <- tidy(wrld_simpl)

ggplot(world %>% filter(id!="ATA")) +
  geom_polygon(aes(long,lat, group=group), fill="grey65")

# lets get some aid data  
china <- import("data/china-aid.csv")
#head(china)

china <- china %>% 
  select(recipient_iso3, crs_sector_name, crs_sector_code, year, usd_current, recipient_condensed, project_total_commitments, year) %>% 
  group_by(recipient_iso3) %>% 
  summarize(value = sum(usd_current, na.rm = T)/1000000)
china <- filter(china, value != 0) 

#join data to map
afr_cn <- left_join(afr_df, china, by=c("id"="recipient_iso3"))

ggplot() +
  geom_polygon(data = afr_cn, aes(fill = value, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  theme_void() +
  scale_fill_viridis(name = "Chinese Aid \nUSD million") +  
  #scale_fill_continuous(name = "Chinese Aid", low='thistle2', high='darkblue', guide='colorbar') +  
  labs(title = "Chinese aid to Africa 2000-15") +
  ylim(-35,35) +
  theme(
    text = element_text(color = "#22211d"), 
    plot.background = element_blank(), 
    panel.background = element_blank(), 
    legend.background = element_blank(),
    plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 13, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    legend.position = c(0.2, 0.26)
  ) +
  NULL
ggsave("input/china-africa.png", width = 6, height = 5, units = "in")

# and logged
ggplot() +
  geom_polygon(data = afr_cn, aes(fill = log(value), x = long, y = lat, group = group) , size=0, alpha=0.9) +
  theme_void() +
  scale_fill_viridis(name = "Chinese Aid \nUSD million (log)") +  
  #scale_fill_continuous(name = "Chinese Aid", low='thistle2', high='darkblue', guide='colorbar') +  
  labs(title = "Chinese aid to Africa 2000-15") +
  ylim(-35,35) +
  theme(
    text = element_text(color = "#22211d"), 
    plot.background = element_blank(), 
    panel.background = element_blank(), 
    legend.background = element_blank(),
    plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 13, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    legend.position = c(0.2, 0.26)
  ) +
  NULL
ggsave("input/china-africa-log.png", width = 6, height = 5, units = "in")



world_cn <- left_join(world, china, by=c("id"="recipient_iso3"))

ggplot() +
  geom_polygon(data = world_cn %>% filter(id!="ATA"), aes(fill = log(value), x = long, y = lat, group = group) , size=0, alpha=0.9) +
  theme_void() +
  scale_fill_viridis(name = "USD (Log)") +  
  #scale_fill_continuous(name = "Chinese Aid", low='thistle2', high='darkblue', guide='colorbar') + 
  labs(title = "") +
  theme(
  text = element_text(color = "#22211d"), 
  plot.background = element_blank(), 
  panel.background = element_blank(), 
  legend.background = element_blank(),
  plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  plot.subtitle = element_text(size= 13, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  legend.position = c(0.2, 0.26)
)
ggsave("input/china-world-log.png", width = 7, height = 5, units = "in")



## And now lets get some OECD data
library(countrycode)

oecd <- import("data/oecd.csv")
names(oecd)

oecd_aid <- oecd %>% 
  filter(Donor == "All Donors, Total") %>% 
  mutate(id_iso3 = countrycode(Recipient, "country.name", "iso3c"))
  

oecd_aid <- oecd_aid %>% 
  filter(SECTOR != 150 & SECTOR != 151) %>% 
  group_by(id_iso3) %>% 
  summarise(value = sum(Value, na.rm=T))

#join with maps
afr_oecd <- left_join(afr_df, oecd_aid, by=c("id"="id_iso3"))

ggplot() +
  geom_polygon(data = afr_oecd, aes(fill = value, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  theme_void() +
  scale_fill_viridis(name = "Democracy Aid (OECD) \nUSD million") +  
  labs(title = "Democracy aid to Africa 2008-18", subtitle="") +
  ylim(-35,35) +
  theme(
    text = element_text(color = "#22211d"), 
    plot.background = element_blank(), 
    panel.background = element_blank(), 
    legend.background = element_blank(),
    plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 13, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    legend.position = c(0.2, 0.26)
  ) +
  NULL
ggsave("input/oecd-africa.png", width = 6, height = 5, units = "in")

#and the world
world_oecd <- left_join(world, oecd_aid, by=c("id"="id_iso3"))


ggplot() +
  geom_polygon(data = world_oecd %>% filter(id!="ATA"), aes(fill = value, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  theme_void() +
  scale_fill_viridis_c(name = "USD m.") +
  labs(title = "", subtitle="") +
  theme(
    text = element_text(color = "#22211d"), 
    plot.background = element_blank(), 
    panel.background = element_blank(), 
    legend.background = element_blank(),
    plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 13, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    legend.position = c(0.2, 0.26)
  )
ggsave("input/oecd-world.png", width = 7, height = 5, units = "in")

# and logged
ggplot() +
  geom_polygon(data = world_oecd %>% filter(id!="ATA"), aes(fill = log(value), x = long, y = lat, group = group) , size=0, alpha=0.9) +
  theme_void() +
  scale_fill_viridis_c(name = "USD m. (log)") +
  labs(title = "", subtitle="") +
  theme(
    text = element_text(color = "#22211d"), 
    plot.background = element_blank(), 
    panel.background = element_blank(), 
    legend.background = element_blank(),
    plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 13, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    legend.position = c(0.2, 0.26)
  )
ggsave("input/oecd-world-log.png", width = 7, height = 5, units = "in")



# Which regime types recieve the most aid
vdem <- import("data/vdem_v8_lite.dta") %>% filter(year > 2007)

oecd_aid <- oecd %>% 
  filter(Donor == "All Donors, Total") %>% 
  mutate(id_iso3 = countrycode(Recipient, "country.name", "iso3c"))

oecd_aid <- oecd_aid %>% 
  filter(SECTOR != 150 & SECTOR != 151) %>% 
  group_by(id_iso3, Year) %>% 
  summarise(value = sum(Value, na.rm=T))

oecd_aid$Year <- as.numeric(oecd_aid$Year)

oecd_aid <- left_join(oecd_aid, vdem, by=c("id_iso3"="country_text_id", "Year"="year"))

row_oecd <- oecd_aid %>% 
  filter(!is.na(v2x_regime)) %>% 
  group_by(v2x_regime, Year) %>% 
  summarise(value = sum(value, na.rm=T))

row_col <- c("#e66101", "#fdb863", "#b2abd2", "#5e3c99")

ggplot(row_oecd, aes(x=Year, y=value, fill=as.factor(v2x_regime))) +
  geom_area(position = "fill") +
  scale_fill_manual(name ="Regime type (RoW)",
                    values = row_col,
                    breaks = c("0","1","2","3"),
                    labels = c("Closed Autocracy", "Electoral Autocracy", "Electoral Democracy", "Liberal Democracy")) +
  theme_classic() + 
  ylab("Share of Democracy aid (OECD)")
  NULL

ggsave("input/oecd-aid-row.png", width = 7, height = 5, units = "in")

#and china?
vdem <- import("data/vdem_v8_lite.dta") %>% filter(year > 1999 & year < 2015)
china_aid <- left_join(china, vdem, by=c("recipient_iso3"="country_text_id"))

row_china <- china_aid %>% 
  filter(!is.na(v2x_regime)) %>% 
  group_by(v2x_regime, year) %>% 
  summarise(value = sum(value, na.rm=T))

row_china
 
ggplot(row_china, aes(x=year, y=value, fill=as.factor(v2x_regime))) +
  geom_area(position = "fill") +
  scale_fill_manual(name ="Regime type (RoW)",
                    values = row_col,
                    breaks = c("0","1","2","3"),
                    labels = c("Closed Autocracy", "Electoral Autocracy", "Electoral Democracy", "Liberal Democracy")) +
  theme_classic() + 
  ylab("Share of Chinese aid") +
NULL

ggsave("input/china-aid-row.png", width = 7, height = 5, units = "in")

# how about GWF regime type?

gwf <- china_aid %>% 
  mutate(regt = e_wr_regtype,
         regt = ifelse(regt == "indirect military" | regt == "military-personal", "military", 
                       ifelse(regt=="party-personal" | regt=="oligarchy" |regt=="party-military" | regt =="party-military-personal", "party",
                              ifelse(regt=="", "Democracy", e_wr_regtype))), 
         regt = capitalize(regt)) %>% 
  filter(year<2011)

gwf <- gwf %>% 
  filter(!is.na(regt) & regt != "Democracy") %>% 
  group_by(regt, year) %>% 
  summarise(value = sum(value, na.rm=T))
gwf <- gwf %>% mutate(year= round(year, 0))


ggplot(gwf, aes(x=year, y=value, fill=as.factor(regt))) +
  geom_area(position = "fill") +
  scale_fill_manual(name ="Regime type (GWF)",
                   values = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")) +
                   #breaks = c("0","1","2","3"),
                   #labels = c("Closed Autocracy", "Electoral Autocracy", "Electoral Democracy", "Liberal Democracy")) +
  theme_classic() + 
  ylab("Share of Chinese aid") +
  scale_x_continuous(breaks = c(2000, 2002, 2004, 2006, 2008, 2010)) +
  NULL
ggsave("input/china-aid-gwf.png", width = 7, height = 5, units = "in")




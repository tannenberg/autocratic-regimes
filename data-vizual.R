library(tidyverse)
library(rio)

oecd <- import("data/oecd.csv")
names(oecd)

table(oecd$Sector)
df <- oecd %>% filter(SECTOR != 150 & SECTOR != 151) %>% 
  group_by(Year, SECTOR, Sector) %>% 
  summarise(value = sum(Value, na.rm = T)) 

df <- df %>% mutate(., type = ifelse(SECTOR == 15113, "Anti-corruption",
                              ifelse(SECTOR==15130, "Judicary", 
                              ifelse(SECTOR==15150, "Civil society", 
                              ifelse(SECTOR==15153, "Media", 
                              ifelse(SECTOR==15170, "Womens equality", 
                              ifelse(SECTOR==15152, "Legislatures & parties",Sector)))))))

ggplot(df, aes(x=Year, y=value, color=as.factor(type))) +
  geom_line() + 
  theme_classic()


ab6 <- import("data/ab6.dta")

ggplot(ab6 %>% filter(Q30 %in% 1:3), aes(x=Q30)) +
  geom_bar()

ab5 <- import("data/ab5.dta")
ab4 <- import("data/ab4.dta")

ab2 <- import("data/ab2.sav")

table(ab2$q38)

## lets do some mapping

cn_aid <- import()






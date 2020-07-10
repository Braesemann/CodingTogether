#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Coding Alone - Trust and Digital Innovation #
# 2020-07-10, Fabian Braesemann
# Output: 
# - World map (Fig 1A)
# - Power law plot (Figure 1B)
# - SO City and Country regression (Figure 1C)
# - Beeswarm plot Affinity, GDP, Trust quartiles (Figure 2A)

#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

library(reshape2)
library(forecast)
library(lubridate)
library("plotly")
library(tidyverse)
library("network")
library("maps")
library(rgeos)
library(rgdal)
library(raster)
library("rnaturalearth")
library(RColorBrewer)
library(ggnetwork)
library("countrycode")
library("scales")
library("ggbeeswarm")
library(ggpubr)

#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Load data
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

load(paste(getwd(), "/coding_alone_top_cities.RData", sep = ""))
load(paste(getwd(), "/coding_alone.RData", sep = ""))

options(stringsAsFactors = F)
df5 <- read.csv(paste(getwd(),"/cities_pop.csv", sep =""),sep = ";")

#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Figure 1A World Map
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

df <- top_cities
df$lon <- as.numeric(df$lon)
df$lat <- as.numeric(df$lat)

countries <- ne_countries(returnclass = "sf")

# This part maps the points to Robinson!
coords=cbind(df$lon,df$lat)

coords2 <- data.frame(coords) # Hier ist der Trick, dass wir die Punkte in Robinson umwandeln

coordinates(coords2) <- c("X1","X2")

proj4string(coords2) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
coords3 <- spTransform(coords2,CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

df$lon <- coords3@coords[,1]
df$lat <- coords3@coords[,2]

countries$Alpha_3 <- countrycode(countries$name,origin = "country.name", destination = "iso3c")

df2 <- merge(countries, df, by.x = "Alpha_3", by.y = "iso3", all.x = T)


vir <- brewer.pal(5, "Blues")
vir2 <- brewer.pal(5, "Oranges")

df$contributions_city2 <- ifelse(df$contributions_city < 2, 1,
                                 ifelse(df$contributions_city > 1 & df$contributions_city < 11, 10,
                                 ifelse(df$contributions_city > 10 & df$contributions_city < 101, 100,
                                        ifelse(df$contributions_city > 100 & df$contributions_city < 1001, 1000, 10000))))

summary(df$trust)

df2$trust2 <- ifelse(df2$trust <= 10, 10,
                     ifelse(df2$trust > 10 & df2$trust <= 20, 20, 
                            ifelse(df2$trust > 20 & df2$trust <= 30, 30,
                                   ifelse(df2$trust > 30 & df2$trust <= 40, 40, 50))))

x <- df2 %>% dplyr::select(name, trust, trust2)

places <- readOGR(dsn = "ne_10m_populated_places_simple", layer = "ne_10m_populated_places_simple")
places <- places@data

df3 <- df2 %>% dplyr::select("name", "City")
df3$ID <- paste(df3$name, df3$City, sep = ", ")
places2 <- places %>% dplyr::select("name", "sov0name", "pop_max")
places2$ID2 <- paste(places2$sov0name, places2$name, sep = ", ")
df4 <- merge(df3, places2, by.x = c("name","City"), by.y = c("sov0name","name"), all.x = T, suffixes = c(".x",".y"), no.dups = F)
df5 <- df4 %>% dplyr::select(ID, pop_max)
#write.csv(df5, "cities_pop.csv", row.names = F)

colnames(df5) <- c("country_name", "City", "city_pop")
df5$City <- trimws(df5$City)

df6 <- merge(df, df5, by = c("country_name"), all.x = T)

df7 <- df6 %>% dplyr::select(City.x, City.y)

df6 <- df6 %>% mutate(City_con_10kpop = contributions_city / city_pop * 100000)

df6$City_con_10kpop <- ifelse(is.na(df6$City_con_10kpop),0,df6$City_con_10kpop)

df6$City_con_10kpop2 <- ifelse(df6$City_con_10kpop <= 1, 1,
                                 ifelse(df6$City_con_10kpop > 1 & df6$City_con_10kpop <= 10, 10,
                                        ifelse(df6$City_con_10kpop > 10 & df6$City_con_10kpop <= 50, 50,
                                               ifelse(df6$City_con_10kpop > 50 & df6$City_con_10kpop <= 100, 100, 500))))


summary(df6$City_con_10kpop)

# Plot
ggplot() +
  geom_sf(data = df2, aes(fill = factor(trust2)), 
          lwd = 0.1) +
  coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +
  geom_nodes(data = df6, 
             aes(x=lon, y = lat, size = City_con_10kpop,
                 text = sprintf("%s <br>SO Contributions: %s <br> SO Contrib. per 10k pop: %s", City.x, contributions_city, round(City_con_10kpop)),
                            col = factor(City_con_10kpop2)), shape = 21, stroke = 1, fill = "grey", alpha = 0.5) +
  scale_color_manual(values = vir2) +
  #scale_fill_manual(values = c(brewer.pal(9,"RdBu")[1], 
   #                             brewer.pal(9,"RdBu")[9])) +
  scale_fill_manual(values = vir) +
  #scale_color_discrete(breaks = c(1,10,100,1000,10000)) +
  #scale_colour_continuous(breaks = c(1,10,100,1000,10000)) +
  scale_size_continuous(breaks = c(1,10,50,100,500), range = c(0.05,4.5)) +
  labs(col = "Stack Overflow Contributions per 10,000 pop.", size = "Stack Overflow Contributions per 10,000 pop.", fill = "Trust") +
  theme_blank() + theme(legend.position = "bottom", legend.background = element_blank(),
                        legend.direction = "vertical",
                        legend.text = element_text(size = 14),
                        #legend.position = "none",
                        panel.grid.major = element_line(colour = "white")) +
  guides(fill = guide_legend(nrow = 1),
         col = guide_legend(nrow = 1))

#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Figure 1B Power Law Plot
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

df <- df %>% mutate(relCountry = contributions_country / sum(contributions_country), 
                    rankCountry = rank(-relCountry), rankRelCountry = rankCountry / max(rankCountry),
                    relCity = contributions_city / sum(contributions_city), 
                    rankCity = rank(-relCity), rankRelCity = rankCity / max(rankCity))

fig1BdataA <- df %>% dplyr::select(name = country_name, contributions = contributions_country,
                                 rank = rankRelCountry)
fig1BdataA$level <- "Country"

fig1BdataB <- df %>% dplyr::select(name = City, contributions = contributions_city,
                                  rank = rankRelCity)

fig1BdataB$level <- "City"

fig1Bdata <- rbind(fig1BdataA, fig1BdataB)

fig1BdataB <- fig1BdataB %>% arrange(contributions)

fig1BdataB$cumsum <- cumsum(fig1BdataB$contributions)

fig1BdataB$summe <- fig1BdataB$cumsum/sum(fig1BdataB$contributions)

ggplot(fig1Bdata, aes(x = contributions, y = rank, col = level)) + 
  geom_point(alpha = 0.8, size = 2.5)+ 
  geom_line(lwd = 1.2, alpha = 0.3) + 
  scale_color_manual(values = c(brewer.pal(5,"Oranges")[4],brewer.pal(5,"Blues")[4]))+ coord_fixed(2.25) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = c(1,0.1,0.01), labels = c("100","10","1")) +
  #geom_smooth(method = "lm") +
  #geom_abline(slope = -0.8, intercept = 2) +
  annotation_logticks(sides ="bltr", colour = "grey", size = 1) +
  labs(x = "Stack Overflow Contributions", y = "CCDF (% of population)", col = "", lty = "") +
  theme_bw() + theme(panel.grid = element_blank(), text = element_text(size = 16),
                     axis.text = element_text(size = 14),
                     legend.position = c(0.15,0.165), legend.text = element_text(size = 16),
                     legend.title = element_blank(),
                     legend.background = element_rect(colour = "grey", size = 0.5))

#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Figure 1C Clicks vs. Contributions
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

fig1CdataA <- df %>% dplyr::select(name = country_name, contributions = contributions_country,
                                  clicks = clicks_country)
fig1CdataA$level <- "Country"

fig1CdataA2 <- fig1CdataA %>% filter(contributions > 0)
summary(lm(log(clicks) ~ log(contributions), data = fig1CdataA2))

fig1CdataB <- df %>% dplyr::select(name = City, contributions = contributions_city,
                                  clicks = clicks_city)

fig1CdataB2 <- fig1CdataB %>% filter(contributions > 0)
summary(lm(log(clicks) ~ log(contributions), data = fig1CdataB2))

city_reg = expression(paste("R"^"2"," = 0.72"))
city_slope = expression(paste(beta, " = 0.81"))
country_reg = expression(paste("R"^"2"," = 0.59"))
country_slope = expression(paste(beta," = 0.58"))

reg_label = c(country_reg,city_reg)
slope_label = c(country_slope, city_slope)

fig1CdataB$level <- "City"

fig1Cdata <- rbind(fig1CdataA, fig1CdataB)

fig1Cdata %>% filter(contributions > 0) %>%
ggplot(aes(x = contributions, y = clicks, col = level, fill = level)) +
  scale_color_manual(values = c(brewer.pal(5,"Oranges")[4],brewer.pal(5,"Blues")[4]))+
  scale_fill_manual(values = c(brewer.pal(5,"Oranges")[4],brewer.pal(5,"Blues")[4]))+
  annotation_logticks(sides ="bltr", colour = "grey", size = 0.5) +
  #annotate("text", x = 6, y = 20000000, label = reg_label) +
  #annotate("text", x = 7.5, y = 40000000, label = slope_label) +
  geom_point(size = 2, alpha = 0.8, stroke = 0.5) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
  geom_smooth(method="lm", show.legend = F, se = F) +
  facet_wrap(~level, nrow = 1) +
  labs(x = "Stack Overflow Contributions", y = "Stack Overflow Clicks") +
  theme_bw() +
  theme(panel.grid = element_blank(), text = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.position = c(0.85,0.165), legend.text = element_text(size = 16),
        legend.title = element_blank(),
        legend.background = element_rect(colour = "grey", size = 0.5))

#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Figure 2A(1) Beeswarm plot countries
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

# GDP
first <- quantile(df6$gdp_pc, na.rm = T)[2]
second <- quantile(df6$gdp_pc, na.rm = T)[3]
third <- quantile(df6$gdp_pc, na.rm = T)[4]

df6$quartileGDP <- ifelse(df6$gdp_pc < first, "25%",
                      ifelse(df6$gdp_pc > first & df6$gdp_pc < second, "50%",
                             ifelse(df6$gdp_pc > second & df6$gdp_pc < third, "75%", "100%")))

df6$quartileGDP <- factor(df6$quartileGDP, levels = c("25%", "50%", "75%", "100%"))

# Affinity
first <- quantile(df6$affinity_country, na.rm = T)[2]
second <- quantile(df6$affinity_country, na.rm = T)[3]
third <- quantile(df6$affinity_country, na.rm = T)[4]

df6$quartileAff <- ifelse(df6$affinity_country < first, "25%",
                         ifelse(df6$affinity_country > first & df6$affinity_country < second, "50%",
                                ifelse(df6$affinity_country > second & df6$affinity_country < third, "75%", "100%")))

df6$quartileAff <- factor(df6$quartileAff, levels = c("25%", "50%", "75%", "100%"))

# Trust
first <- quantile(df6$trust, na.rm = T)[2]
second <- quantile(df6$trust, na.rm = T)[3]
third <- quantile(df6$trust, na.rm = T)[4]

df6$quartileTrust <- ifelse(df6$trust < first, "25%",
                         ifelse(df6$trust > first & df6$trust < second, "50%",
                                ifelse(df6$trust > second & df6$trust < third, "75%", "100%")))

df6$quartileTrust <- factor(df6$quartileTrust, levels = c("25%", "50%", "75%", "100%"))

fig2Adata <- df6 %>% dplyr::select(pop, name = country_name, contributions = contributions_country,
                                  quartileTrust, quartileAff, quartileGDP)

x <- gather(fig2Adata, key, value,-name, -contributions, -pop)

x$value <- factor(x$value, levels = c("25%", "50%", "75%", "100%"))
x$key <- ifelse(x$key == "quartileAff", "Affinity",
                 ifelse(x$key == "quartileGDP", "GDP", "Trust"))

countryBees <- x %>% filter(!is.na(value), contributions > 0) %>%
ggplot(aes(y = contributions/pop * 10000, x = value, 
               fill = key)) + 
  facet_wrap(~key) +
  geom_beeswarm(shape = 21, alpha = 0.5, size = 3, col = "black", stroke = 0.3) +
  geom_boxplot(alpha =0, lwd = 0.2, width = 0.4) +
  scale_fill_manual(values = c(brewer.pal(5,"Greens")[4],
                                 brewer.pal(5,"Oranges")[4],
                                 brewer.pal(5,"Blues")[4])) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(side = "lr", colour = "grey") +
  labs(x= "", y = "", size = "Share of Urban Pop. (%)") +
  theme_bw() + 
  theme(text = element_text(size = 14),legend.direction = "horizontal",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        panel.grid = element_blank(),
        legend.position =  "bottom", legend.background = element_blank()) + 
  guides(fill = F, alpha = F, size = guide_legend(override.aes = list(alpha = 1)))

#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Figure 2A(2) Beeswarm plot OECD cities
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

load(paste(dirname(getwd()), "/Data/coding_alone.RData", sep = ""))
df2 <- coding_alone

# GDP
first <- quantile(df2$city_gdp_pc, na.rm = T)[2]
second <- quantile(df2$city_gdp_pc, na.rm = T)[3]
third <- quantile(df2$city_gdp_pc, na.rm = T)[4]

df2$quartileGDP <- ifelse(df2$city_gdp_pc < first, "25%",
                         ifelse(df2$city_gdp_pc > first & df2$city_gdp_pc < second, "50%",
                                ifelse(df2$city_gdp_pc > second & df2$city_gdp_pc < third, "75%", "100%")))

df2$quartileGDP <- factor(df2$quartileGDP, levels = c("25%", "50%", "75%", "100%"))

# Affinity
first <- quantile(df2$affinity, na.rm = T)[2]
second <- quantile(df2$affinity, na.rm = T)[3]
third <- quantile(df2$affinity, na.rm = T)[4]

df2$quartileAff <- ifelse(df2$affinity < first, "25%",
                         ifelse(df2$affinity > first & df2$affinity < second, "50%",
                                ifelse(df2$affinity > second & df2$affinity < third, "75%", "100%")))

df2$quartileAff <- factor(df2$quartileAff, levels = c("25%", "50%", "75%", "100%"))

# Trust
first <- quantile(df2$region_trust, na.rm = T)[2]
second <- quantile(df2$region_trust, na.rm = T)[3]
third <- quantile(df2$region_trust, na.rm = T)[4]

df2$quartileTrust <- ifelse(df2$region_trust < first, "25%",
                           ifelse(df2$region_trust > first & df2$region_trust < second, "50%",
                                  ifelse(df2$region_trust > second & df2$region_trust < third, "75%", "100%")))

df2$quartileTrust <- factor(df2$quartileTrust, levels = c("25%", "50%", "75%", "100%"))

fig2Bdata <- df2 %>% dplyr::select(city_pop,name = country_name, activity = activity,
                                  quartileTrust, quartileAff, quartileGDP)

x <- gather(fig2Bdata, key, value,-name, -activity,-city_pop)

x$value <- factor(x$value, levels = c("25%", "50%", "75%", "100%"))
x$key <- ifelse(x$key == "quartileAff", "Affinity",
                ifelse(x$key == "quartileGDP", "GDP", "Trust"))

CityBees <- x %>% filter(!is.na(value), activity > 0) %>%
  ggplot(aes(y = activity / city_pop * 10000, x = value, 
             fill = key)) + 
  facet_wrap(~key) +
  geom_beeswarm(shape = 21, alpha = 0.5, size = 3, col = "black", stroke = 0.3) +
  geom_boxplot(alpha =0, lwd = 0.2, width = 0.4) +
  scale_fill_manual(values = c(brewer.pal(5,"Greens")[4],
                               brewer.pal(5,"Oranges")[4],
                               brewer.pal(5,"Blues")[4])) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(side = "lr", colour = "grey") +
  labs(x= "Quartiles", y = "                                                                 Stack Overflow Contributions per 10,000 pop.", size = "Share of Urban Pop. (%)") +
  theme_bw() + 
  theme(text = element_text(size = 14),legend.direction = "horizontal",
        legend.title = element_text(size = 10),
        panel.grid = element_blank(),
        legend.text = element_text(size = 10),
        legend.position =  "bottom", legend.background = element_blank()) + 
  guides(fill = F, alpha = F, size = guide_legend(override.aes = list(alpha = 1)))

ggarrange(countryBees, CityBees, nrow = 2)


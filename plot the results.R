
########################################################Distribute the total benefits
###########The GDP in 1980 was based on data from World Bank and other websites, and the GDP in 2100 was obtained from the literature
library(terra)
r <- rast("C:/Users/23073/Desktop/GDP_025d_s/GDP_LitPopBase_025d_s/GDP2100_ssp2.tif")
res(r)      
crs(r)      
r_sum <- aggregate(r, fact = c(4, 5), fun = sum, na.rm = TRUE)
writeRaster(r_sum, "C:/Users/23073/Desktop/gdp_125_1.tif", overwrite = TRUE)

library(terra)
library(ggplot2)
library(rnaturalearth)      
library(rnaturalearthdata)
library(sf)
r <- rast("C:/Users/23073/Desktop/GDP_025d_s/GDP_LitPopBase_025d_s/gdp_125_1.tif") 
print(crs(r))
print(res(r))
df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
names(df)[3] <- "gdp"
world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf")
p_linear <- ggplot() +
  geom_raster(data = df, aes(x = x, y = y, fill = gdp)) +
  geom_sf(data = world, fill = NA, color = "grey20", linewidth = 0.15) +
  coord_sf(expand = FALSE) +
  scale_fill_viridis_c(option = "C", name = "GDP") +
  labs(title = "GDP Distribution (1° × 1.25°)") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank()
  )
q <- quantile(df$gdp, probs = c(0.01, 0.99), na.rm = TRUE)
df$gdp_clipped <- pmax(pmin(df$gdp, q[2]), q[1])
p_quantile <- ggplot() +
  geom_raster(data = df, aes(x = x, y = y, fill = gdp_clipped)) +
  geom_sf(data = world, fill = NA, color = "grey20", linewidth = 0.15) +
  coord_sf(expand = FALSE) +
  scale_fill_viridis_c(option = "C",
                       name = "GDP (clipped 1–99%)",
                       guide = guide_colorbar(barheight = unit(80, "pt"))) +
  labs(title = "GDP Distribution (1° × 1.25°), 1–99% Clipped") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank()
  )
print(p_linear)
# ggsave("gdp_map_linear.png", p_linear, width = 10, height = 5, dpi = 300)
print(p_quantile)
# ggsave("gdp_map_quantile.png", p_quantile, width = 10, height = 5, dpi = 300)

library(data.table)
library(dplyr)
country_grid <- fread("C:/Users/23073/Desktop/country_lon_lat.csv")
sum(df$gdp)
colnames(df)[which(names(df) == 'x')] <- "lon"
colnames(df)[which(names(df) == 'y')] <- "lat"
total <- merge(df,country_grid ,by=c("lon","lat"))
sum(total$gdp)
p_quantile <- ggplot() +
  geom_raster(data = total, aes(x = lon, y = lat, fill = gdp_clipped)) +
  geom_sf(data = world, fill = NA, color = "grey20", linewidth = 0.15) +
  coord_sf(expand = FALSE) +
  scale_fill_viridis_c(option = "C",
                       name = "GDP (clipped 1–99%)",
                       guide = guide_colorbar(barheight = unit(80, "pt"))) +
  labs(title = "GDP Distribution (1° × 1.25°), 1–99% Clipped") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank()
  )
print(p_quantile)
type <- fread("C:/Users/23073/Desktop/Montreal_parties.csv")
unique(type$country)
type <- type %>%
  mutate(country = ifelse(country == "C么te d'Ivoire", "Côte d'Ivoire", country))
total <- merge(total,type,by=c("country"))
gdp_ <- total %>%
  group_by(type) %>%
  summarise(gdp_type = sum(gdp, na.rm = TRUE))

gdp_1980 <- fread("C:/Users/23073/Desktop/GDP_1980.csv")
gdp_1980 <- na.omit(gdp_1980)
gdp_1980 <- gdp_1980 %>%
  group_by(type) %>%
  summarise(gdp_type_1980 = sum(GDP_1980, na.rm = TRUE))
total <- merge(gdp_,gdp_1980,by=c("type"))
fwrite(total,file="C:/Users/23073/Desktop/GDP_1980_2100.csv",row.names = FALSE)



library(data.table)
library(dplyr)
total <- fread("C:/Users/23073/Desktop/2025/oe/cost_total_oe.csv")
total <- data.frame(total)
total
total_91 <- total[total$year >= 1991,]
unique(total_91$year)
total_ <- total_91 %>%
  group_by(country) %>%
  summarise(total_cost = sum(cost_total, na.rm = TRUE))

gdp <- fread("C:/Users/23073/Desktop/GDP_1980_2100.csv")
gdp$gdp_pro_1980 <- gdp$gdp_type_1980/sum(gdp$gdp_type_1980)
gdp$gdp_pro_2100 <- gdp$gdp_type/sum(gdp$gdp_type)

total_

library(dplyr)
gdp_A5 <- gdp %>% filter(type == "A5")
total_ <- total_ %>%
  mutate(
    allocated_cost_lower = total_cost * gdp_A5$gdp_pro_1980,
    allocated_cost_upper = total_cost * gdp_A5$gdp_pro_2100
  )
total_ <- total_ %>%
  mutate(
    allocated_cost_mean = (allocated_cost_lower + allocated_cost_upper) / 2
  )


total_$allocated_cost_mean_billion <- total_$allocated_cost_mean/10^9
total_$allocated_cost_upper_billion <- total_$allocated_cost_upper/10^9
total_$allocated_cost_lower_billion <- total_$allocated_cost_lower/10^9
fwrite(total_,file="C:/Users/23073/Desktop/cost_benefit_ratio_new_91.csv",row.names=FALSE)
############################################################################################################################


###############################################The appendix figure about population
library(data.table)
library(dplyr)
pop <- fread("C:/Users/23073/Desktop/pop_1980_1999_2000pro.csv")
pop1 <- fread("C:/Users/23073/Desktop/pop_2000_2021_grid.csv")
pop2 <- fread("C:/Users/23073/Desktop/pop_2022_2100_grid.csv")
pop <- pop[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop1 <- pop1[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop2 <- pop2[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop <- rbind(pop,pop1)
pop <- rbind(pop,pop2)
pop$lat <- round(pop$lat * 2) / 2
# Round the fourth decimal place
pop$lon <- round(pop$lon, 4)
setDT(pop)
pop <- pop[pop$year>=1991,]
pop_long <- melt(pop, id.vars = c("country", "year", "AgeGrp", "lon", "lat"),
                 measure.vars = list(c("popfemale_grid", "popmale_grid")),
                 value.name = "population",
                 variable.name = "sex_name")
pop_long[, sex_name := ifelse(sex_name == "popfemale_grid", "Female", "Male")]
pop_long
colnames(pop_long)[which(names(pop_long) == "AgeGrp")] <- "age_name"

pop_per_country_per_age_per_sex <- pop_long %>%
  group_by(country,age_name,sex_name,year) %>%
  summarise(pop_country_age_sex = sum(population, na.rm = TRUE))
pop_per_country_per_age_per_sex <- data.frame(pop_per_country_per_age_per_sex)

pop_per_country_per_age_per_sex <- pop_per_country_per_age_per_sex %>%
  filter(age_name != "")
pop_per_country_per_age_per_sex <- pop_per_country_per_age_per_sex %>%
  group_by(country) %>%
  mutate(pop_country = sum(pop_country_age_sex, na.rm = TRUE)) %>%
  ungroup()
unique_pop <- pop_per_country_per_age_per_sex %>%
  distinct(country, pop_country)
fwrite(unique_pop,file="C:/Users/23073/Desktop/pop_per_country_1991_2100_sum.csv",row.names=FALSE)


library(data.table)
library(dplyr)
unique_pop <- fread("C:/Users/23073/Desktoppop_per_country_1991_2100_sum.csv")
total <- fread("C:/Users/23073/Desktop/cost_benefit_ratio.csv")
total <- merge(total,unique_pop,by=c("country"))

my_colors <- read.csv("C:/Users/23073/Desktop/my_colors.csv")$color
color_map <- data.frame(
  country = total$country,
  color = my_colors
)
library(ggplot2)
library(dplyr)
total$pop_country <- as.numeric(total$pop_country)
color_map <- data.frame(
  country = unique(total$country),
  color = my_colors[1:length(unique(total$country))]
)
total_plot <- total %>%
  left_join(color_map, by = "country")
total_plot$pop_country_billion <- total_plot$pop_country/10^9
p <- ggplot(total_plot, aes(x = reorder(country, pop_country_billion), y = pop_country_billion, fill = country)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = setNames(total_plot$color, total_plot$country)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 18),
    axis.text.y = element_text(size = 24),
    axis.title = element_text(size = 24),
    legend.position = "none"
  ) +
  labs(
    x = "Country",
    y = "Population (billion)"
  )
filename <- paste("C:/Users/23073/Desktop/fig_0827/fig s2_new.tif")
ggsave(filename, p, width = 16, height = 14, units = "in", dpi = 400)



###############################################desease burden
###########################################################gbd_90_21
library(data.table)
library(dplyr)
library(tidyr)
#gbd <- fread("C:/Users/23073/Desktop/gbd_1980_2021.csv")
gbd <- fread("C:/Users/23073/Desktop/gbd_cataract_1990_2021.csv")

pop <- fread("C:/Users/23073/Desktop/pop_1980_1999_2000pro.csv")
pop1 <- fread("C:/Users/23073/Desktop/pop_2000_2021_grid.csv")
pop <- pop[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop1 <- pop1[,c("country","year","AgeGrp","lon","lat","popfemale_grid","popmale_grid")]
pop <- rbind(pop,pop1)
pop$lat <- round(pop$lat * 2) / 2
# Round the fourth decimal place
pop$lon <- round(pop$lon, 4)
setDT(pop)
pop_long <- melt(pop, id.vars = c("country", "year", "AgeGrp", "lon", "lat"),
                 measure.vars = list(c("popfemale_grid", "popmale_grid")),
                 value.name = "population",
                 variable.name = "sex_name")
pop_long[, sex_name := ifelse(sex_name == "popfemale_grid", "Female", "Male")]
pop_long
colnames(pop_long)[which(names(pop_long) == "AgeGrp")] <- "age_name"

pop_per_country_per_age_per_sex <- pop_long %>%
  group_by(country,age_name,sex_name,year) %>%
  summarise(pop_country_age_sex = sum(population, na.rm = TRUE))
pop_per_country_per_age_per_sex <- data.frame(pop_per_country_per_age_per_sex)
pop_long <- merge(pop_long,pop_per_country_per_age_per_sex,by=c("country","year","age_name","sex_name"))
pop_long$pop_pro <- pop_long$population/pop_long$pop_country_age_sex

unique(gbd$cause_name)
#gbd_ <- gbd[gbd$measure_name=="Incidence"&gbd$age_name!="All ages",]
#gbd_ <- gbd[gbd$measure_name=="Deaths"&gbd$age_name!="All ages",]
gbd_ <- gbd[gbd$age_name!="All ages",]
unique(gbd_$cause_name)
colnames(gbd_)[which(names(gbd_) == "location_name")] <- "country"
total <- gbd_
################################################cataract
aggregated_data_year <- total %>%
  group_by(country) %>%
  summarise(
    number_lower_range = sqrt(sum(lower, na.rm = TRUE)),
    number_upper_range = sqrt(sum(upper, na.rm = TRUE)),
    number_total = sum(val, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    number_lower = number_total - number_lower_range,
    number_upper = number_total + number_upper_range
  )
print(aggregated_data_year)

################################################skin cancer
val <- gbd_ %>%
  dplyr::select(country, sex_name, age_name, year, cause_name, val) %>%  
  mutate(cause_name = case_when(
    cause_name == "Non-melanoma skin cancer (squamous-cell carcinoma)" ~ "val_scc",
    cause_name == "Malignant skin melanoma" ~ "val_mm",
    cause_name == "Non-melanoma skin cancer (basal-cell carcinoma)" ~ "val_bcc"
  )) %>%
  pivot_wider(names_from = cause_name, values_from = val)
print(val)

upper <- gbd_ %>%
  dplyr::select(country, sex_name, age_name, year, cause_name, upper) %>%  
  mutate(cause_name = case_when(
    cause_name == "Non-melanoma skin cancer (squamous-cell carcinoma)" ~ "upper_scc",
    cause_name == "Malignant skin melanoma" ~ "upper_mm",
    cause_name == "Non-melanoma skin cancer (basal-cell carcinoma)" ~ "upper_bcc"
  )) %>%
  pivot_wider(names_from = cause_name, values_from = upper)
print(upper)

lower <- gbd_ %>%
  dplyr::select(country, sex_name, age_name, year, cause_name, lower) %>%  
  mutate(cause_name = case_when(
    cause_name == "Non-melanoma skin cancer (squamous-cell carcinoma)" ~ "lower_scc",
    cause_name == "Malignant skin melanoma" ~ "lower_mm",
    cause_name == "Non-melanoma skin cancer (basal-cell carcinoma)" ~ "lower_bcc"
  )) %>%
  pivot_wider(names_from = cause_name, values_from = lower)
print(lower)

total <- merge(val,upper,by=c("country","sex_name","age_name","year"))
total <- merge(total,lower,by=c("country","sex_name","age_name","year"))

total <- total %>%
  mutate(across(everything(), ~ replace_na(., 0)))

aggregated_data_year <- total %>%
  group_by(country) %>%
  mutate(
    number_upper = ((upper_mm - val_mm)^2 + (upper_bcc - val_bcc)^2+ (upper_scc - val_scc)^2),
    number_lower = ((val_mm - lower_mm)^2 + (val_bcc - lower_bcc)^2+ (val_scc - lower_scc)^2),
    # number_upper = ((upper_mm - val_mm)^2 + (upper_scc - val_scc)^2),
    # number_lower = ((val_mm - lower_mm)^2 + (val_scc - lower_scc)^2),
  ) %>%
  summarise(
    number_lower_range = sqrt(sum(number_lower, na.rm = TRUE)),
    number_upper_range = sqrt(sum(number_upper, na.rm = TRUE)),
    number_total = sum(val_mm,val_bcc,val_scc, na.rm = TRUE)
    #number_total = sum(val_mm,val_scc, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    number_lower = number_total - number_lower_range,
    number_upper = number_total + number_upper_range
  )
print(aggregated_data_year)


unique(gbd_$year)
unique(pop_long$year)
pop_long <- pop_long[pop_long$year >= 1990 &pop_long$year <=2021,]
pop_per_country <- pop_long %>%
  group_by(country) %>%
  summarise(pop_country = sum(population, na.rm = TRUE))

total <- merge(aggregated_data_year,pop_per_country,by=c("country"))
total$pop_country_permillion <- total$pop_country/10^6
total$total_incidence_country <- total$number_total/total$pop_country_permillion
total$total_upper_country <- total$number_upper/total$pop_country_permillion
total$total_lower_country <- total$number_lower/total$pop_country_permillion
total <- total[,c("country","total_incidence_country","total_upper_country","total_lower_country")]

df<- fread("C:/Users/23073/Desktop/cost_benefit_ratio.csv")

filtered_total <- total %>%
  filter(country %in% df$country)

library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(viridis)
library(Polychrome)
world_new <- st_read("C:/Users/23073/Desktop/natural_earth_vector/10m_cultural/ne_10m_admin_0_countries_chn.shp")
world_new <- world_new[,21]
hongkong <- world_new[world_new$NAME_LONG == "Hong Kong", ]
macao <- world_new[world_new$NAME_LONG == "Macao", ]
china <- world_new[world_new$NAME_LONG == "China", ]
china_merged <- rbind(china, hongkong, macao)
china_merged$NAME_LONG <- "China"
china_merged <- china_merged[!(china_merged$NAME_LONG %in% c("Hong Kong", "Macao")), ]
world_new <- rbind(world_new[!(world_new$NAME_LONG %in% c("China", "Hong Kong", "Macao")), ], china_merged)

world_merged_1 <- merge(world_new, filtered_total, by.x = "NAME_LONG", by.y = "country", all.x = TRUE)
boundary <- st_boundary(world_merged_1)

min_value <- min(c(min(world_merged_1$total_incidence_country, na.rm = TRUE)))
max_value <- max(c(max(world_merged_1$total_incidence_country, na.rm = TRUE)))
quantiles <- quantile(world_merged_1$total_incidence_country, probs = seq(0, 1, 0.01), na.rm = TRUE)

world_merged_1$centroid <- st_point_on_surface(world_merged_1$geometry)
world_merged_1$lon <- st_coordinates(world_merged_1$centroid)[, 1]
world_merged_1$lat <- st_coordinates(world_merged_1$centroid)[, 2]
centroid_coordinates <- st_coordinates(world_merged_1$centroid)
centroid_data <- data.frame(
  NAME_LONG = world_merged_1$NAME_LONG,  
  dif_total_country = world_merged_1$total_incidence_country, 
  lon = centroid_coordinates[, "X"],  
  lat = centroid_coordinates[, "Y"]   
)

# cataract
world_merged_1 <- world_merged_1 %>%
  mutate(dif_total_category = cut(
    total_incidence_country,
    breaks = c(-500, 200,250, 300,350, 400, 500, 600, 1000,  max_value),
    labels = c("<200", "200~250", "250~300", "300~350", "350~400", "400~500", "500~600","600~1,000", ">1,000"),
    include.lowest = TRUE
  ))

# #####incidence
# world_merged_1 <- world_merged_1 %>%
#   mutate(dif_total_category = cut(
#     total_incidence_country,
#     breaks = c(-10, 100, 200, 300, 400, 500, 700,1000,2000, max_value),
#     labels = c("<100", "100~200", "200~300", "300~400", "400~500", "500~700", "700~1,000","1,000~2,000", ">2,000"),
#     include.lowest = TRUE
#   ))

# # mortality
# world_merged_1 <- world_merged_1 %>%
#   mutate(dif_total_category = cut(
#     total_incidence_country,
#     breaks = c(-5, 5, 15, 30, 40, 50, 60, 70, 100, max_value),
#     labels = c("<5", "5~15", "15~30", "30~40", "40~50", "50~60", "60~70","70~100", ">100"),
#     include.lowest = TRUE
#   ))

# # death
# custom_colors <- c(
#   "<5" = "#488f31",
#   "5~15" = "#86a44f",
#   "15~30" = "#b8ba76",
#   "30~40" = "#e0d2a3",
#   "40~50" = "#ffeed5",
#   "50~60" = "#f4c8a2",
#   "60~70" = "#eb9f7a",
#   "70~100" = "#e1725e",
#   ">100" = "#de425b"
# )
# ###########incidence
# custom_colors <- c(
#   "<100" = "#488f31",
#   "100~200" = "#86a44f",
#   "200~300" = "#b8ba76",
#   "300~400" = "#e0d2a3",
#   "400~500" = "#ffeed5",
#   "500~700" = "#f4c8a2",
#   "700~1,000" = "#eb9f7a",
#   "1,000~2,000" = "#e1725e",
#   ">2,000" = "#de425b"
# )

# cataract
custom_colors <- c(
  "<200" = "#488f31",
  "200~250" = "#86a44f",
  "250~300" = "#b8ba76",
  "300~350" = "#e0d2a3",
  "350~400" = "#ffeed5",
  "400~500" = "#f4c8a2",
  "500~600" = "#eb9f7a",
  "600~1,000" = "#e1725e",
  ">1,000" = "#de425b"
)


filtered_world_merged_1 <- world_merged_1 %>%
  filter(!(near(lon, 114.0559975, tol = 1e-3) & near(lat, 22.41120026, tol = 1e-3)) &  # 去除第一个点
           !(near(lon, 113.5594336, tol = 1e-3) & near(lat, 22.13617585, tol = 1e-3)))   # 去除第二个点
p1 <- ggplot(data = filtered_world_merged_1) +
  geom_sf(aes(fill = dif_total_category), color = NA) +
  geom_sf(data = boundary, color = "white", fill = NA, linewidth = 0.5) +
  scale_fill_manual(
    values = custom_colors,
    name = "DALYs Per Million",
    #name = "Cases Per Million",
    na.value = "lightgray"  
  ) +
  labs(fill = "DALYs Per Million") +
  #labs(fill = "Cases Per Million") +
  #  ggtitle("(a) 2030") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text( size = 18),
    legend.title = element_text(size = 22),  
    legend.text = element_text(size = 22),  
  ) +
  guides(
    #fill = guide_colorbar(
    fill = guide_legend(
      barheight = unit(0.5, "cm"),
      barwidth = unit(0.5, "cm")
    )
  )

filename <- paste("C:/Users/23073/Desktop/fig_0827/daly_90_21.png")
ggsave(filename, p1, width = 16, height = 8, units = "in", dpi = 400)



############################################################time series
library(data.table)
library(dplyr)
total <- fread("C:/Users/23073/Desktop/2025/oe/cost_total_oe.csv")
total <- data.frame(total)
total
total_ <- total %>%
  group_by(year,country) %>%
  summarise(total_cost = sum(cost_total, na.rm = TRUE))

gdp <- fread("C:/Users/23073/Desktop/GDP_1980_2100.csv")
gdp$gdp_pro_1980 <- gdp$gdp_type_1980/sum(gdp$gdp_type_1980)
gdp$gdp_pro_2100 <- gdp$gdp_type/sum(gdp$gdp_type)

total_

library(dplyr)
gdp_A5 <- gdp %>% filter(type == "A5")
total_ <- total_ %>%
  mutate(
    allocated_cost_lower = total_cost * gdp_A5$gdp_pro_1980,
    allocated_cost_upper = total_cost * gdp_A5$gdp_pro_2100
  )
total_ <- total_ %>%
  mutate(
    allocated_cost_mean = (allocated_cost_lower + allocated_cost_upper) / 2
  )

library(readxl)
fund <- read_excel("C:/Users/23073/Desktop/非A5国家投资额.xlsx", sheet = 1)
fund <- fund %>%
  filter(country != "Total")
unique(fund$country)

fund <- fund %>%
  mutate(country = ifelse(country == "Czech Republic", "Czechia", country))
fund <- fund %>%
  mutate(country = ifelse(country == "Russian Federation", "Russia", country))
fund <- fund %>%
  mutate(country = ifelse(country == "Slovak Republic", "Slovakia", country))
fund <- fund %>%
  mutate(country = ifelse(country == "United States of America", "United States", country))
setdiff(fund$country, total_$country)

sum(fund$Agreed_Contributions)
total_ <- merge(total_,fund,by=c("country"))
total_$ratio <- total_$allocated_cost_mean/total_$Agreed_Contributions
total_$ratio_upper <- total_$allocated_cost_upper/total_$Agreed_Contributions
total_$ratio_lower <- total_$allocated_cost_lower/total_$Agreed_Contributions
total_$allocated_cost_mean_billion <- total_$allocated_cost_mean/10^9
total_$allocated_cost_upper_billion <- total_$allocated_cost_upper/10^9
total_$allocated_cost_lower_billion <- total_$allocated_cost_lower/10^9
#fwrite(total_,file="C:/Users/23073/Desktop/time_benefit_high.csv",row.names = FALSE)


library(ggplot2)
country_labels <- paste(seq_along(color_map$country), color_map$country)
p <- ggplot(total_, aes(x = factor(year), y = allocated_cost_mean_billion, fill = country)) + 
  geom_rect(aes(xmin = -Inf, xmax = which(levels(factor(year)) == "2025"), ymin = -Inf, ymax = Inf), 
            fill = "gray90", alpha = 0.5) + 
  geom_bar(stat = "identity", position = "stack") + 
  scale_fill_manual(values = setNames(color_map$color, color_map$country), labels = country_labels) + 
  labs(title = "Health-related economic benefits obtained from the MLF investment (billion)", 
       x = "Year", y = "Benefits (billion)", fill = "Country") + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.margin = margin(10, 25, 10, 10),
    plot.title = element_text(size = 32, hjust = 0.5),
    axis.text.x = element_text(size = 26),
    axis.text.y = element_text(size = 26),
    axis.title.x = element_text(size = 26),
    axis.title.y = element_text(size = 26),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    legend.position = "bottom",
    panel.grid.major.x = element_line(color = "gray", size = 1),
    panel.grid.minor.x = element_blank()
  ) + 
  # Customize x-axis labels
  scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 10 == 0, x, "")) + 
  guides(fill = guide_legend(
    title = NULL,  
    nrow = 6
  ))
print(p)


filename <- paste("C:/Users/23073/Desktop/fig_0827/time_benefit_high_new.png")
ggsave(filename, p, width = 19, height = 12, units = "in", dpi = 400)


library(dplyr)
library(ggplot2)
country_labels <- paste(seq_along(color_map$country), color_map$country)
total_percentage <- total_ %>%
  group_by(year) %>%
  mutate(total_benefit = sum(allocated_cost_mean_billion),
         percentage = allocated_cost_mean_billion / total_benefit * 100) %>%
  ungroup()

p2 <- ggplot(total_percentage, aes(x = factor(year), y = percentage, fill = country)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = ifelse(percentage > 3, round(percentage,0), "")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = setNames(color_map$color, color_map$country), labels = country_labels) +
  labs(title = "Percentage of country-level health-related economic benefits obtained from the MLF investment",
       x = "Year", y = "Percentage (%)", fill = "Country") +
  theme_minimal(base_size = 14) +
  theme(
    plot.margin = margin(10, 25, 10, 10),
    plot.title = element_text(size = 30, hjust = 0.5),
    axis.text.x = element_text(size = 26),
    axis.text.y = element_text(size = 26),
    axis.title.x = element_text(size = 26),
    axis.title.y = element_text(size = 26),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    legend.position = "bottom",
    panel.grid.major.x = element_line(color = "gray", size = 1),
    panel.grid.minor.x = element_blank()
  ) +
  scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 10 == 0, x, "")) +
  guides(fill = guide_legend(title = NULL, nrow = 6))

# 打印图表
print(p2)

filename2 <- paste("C:/Users/23073/Desktop/fig_0827/benefit_percentage.png")
ggsave(filename2, p2, width = 19, height = 12, units = "in", dpi = 400)




library(data.table)
library(dplyr)
bene_high <- fread("E:/time_benefit_high.csv")
bene_high <- bene_high[bene_high$year >= 1991,]
bene_high_25 <- bene_high[bene_high$year<=2025,]
sum(bene_high_25$allocated_cost_mean_billion)
sum(bene_high_25$allocated_cost_lower_billion)
sum(bene_high_25$allocated_cost_upper_billion)
bene_high_100 <- bene_high[bene_high$year>=2026,]
sum(bene_high_100$allocated_cost_mean_billion)
sum(bene_high_100$allocated_cost_lower_billion)
sum(bene_high_100$allocated_cost_upper_billion)


bene_high_25 <- bene_high[bene_high$year==1991,]
sum(bene_high_25$allocated_cost_mean_billion)
sum(bene_high_25$allocated_cost_lower_billion)
sum(bene_high_25$allocated_cost_upper_billion)
bene_high_100 <- bene_high[bene_high$year==2100,]
sum(bene_high_100$allocated_cost_mean_billion)
sum(bene_high_100$allocated_cost_lower_billion)
sum(bene_high_100$allocated_cost_upper_billion)




bene_high <- fread("C:/Users/23073/Desktop/time_benefit_low.csv")
bene_high_25 <- bene_high[bene_high$year<=2025,]
sum(bene_high_25$distributed_cost_mean_billion)
bene_high_100 <- bene_high[bene_high$year>=2026,]
sum(bene_high_100$distributed_cost_mean_billion)


bene_high <- fread("E:/time_benefit_high.csv")
bene_high <- bene_high[bene_high$year >= 1991,]
bene_high_25 <- bene_high[bene_high$year==1991,]
sum(bene_high_25$allocated_cost_mean_billion)
sum(bene_high_25$allocated_cost_upper_billion)
sum(bene_high_25$allocated_cost_lower_billion)
bene_high_100 <- bene_high[bene_high$year==2100,]
sum(bene_high_100$allocated_cost_mean_billion)
sum(bene_high_100$allocated_cost_upper_billion)
sum(bene_high_100$allocated_cost_lower_billion)
head(bene_high)

library(dplyr)
bene_high <- bene_high[order(bene_high$country, bene_high$year), ]  
bene_high$growth_rate <- ave(bene_high$allocated_cost_mean_billion, bene_high$country, FUN = function(x) c(NA, diff(x)/head(x, -1)))
avg_growth_rate_per_country <- bene_high %>%
  group_by(country) %>%
  summarise(avg_growth_rate = mean(growth_rate, na.rm = TRUE))
global_avg_growth_rate <- mean(avg_growth_rate_per_country$avg_growth_rate, na.rm = TRUE)
avg_growth_rate_per_country_sorted <- avg_growth_rate_per_country %>%
  arrange(desc(avg_growth_rate))
print(paste("Global average annual growth rate: ", round(global_avg_growth_rate, 4)))
head(avg_growth_rate_per_country_sorted) 



#################################################################A bar chart of investment and returns
df <- data.frame(
  Indicator = c("Investment of MLF", "Benefits obtained from the MLF investment", "Benefits obtained from the MLF investment"),
  Period = c("1991–2025 ", "1991–2025", "1991–2100"),
  Value = c(4.9, 29, 3069),
  Lower = c(NA, 12, 1236),  
  Upper = c(NA, 46, 4902)  
)

df$Period <- factor(df$Period, levels = c("1991–2025 ", "1991–2025", "1991–2100"))

df$Indicator <- c("Investment of MLF", "Health-related economic benefits", "Health-related economic benefits")

custom_colors <- c(
  "Investment of MLF" = "#be8634", 
  "Health-related economic benefits" = "#ff68a5"
)

p <- ggplot(df, aes(x = Period, y = Value, fill = Indicator)) + 
  geom_col(position = position_dodge(width = 0.8), width = 0.7) + 
  scale_fill_manual(values = custom_colors) + 
  scale_y_log10() + 
  labs(
    x = "Period", 
    y = "Value (billion, log scale)", 
    fill = ""
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(size = 28, hjust = 0.5),

    legend.position = c(0.02, 0.98),         
    legend.justification = c("left", "top"),  
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_text(size = 22),
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 22),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +

  geom_errorbar(aes(ymin = Lower, ymax = Upper), 
                position = position_dodge(width = 0.8), 
                width = 0.25) 


print(p)

filename <- "C:/Users/23073/Desktop/fig_0827/total_invest_bene_new.png"
ggsave(filename, p, width = 14, height = 7, units = "in", dpi = 400)




#########################################################pie chart
library(data.table)
total <- fread("E:/cost_benefit_ratio.csv")
sum(total$Agreed_Contributions)
my_colors <- read.csv("E:/my_colors.csv")$color
color_map <- data.frame(
  country = total$country,
  color   = my_colors
)
head(total)
total$Agreed_Contributions_billion <- total$Agreed_Contributions/10^9

library(dplyr)
library(plotly)
library(webshot)
threshold <- 0.02
df <- total %>%
  select(country, Agreed_Contributions_billion) %>%
  left_join(color_map, by = "country")
total_sum <- sum(df$Agreed_Contributions_billion, na.rm = TRUE)
df <- df %>%
  mutate(pct = Agreed_Contributions_billion / total_sum) %>%
  mutate(country = ifelse(pct < threshold, "Others", country)) %>%
  group_by(country) %>%
  summarise(
    Agreed_Contributions_billion = sum(Agreed_Contributions_billion, na.rm = TRUE),
    .groups = "drop"
  )
df <- df %>%
  mutate(is_others = ifelse(country == "Others", 1, 0)) %>%
  arrange(is_others, desc(Agreed_Contributions_billion)) %>%
  select(-is_others)
df <- df %>%
  mutate(
    pct = Agreed_Contributions_billion / sum(Agreed_Contributions_billion),
    val_str = format(round(Agreed_Contributions_billion), big.mark = ","),
    pct_str = paste0(round(100 * pct), "%"),
    label_txt = paste0(country, "<br>", pct_str)   # 去掉 val_str
  )
df <- df %>%
  mutate(color = ifelse(country == "Others", "#BEBEBE",
                        color_map$color[match(country, color_map$country)]))
p_donut <- plot_ly(
  type   = "pie",
  labels = df$country,
  values = df$Agreed_Contributions_billion,
  text   = df$label_txt,
  textposition = "inside",
  marker = list(colors = df$color),
  hole   = 0.45,
  sort   = FALSE, 
  direction = "counterclockwise",   
  rotation  = -81,
  hoverinfo = "text",
  textinfo  = "none"
) %>%
  layout(
    showlegend = FALSE,
    margin = list(l = 0, r = 0, t = 50, b = 0),
    font = list(family = "Arial"),
    annotations = list(
      font = list(size = 20)
    )
  )
p_donut



library(data.table)
total <- fread("E:/cost_benefit_ratio_new_91.csv")
sum(total$Agreed_Contributions)
my_colors <- read.csv("E:/my_colors.csv")$color
color_map <- data.frame(
  country = total$country,
  color   = my_colors
)
head(total)

library(dplyr)
library(plotly)
library(webshot)
threshold <- 0.02
df <- total %>%
  select(country, allocated_cost_mean_billion) %>%
  left_join(color_map, by = "country")
total_sum <- sum(df$allocated_cost_mean_billion, na.rm = TRUE)
df <- df %>%
  mutate(pct = allocated_cost_mean_billion / total_sum) %>%
  mutate(country = ifelse(pct < threshold, "Others", country)) %>%
  group_by(country) %>%
  summarise(
    allocated_cost_mean_billion = sum(allocated_cost_mean_billion, na.rm = TRUE),
    .groups = "drop"
  )
df <- df %>%
  mutate(is_others = ifelse(country == "Others", 1, 0)) %>%
  arrange(is_others, desc(allocated_cost_mean_billion)) %>%
  select(-is_others)
df <- df %>%
  mutate(
    pct = allocated_cost_mean_billion / sum(allocated_cost_mean_billion),
    val_str = format(round(allocated_cost_mean_billion), big.mark = ","),
    pct_str = paste0(round(100 * pct), "%"),
    label_txt = paste0(country, "<br>", pct_str)   # 去掉 val_str
  )
df <- df %>%
  mutate(color = ifelse(country == "Others", "#BEBEBE",
                        color_map$color[match(country, color_map$country)]))
p_donut <- plot_ly(
  type   = "pie",
  labels = df$country,
  values = df$allocated_cost_mean_billion,
  text   = df$label_txt,
  textposition = "inside",
  marker = list(colors = df$color),
  hole   = 0.45,
  sort   = FALSE,  
  direction = "counterclockwise",   
  rotation  = -157,
  hoverinfo = "text",
  textinfo  = "none"
) %>%
  layout(
    showlegend = FALSE,
    margin = list(l = 0, r = 0, t = 50, b = 0),
    font = list(family = "Arial"),
    annotations = list(
      font = list(size = 20) 
    )
  )
p_donut



library(data.table)
total <- fread("E:/cost_benefit_ratio.csv")
sum(total$Agreed_Contributions)
my_colors <- read.csv("E:/my_colors.csv")$color
color_map <- data.frame(
  country = total$country,
  color   = my_colors
)
head(total)

library(dplyr)
library(plotly)
library(webshot)
threshold <- 0.02
df <- total %>%
  select(country, distributed_cost_mean_million) %>%
  left_join(color_map, by = "country")
total_sum <- sum(df$distributed_cost_mean_million, na.rm = TRUE)
df <- df %>%
  mutate(pct = distributed_cost_mean_million / total_sum) %>%
  mutate(country = ifelse(pct < threshold, "Others", country)) %>%
  group_by(country) %>%
  summarise(
    distributed_cost_mean_million = sum(distributed_cost_mean_million, na.rm = TRUE),
    .groups = "drop"
  )
df <- df %>%
  mutate(is_others = ifelse(country == "Others", 1, 0)) %>%
  arrange(is_others, desc(distributed_cost_mean_million)) %>%
  select(-is_others)
df <- df %>%
  mutate(
    pct = distributed_cost_mean_million / sum(distributed_cost_mean_million),
    val_str = format(round(distributed_cost_mean_million), big.mark = ","),
    pct_str = paste0(round(100 * pct), "%"),
    label_txt = paste0(country, "<br>", pct_str)  
  )
df <- df %>%
  mutate(color = ifelse(country == "Others", "#BEBEBE",
                        color_map$color[match(country, color_map$country)]))
p_donut <- plot_ly(
  type   = "pie",
  labels = df$country,
  values = df$distributed_cost_mean_million,
  text   = df$label_txt,
  textposition = "inside",
  marker = list(colors = df$color),
  hole   = 0.45,
  sort   = FALSE,
  hoverinfo = "text",
  textinfo  = "text",
  textfont = list(size = 36)  
) %>%
  layout(
    showlegend = FALSE,
    margin = list(l = 0, r = 0, t = 50, b = 0),
    font = list(family = "Arial"),
    annotations = list(
      font = list(size = 20) 
    )
  )
p_donut



######################################average toc difference
library(data.table)
library(dplyr)
toc_dif <- fread("C:/Users/23073/Desktop/toc_pre_dif.csv")
grid <- fread("C:/Users/23073/Desktop/country_lon_lat.csv")
total <- merge(toc_dif,grid,by=c("lon","lat"))
df<- fread("C:/Users/23073/Desktop/cost_benefit_ratio_new.csv")
df
countries <- unique(df$country)
result <- total[country %in% countries]

library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(viridis)
library(Polychrome)
world_new <- st_read("C:/Users/23073/Desktop/natural_earth_vector/10m_cultural/ne_10m_admin_0_countries_chn.shp")
world_new <- world_new[,21]
hongkong <- world_new[world_new$NAME_LONG == "Hong Kong", ]
macao <- world_new[world_new$NAME_LONG == "Macao", ]
china <- world_new[world_new$NAME_LONG == "China", ]
china_merged <- rbind(china, hongkong, macao)
china_merged$NAME_LONG <- "China"
china_merged <- china_merged[!(china_merged$NAME_LONG %in% c("Hong Kong", "Macao")), ]
world_new <- rbind(world_new[!(world_new$NAME_LONG %in% c("China", "Hong Kong", "Macao")), ], china_merged)

world_cropped    <- subset(world_merged, NAME_LONG != "Antarctica")
boundary_cropped <- subset(boundary,      NAME_LONG != "Antarctica")


p <- ggplot() +
  geom_sf(data = world_cropped, fill = "grey80", color = NA) +
  geom_tile(data = result, aes(x = lon, y = lat, fill = toc_avg_dif)) +
  geom_sf(data = world_cropped, fill = NA, color = "white", size = 0.2) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
  scale_fill_viridis(option = "plasma") +
  labs(
    title = "TCO losses avoided by the MP",
    x = NULL,
    y = NULL,
    fill = "DU"
  ) +
  theme_minimal(base_size = 22) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
print(p)

filename <- paste("C:/Users/23073/Desktop/fig_0827/toc_dif.png")
ggsave(filename, p, width = 12, height = 6, units = "in", dpi = 400)



#################################grid diagram of radiation differences
library(data.table)
library(dplyr)
library(tidyr)
ra <- fread("C:/Users/23073/Desktop/ra_annual.csv")
head(ra)
ra$ra_mj <- ra$sum_y*3600/10^6
unique(ra$year)
ra <- ra[ra$year >= 1991,]
ra_nocontrol <- fread("C:/Users/23073/Desktop/ra_annual_Nocontrol.csv")
head(ra_nocontrol)
ra_nocontrol$ra_mj_nocontrol <- ra_nocontrol$sum_y*3600/10^6
unique(ra_nocontrol$year)
ra_nocontrol <- ra_nocontrol[ra_nocontrol$year >= 1991,]

total <- merge(ra,ra_nocontrol,by=c("lon","lat","year"))
total$ra_mj_dif <- total$ra_mj_nocontrol-total$ra_mj

result <- total %>%
  group_by(lat, lon) %>%
  summarise(mean_ra_mj_dif = mean(ra_mj_dif))
result <- data.frame(result)

grid <- fread("C:/Users/23073/Desktop/country_lon_lat.csv")
result <- merge(result,grid,by=c("lon","lat"))
df<- fread("C:/Users/23073/Desktop/cost_benefit_ratio_new.csv")
df
countries <- unique(df$country)
setDT(result)
result <- result[country %in% countries]

library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(viridis)
library(Polychrome)
world_new <- st_read("C:/Users/23073/Desktop/natural_earth_vector/10m_cultural/ne_10m_admin_0_countries_chn.shp")
world_new <- world_new[,21]
hongkong <- world_new[world_new$NAME_LONG == "Hong Kong", ]
macao <- world_new[world_new$NAME_LONG == "Macao", ]
china <- world_new[world_new$NAME_LONG == "China", ]
china_merged <- rbind(china, hongkong, macao)
china_merged$NAME_LONG <- "China"
china_merged <- china_merged[!(china_merged$NAME_LONG %in% c("Hong Kong", "Macao")), ]
world_new <- rbind(world_new[!(world_new$NAME_LONG %in% c("China", "Hong Kong", "Macao")), ], china_merged)

world_cropped    <- subset(world_merged, NAME_LONG != "Antarctica")
boundary_cropped <- subset(boundary,      NAME_LONG != "Antarctica")

p <- ggplot() +
  geom_sf(data = world_cropped, fill = "grey80", color = NA) +
  geom_tile(data = result, aes(x = lon, y = lat, fill = mean_ra_mj_dif)) +
  geom_sf(data = world_cropped, fill = NA, color = "white", size = 0.2) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
  scale_fill_viridis(option = "cividis", na.value = "grey80") +
  labs(
    title = "UV radiation avoided by the MP",
    x = NULL,
    y = NULL,
    fill = expression("MJ/m"^2)
  ) +
  theme_minimal(base_size = 22) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
print(p)

filename <- paste("C:/Users/23073/Desktop/fig_0827/UV_dif.png")
ggsave(filename, p, width = 12, height = 6, units = "in", dpi = 400)


####The emissions of various substances in the two scenarios
library(readxl)
emi <- read_excel("C:/Users/23073/Desktop/emission_and_EESC.xlsx", sheet = "Sheet1")
emi
emi <- data.frame(emi)
head(emi)
library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyr)
emi_long <- melt(emi, id.vars = c("year", "scenario"), variable.name = "substance", value.name = "value")
#emi_long_a1 <- emi_long[emi_long$scenario=="WMO A1",]
emi_long_a1 <- emi_long[emi_long$scenario=="BAU",]
emi_long_a1$substance <- gsub("\\.", "-", emi_long_a1$substance)

emi_long_a1 <- emi_long_a1 %>%
  mutate(substance_parsed = paste0("'", substance, "'")) %>%   # 默认：纯文本
  mutate(substance_parsed = case_when(
    substance == "CH3Br"   ~ "CH[3]*Br",
    substance == "CCl4"    ~ "CCl[4]",
    substance == "CH3CCl3" ~ "CH[3]*CCl[3]",
    substance == "CH3Cl"   ~ "CH[3]*Cl",
    TRUE ~ substance_parsed
  ))

p <- ggplot(emi_long_a1, aes(x = year, y = value)) +
  geom_line(size = 1.2) +   
  
  facet_wrap(~substance_parsed, scales = "free_y", ncol = 4,
             labeller = label_parsed) +      
  
  theme_minimal() +
  
  labs(
    x = "Year",
    y = "Emissions (million kilograms)"
  ) +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_text(size = 22),
    strip.text = element_text(size = 22) 
  )
p
filename <- paste("C:/Users/23073/Desktop/fig_0827/emission_bau.png")
ggsave(filename, p, width = 16, height = 18, units = "in", dpi = 400)


##########EESC under different scenarios
library(readxl)
eesc <- read_excel("C:/Users/23073/Desktop/emission_and_EESC.xlsx", sheet = "Sheet2")
eesc
eesc <- data.frame(eesc)
library(ggplot2)
p <- ggplot(eesc, aes(x = Year, y = EESC, color = scenario)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c("WMO A1" = "#8d8dfe", "BAU" = "#fca09f")) +
  scale_y_log10() +
  scale_x_continuous(
    breaks = seq(1960,2100, by = 20), 
    labels = seq(1960,2100, by = 20)   
  ) +
  labs(
    x = "Year",
    y = "EESC (log scale)",
    color = "Scenario"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  )
p
filename <- paste("C:/Users/23073/Desktop/fig_0827/eesc_compare.png")
ggsave(filename, p, width = 10, height = 7, units = "in", dpi = 400)


#################average toc time series
library(data.table)
wmo <- fread("C:/Users/23073/Desktop/WMO_tuv_inp.csv")
library(dplyr)
wmo_global_annual <- wmo %>%
  filter(year >= 1980, year <= 2100) %>%
  group_by(year) %>% 
  summarise(toc_global_avg_wmo = mean(toc, na.rm = TRUE), .groups = "drop") %>%
  arrange(year)
head(wmo_global_annual)
bau <- fread("C:/Users/23073/Desktop/Nocontrol_tuv_inp.csv")
library(dplyr)
bau_global_annual <- bau %>%
  filter(year >= 1980, year <= 2100) %>%
  group_by(year) %>% 
  summarise(toc_global_avg_bau = mean(toc, na.rm = TRUE), .groups = "drop") %>%
  arrange(year)
head(bau_global_annual)
total <- merge(wmo_global_annual,bau_global_annual,by=c("year"))
fwrite(total,file="C:/Users/23073/Desktop/global_ozone_mean.csv",row.names=FALSE)

library(data.table)
oz <- fread("C:/Users/23073/Desktop/global_ozone_mean.csv")
head(oz)
library(ggplot2)
p <- ggplot(oz, aes(x = year, y = TCO, color = scenario)) +
  geom_line(linewidth = 1.5) + 
  scale_color_manual(values = c("WMO A1" = "#8d8dfe", "BAU" = "#fca09f")) +
  scale_y_log10() + 
  scale_x_continuous(
    breaks = seq(1960,2100, by = 20), 
    labels = seq(1960,2100, by = 20)   
  ) +
  labs(
    x = "Year",
    y = "TCO (DU)",
    color = "Scenario"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 26),
    axis.text.y = element_text(size = 26),
    axis.title.x = element_text(size = 28),
    axis.title.y = element_text(size = 28),
    legend.position = "bottom",
    legend.title = element_text(size = 28),
    legend.text = element_text(size = 28)
  )
p
filename <- paste("C:/Users/23073/Desktop/tco_compare.png")
ggsave(filename, p, width = 10, height = 7, units = "in", dpi = 400)


#################################Radiation time series of the most vulnerable areas (select 25°N-50°N)

library(data.table)
library(dplyr)
library(tidyr)
ra <- fread("C:/Users/23073/Desktop/ra_annual.csv")
head(ra)
ra$ra_mj <- ra$sum_y*3600/10^6
unique(ra$year)
ra <- ra[ra$year >= 1980,]
ra_filtered <- ra %>%
  filter(lat >= 25 & lat <= 50) %>% 
  group_by(year) %>% 
  summarise(radiation_avg_wmo = mean(ra_mj, na.rm = TRUE), .groups = "drop") %>%
  arrange(year)
head(ra_filtered)


ra_nocontrol <- fread("C:/Users/23073/Desktop/ra_annual_Nocontrol.csv")
head(ra_nocontrol)
ra_nocontrol$ra_mj_nocontrol <- ra_nocontrol$sum_y*3600/10^6
unique(ra_nocontrol$year)
ra_nocontrol <- ra_nocontrol[ra_nocontrol$year >= 1980,]
ra_filtered_bau <- ra_nocontrol %>%
  filter(lat >= 25 & lat <= 50) %>%
  group_by(year) %>%
  summarise(radiation_avg_bau = mean(ra_mj_nocontrol, na.rm = TRUE), .groups = "drop") %>%
  arrange(year)
head(ra_filtered_bau)


total <- merge(ra_filtered,ra_filtered_bau,by=c("year"))
fwrite(total,file="C:/Users/23073/Desktop/uv_25n_50n_mean.csv",row.names=FALSE)


library(data.table)
uv <- fread("C:/Users/23073/Desktop/uv_25n_50n_mean.csv")
head(uv)
library(ggplot2)
p <- ggplot(uv, aes(x = year, y = UV_radiation, color = scenario)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = c("WMO A1" = "#8d8dfe", "BAU" = "#fca09f")) +
  scale_y_log10() +
  scale_x_continuous(
    breaks = seq(1960,2100, by = 20),
    labels = seq(1960,2100, by = 20)
  ) +
  labs(
    x = "Year",
    y = expression("UV radiation (MJ/m"^2*")"),
    color = "Scenario"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 26),
    axis.text.y = element_text(size = 26),
    axis.title.x = element_text(size = 28),
    axis.title.y = element_text(size = 28),
    legend.position = "bottom",
    legend.title = element_text(size = 28),
    legend.text = element_text(size = 28)
  )
p
filename <- paste("C:/Users/23073/Desktop/fig_0827/uv_compare.png")
ggsave(filename, p, width = 10, height = 7, units = "in", dpi = 400)



###########################The change in the number
library(data.table)
total <- fread("C:/Users/23073/Desktop/total_cataract_daly_1980_2100.csv")
total <- data.frame(total)
total

gdp <- fread("C:/Users/23073/Desktop/GDP_1980_2100.csv")
gdp$gdp_pro_1980 <- gdp$gdp_type_1980/sum(gdp$gdp_type_1980)
gdp$gdp_pro_2100 <- gdp$gdp_type/sum(gdp$gdp_type)

library(dplyr)
gdp_A5 <- gdp %>% filter(type == "A5")
total_ <- total %>%
  mutate(
    dif_lower = dif * gdp_A5$gdp_pro_1980,
    dif_upper = dif * gdp_A5$gdp_pro_2100,
  )
total_ <- total_ %>%
  mutate(
    dif_mean = (dif_lower + dif_upper) / 2
  )
head(total_)

result <- total_ %>%
  group_by(country, year) %>%
  summarise(
    dif_lower = sum(dif_lower, na.rm = TRUE),
    dif_upper = sum(dif_upper, na.rm = TRUE),
    dif_mean  = sum(dif_mean,  na.rm = TRUE),
    .groups = "drop"
  )

country <- fread("C:/Users/23073/Desktop/cost_benefit_ratio_new_91.csv")
uni_cou <- unique(country$country)
result_select <- result %>%
  filter(country %in% uni_cou)
result_select <- result_select[result_select$year >= 1991,]

total_series_de <- result_select %>%
  group_by(year) %>%
  summarise(
    dif_lower = sum(dif_lower, na.rm = TRUE),
    dif_upper = sum(dif_upper, na.rm = TRUE),
    dif_mean  = sum(dif_mean,  na.rm = TRUE),
    .groups = "drop"
  )
total_series_number <- result_select %>%
  group_by(year) %>%
  summarise(
    dif_lower = sum(dif_lower, na.rm = TRUE),
    dif_upper = sum(dif_upper, na.rm = TRUE),
    dif_mean  = sum(dif_mean,  na.rm = TRUE),
    .groups = "drop"
  )
total_series_daly <- result_select %>%
  group_by(year) %>%
  summarise(
    dif_lower = sum(dif_lower, na.rm = TRUE),
    dif_upper = sum(dif_upper, na.rm = TRUE),
    dif_mean  = sum(dif_mean,  na.rm = TRUE),
    .groups = "drop"
  )
# total_series$dif_lower_million <- total_series$dif_lower/10^6
# total_series$dif_upper_million <- total_series$dif_upper/10^6
# total_series$dif_mean_million <- total_series$dif_mean/10^6
total_series_daly$desease <- "daly"
total_series_number$desease <- "number"
total_series_de$desease <- "de"
library(ggplot2)
library(scales)

p1 <- ggplot(total_series_number, aes(x = year)) +
  geom_ribbon(aes(ymin = dif_lower, ymax = dif_upper),
              fill = "#9ecae1", alpha = 0.35) +
  geom_line(aes(y = dif_mean),
            color = "#2171b5", linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_x_continuous(breaks = seq(1990,2100,20)) +
  scale_y_continuous(labels = label_comma()) +
  labs(y = "Avoided skin cancer cases") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )
p2 <- ggplot(total_series_de, aes(x = year)) +
  geom_ribbon(aes(ymin = dif_lower, ymax = dif_upper),
              fill = "#fcbba1", alpha = 0.35) +
  geom_line(aes(y = dif_mean),
            color = "#cb181d", linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_x_continuous(breaks = seq(1990,2100,20)) +
  scale_y_continuous(labels = label_comma()) +
  labs(y = "Avoided skin cancer deaths") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )
p3 <- ggplot(total_series_daly , aes(x = year)) +
  geom_ribbon(aes(ymin = dif_lower, ymax = dif_upper),
              fill = "#a1d99b", alpha = 0.35) +
  geom_line(aes(y = dif_mean),
            color = "#238b45", linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_x_continuous(breaks = seq(1990,2100,20)) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    x = "Year",
    y = "Avoided cataract-related DALYs"
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    #axis.text.x = element_blank()
  )
library(patchwork)
p <- p1 / p2 / p3
p
filename <- paste("C:/Users/23073/Desktop/avoided_health_total.png")
ggsave(filename, p, width = 10, height = 12, units = "in", dpi = 400)


######################################Change the expression form of average toc difference to percentage change
library(data.table)
library(dplyr)
toc_dif <- fread("C:/Users/23073/Desktop/toc_dif_percentage.csv")
toc_dif$percentage <- toc_dif$ratio*100
grid <- fread("C:/Users/23073/Desktop/country_lon_lat.csv")
total <- merge(toc_dif,grid,by=c("lon","lat"))
df<- fread("C:/Users/23073/Desktop/cost_benefit_ratio_new.csv")
df
countries <- unique(df$country)
result <- total[country %in% countries]

library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(viridis)
library(Polychrome)
world_new <- st_read("C:/Users/23073/Desktop/natural_earth_vector/10m_cultural/ne_10m_admin_0_countries_chn.shp")
world_new <- world_new[,21]
hongkong <- world_new[world_new$NAME_LONG == "Hong Kong", ]
macao <- world_new[world_new$NAME_LONG == "Macao", ]
china <- world_new[world_new$NAME_LONG == "China", ]
china_merged <- rbind(china, hongkong, macao)
china_merged$NAME_LONG <- "China"
china_merged <- china_merged[!(china_merged$NAME_LONG %in% c("Hong Kong", "Macao")), ]
world_new <- rbind(world_new[!(world_new$NAME_LONG %in% c("China", "Hong Kong", "Macao")), ], china_merged)

world_cropped    <- subset(world_merged, NAME_LONG != "Antarctica")
boundary_cropped <- subset(boundary,      NAME_LONG != "Antarctica")

p <- ggplot() +
  geom_sf(data = world_cropped, fill = "grey80", color = NA) +
  geom_tile(data = result, aes(x = lon, y = lat, fill = percentage)) +
  geom_sf(data = world_cropped, fill = NA, color = "white", size = 0.2) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
  scale_fill_viridis(option = "plasma") +
  labs(
    title = "Percentage increase in TCO relative to the BAU scenario",
    x = NULL,
    y = NULL,
    fill = "Percentage (%)"
  ) +
  theme_minimal(base_size = 24) +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    plot.title = element_text(hjust = 0.5)
  )
print(p)

filename <- paste("C:/Users/23073/Desktop/toc_dif_percentage.png")
ggsave(filename, p, width = 12, height = 7, units = "in", dpi = 400)


#################################Change the grid graph representation of radiation differences to percentages
library(data.table)
library(dplyr)
library(tidyr)
ra <- fread("C:/Users/23073/Desktop/ra_annual.csv")
head(ra)
ra$ra_mj <- ra$sum_y*3600/10^6
unique(ra$year)
ra <- ra[ra$year >= 1991,]
ra_nocontrol <- fread("C:/Users/23073/Desktop/ra_annual_Nocontrol.csv")
head(ra_nocontrol)
ra_nocontrol$ra_mj_nocontrol <- ra_nocontrol$sum_y*3600/10^6
unique(ra_nocontrol$year)
ra_nocontrol <- ra_nocontrol[ra_nocontrol$year >= 1991,]

total <- merge(ra,ra_nocontrol,by=c("lon","lat","year"))
total$ra_mj_dif <- total$ra_mj_nocontrol-total$ra_mj

result <- total %>%
  group_by(lat, lon) %>%
  summarise(mean_ra_mj_dif = mean(ra_mj_dif),
            mean_ra_mj_nocontrol=mean(ra_mj_nocontrol),
            ratio=mean_ra_mj_dif/mean_ra_mj_nocontrol)
result <- data.frame(result)

grid <- fread("C:/Users/23073/Desktop/country_lon_lat.csv")
result <- merge(result,grid,by=c("lon","lat"))
df<- fread("C:/Users/23073/Desktop/cost_benefit_ratio_new.csv")
df
countries <- unique(df$country)
setDT(result)
result <- result[country %in% countries]
result$percentage <- result$ratio*100

library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(viridis)
library(Polychrome)
world_new <- st_read("C:/Users/23073/Desktop/natural_earth_vector/10m_cultural/ne_10m_admin_0_countries_chn.shp")
world_new <- world_new[,21]
hongkong <- world_new[world_new$NAME_LONG == "Hong Kong", ]
macao <- world_new[world_new$NAME_LONG == "Macao", ]
china <- world_new[world_new$NAME_LONG == "China", ]
china_merged <- rbind(china, hongkong, macao)
china_merged$NAME_LONG <- "China"
china_merged <- china_merged[!(china_merged$NAME_LONG %in% c("Hong Kong", "Macao")), ]
world_new <- rbind(world_new[!(world_new$NAME_LONG %in% c("China", "Hong Kong", "Macao")), ], china_merged)

world_cropped    <- subset(world_merged, NAME_LONG != "Antarctica")
boundary_cropped <- subset(boundary,      NAME_LONG != "Antarctica")

p <- ggplot() +
  geom_sf(data = world_cropped, fill = "grey80", color = NA) +
  geom_tile(data = result, aes(x = lon, y = lat, fill = percentage)) +
  geom_sf(data = world_cropped, fill = NA, color = "white", size = 0.2) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
  scale_fill_viridis(option = "cividis", na.value = "grey80") +
  labs(
    title = "Percentage reduction in radiation relative to the BAU scenario",
    x = NULL,
    y = NULL,
    fill = "Percentage (%)"
  ) +
  theme_minimal(base_size = 24) +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    plot.title = element_text(hjust = 0.5)
  )
print(p)

filename <- paste("C:/Users/23073/Desktop/UV_dif_percentage.png")
ggsave(filename, p, width = 12, height = 7, units = "in", dpi = 400)

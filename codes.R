################
## EVHOE dataset
################

library(dplyr)
library(ggplot2)
library(mapdata)
library(sampling)
library(sf)

# Load EVHOE data and shapefile of EVHOE
load("data/EVHOE_2008_2019.RData")
evhoe_shp <- st_read("data/STRATES/Agreed_Strata_EVHOE_Polyg_WGS84.shp") %>% 
  dplyr::select(STRATE)
evhoe_shp$area_strata <- as.numeric(st_area(evhoe_shp))

# Species for the analysis
species <- "Merluccius_merluccius"

# Load Map data
mapBase <- map("worldHires", fill = T, plot = F)
mapBase <- st_as_sf(mapBase) %>% filter(ID %in% c("France","Spain","UK","Ireland"))

##-------------------------------------------
## Haul data (locations and haul information)
##-------------------------------------------
Haul_df <- Save_Datras$datras_HH.full %>%
  dplyr::select(Year,long,lati,StNo,HaulNo,Depth)

# Extent of EVHOE (spatial and temporal)
xlims <- range(pretty(Haul_df$long))
ylims <- range(pretty(Haul_df$lati))
year_vec <- unique(Haul_df$Year)
year_vec <- year_vec[order(year_vec)]
n_year <- length(year_vec)

# Convert into sf and cross with EVHOE shapefile
Haul_sf <- st_as_sf(Haul_df,coords=c("long","lati"),crs = st_crs(evhoe_shp))
Haul_sf_2 <- st_intersection(Haul_sf,evhoe_shp)

# Plot hauls for all the years - color are the strata
Haul_plot <- ggplot(Haul_sf_2)+
  geom_sf(aes(col=STRATE))+
  geom_sf(data=mapBase)+
  coord_sf(xlim = xlims, ylim = ylims, expand = FALSE)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5,face = "bold",size=14),
        panel.spacing.x = unit(4, "mm"))+
  ggtitle(species)+
  ylab("")+xlab("")

plot(Haul_plot)

##-----------
## Catch data 
##-----------
# level of aggregation --> haul, sex, size category
Catch_df <- Save_Datras$datras_sp.HL.full %>%
  group_by(Year,long,lati,StNo,HaulNo,scientificname) %>%
  dplyr::summarise(TotalNo = sum(TotalNo))

# Join with haul data to add missing hauls to catch data
Catch_df_2 <- full_join(Catch_df,Haul_df) %>%
  mutate(haul_id = paste0(StNo,"-",HaulNo,"-",Year)) %>% 
  tidyr::pivot_wider(names_from = scientificname,values_from = TotalNo)

# Join with EVHOE shapefile
Catch_sf_2 <- st_as_sf(Catch_df_2,
                       coords = c("long","lati"),
                       crs=st_crs(evhoe_shp)) %>% 
  st_intersection(evhoe_shp)

# Filter species
Catch_sf_3 <- Catch_sf_2 %>% 
  select_at(vars(Year,StNo,HaulNo,STRATE,area_strata,contains(species)))
colnames(Catch_sf_3)[which(colnames(Catch_sf_3) == species)] <- "TotalNo"
Catch_sf_3$TotalNo[which(is.na(Catch_sf_3$TotalNo))] <- 0

# Plot
Evhoe_plot <- ggplot(Catch_sf_3)+
  geom_sf(aes(col=TotalNo))+
  scale_color_distiller(palette="Spectral",trans = 'log10')+
  facet_wrap(.~Year)+
  geom_sf(data=mapBase)+
  coord_sf(xlim = xlims, ylim = ylims, expand = FALSE)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5,face = "bold",size=14),
        panel.spacing.x = unit(4, "mm"))+
  ggtitle(species)+
  ylab("")+xlab("")

plot(Evhoe_plot)

##------------------------------------------------------------
## Compute yearly stratified estimates of population abundance
##------------------------------------------------------------
Catch_sf_4 <- Catch_sf_3 %>% 
  filter(TotalNo > 0) %>% 
  mutate(logTotNo = log(TotalNo))

## For each year, compute the abundance estimates
for(i in 1:n_year){
  
  year_i <- year_vec[i]
  Catch_sf_i <- Catch_sf_4 %>% 
    filter(Year == year_i)
  samp_per_strata <- Catch_sf_i %>% 
    data.frame() %>% 
    dplyr::select(-geometry) %>% 
    group_by(Year,STRATE) %>% 
    summarise(n = n())
  Catch_sf_i2 <- inner_join(Catch_sf_i,samp_per_strata)
  Est_Ab <- HTstrata(y = Catch_sf_i2$TotalNo,
                     pik = Catch_sf_i2$n / (Catch_sf_i2$area_strata * 1e-6),
                     strata = as.numeric(factor(Catch_sf_i2$STRATE)))
  Var_Est_Ab <- varest(Ys=Catch_sf_i2$TotalNo,
                       pik=as.numeric(Catch_sf_i2$n / (Catch_sf_i2$area_strata * 1e-6)))
  
  if(i==1) strate_est_df_full <- data.frame(Year = year_i, Est = Est_Ab, Var_Est = Var_Est_Ab)
  if(i!=1){
    strate_est_df <- data.frame(Year = year_i, Est = Est_Ab, Var_Est = Var_Est_Ab)
    strate_est_df_full <- rbind(strate_est_df_full,strate_est_df)
  }
  
}

# Compute standard deviation and convert sum into mean
strat_mean_plot <- strate_est_df_full %>% 
  mutate(sd_Est = sqrt(Var_Est))

ggplot()+
  geom_ribbon(data=strat_mean_plot,
              aes(x=Year,y=Est,
                  ymin = Est - 1.96*sd_Est,
                  ymax = Est + 1.96*sd_Est),
              fill = "red",alpha=0.3)+
  geom_line(data=strat_mean_plot,aes(x=Year,y=Est),col="red")+
  ylim(0,NA)+
  theme_classic()


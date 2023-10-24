################
## EVHOE dataset
################

library(dplyr)
library(ggplot2)
library(mapdata)
library(sf)

# Load EVHOE data
load("EVHOE_2008_2019.RData")

species <- "Solea_solea"

# Load Map data
mapBase <- map("worldHires", fill = T, plot = F)
mapBase <- st_as_sf(mapBase) %>% filter(ID %in% c("France","Spain","UK","Ireland"))

# Haul data
Haul_df <- Save_Datras$datras_HH.full %>%
  dplyr::select(Year,long,lati,StNo,HaulNo,Depth)

# Extent of the EVHOE domain
xlims <- range(pretty(Haul_df$long))
ylims <- range(pretty(Haul_df$lati))

indiv_data <- Save_Datras$datras_sp.CA.full %>% 
  filter(scientificname == species)

plot(indiv_data$LngtClass,indiv_data$IndWgt,main = "Length weight relaionship")

# Catch data
Catch_df <- Save_Datras$datras_sp.HL.full %>%
  group_by(Year,long,lati,StNo,HaulNo,scientificname) %>%
  dplyr::summarise(CatchWgt = CatCatchWgt)

# Join with haul data to add missing hauls to catch data
Catch_df_2 <- full_join(Catch_df,Haul_df) %>%
  filter(scientificname == "Argentina_sphyraena")
Catch_df_2$CatchWgt[which(is.na(Catch_df_2$CatchWgt))] <- 0

# Plot
Evhoe_plot <- ggplot(Catch_df_2)+
  geom_point(aes(x=long,y=lati,col=CatchWgt))+
  scale_color_distiller(palette="Spectral",trans="log10")+
  facet_wrap(.~Year)+
  geom_sf(data=mapBase)+
  coord_sf(xlim = xlims, ylim = ylims, expand = FALSE)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5,face = "bold",size=14),
        panel.spacing.x = unit(4, "mm"))+
  ggtitle("Argentina sphyraena (EVHOE)")+
  ylab("")+xlab("")

plot(Evhoe_plot)

# plot

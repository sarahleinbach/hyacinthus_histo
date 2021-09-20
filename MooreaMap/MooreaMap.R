library(ggplot2)
theme_set(theme_bw())
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(ggspatial)

setwd("/Users/lumosmaximma/Desktop")
map = st_read("data/PYF_adm0.shp")
bathy = read.table("data/bathy_LIDAR_Riegl_820_05m.xyz")
reg = griddify(bathy, nlon =1000, nlat = 1000)
bath.p = rasterToPoints(reg)
bath.df = data.frame(bath.p)
colnames(bath.df) = c("x", "y", "alt")

site = data.frame(longitude = -149.8177, latitude = -17.4731)

#moorea island map without the surrounding reef
# moorea = ggplot(map)+
#   geom_sf()+
#   geom_point(data=site, aes(x = longitude, y = latitude), size = 5)+
#   coord_sf(xlim = c(-149.95,-149.72), ylim = c(-17.6,-17.42))+
#   annotation_scale(location="bl",width_hint=0.5)+
#   annotation_north_arrow(location="bl",which_north="true",
#                          pad_x = unit(0.7,"in"),
#                          pad_y = unit(0.25,"in"),
#                          style = north_arrow_fancy_orienteering(),
#                          height = unit(1.2, "cm"),
#                          width = unit(1.2, "cm"))+
#   theme_classic(base_size=11)+
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_rect(colour = "black", size=1, fill="#dbf8fa"),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank())
# moorea
# ggsave("moorea.pdf", width=8, height=7.6, units="in")

#moorea island map with the surrounding reef
mooreareef = ggplot(map)+
  geom_point(data=subset(bath.df, bath.df$alt<3 & bath.df$alt>0), aes(x,y),color = grey(0.92), size = 0.01, show.legend=FALSE) +
  geom_sf(data = map, color = "black", fill = "#a0a0a0") +
  geom_point(data=site, aes(x = longitude, y = latitude), size = 5)+
  coord_sf(xlim = c(-149.94,-149.74), ylim = c(-17.61,-17.42))+
  annotation_scale(location="bl",width_hint=0.5)+
  annotation_north_arrow(location="bl",which_north="true",
                       pad_x = unit(0.7,"in"),
                       pad_y = unit(0.25,"in"),
                       style = north_arrow_fancy_orienteering(),
                       height = unit(1.2, "cm"),
                       width = unit(1.2, "cm"))+
  theme_classic(base_size=16)+
  theme(panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(colour = "black", size=1, fill=NA),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_text(color = "black"))
mooreareef
ggsave("mooreareef.pdf", width=8, height=7.6, units="in")

# #polynesia = ggplot(map)+
#   geom_sf()+
#   coord_sf(xlim = c(-155,-137), ylim = c(-25,-13))+
#   geom_rect(xmin = -150.15, xmax = -149.2, ymin = -18, ymax = -17.1, 
#             fill = NA, colour = "black", size = 1)+
#   theme_classic(base_size=11)+
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank())
# #polynesia

# pacific = ggplot()+
#   borders("world2", fill = "#a0a0a0", col= NA)+
#   theme_classic(base_size=11)+
#   coord_cartesian(xlim = c(110, 290), ylim = c(-30,30))+
#   geom_point(aes(x = 210, y = -17), size = 5, shape = 0)+
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         axis.line = element_blank(),
#         panel.border = element_rect(colour = "black", fill=NA, size=1),
#         panel.background = element_rect(colour = "black", size=1))
# pacific
# ggsave("pacific.pdf", width=4.5, height=2, units="in")

#another format for the moorea map without surrounding reef
#moorea2 = ggplot(map)+
  # geom_point(data=subset(bath.df, bath.df$alt<3 & bath.df$alt>0), aes(x,y),color = grey(0.92), size = 0.01, show.legend=FALSE) +
  # geom_sf(data = map, color = "black", fill = "#a0a0a0") +
  # geom_point(data=site, aes(x = longitude, y = latitude), size = 5)+
  # coord_sf(xlim = c(-149.92,-149.73), ylim = c(-17.61,-17.45))+
  # annotation_scale(location="br",width_hint=0.5)+
  # annotation_north_arrow(location="bl",which_north="true",
  #                        pad_x = unit(0.7,"in"),
  #                        pad_y = unit(0.25,"in"),
  #                        style = north_arrow_fancy_orienteering(),
  #                        height = unit(1.2, "cm"),
  #                        width = unit(1.2, "cm"))+
  # theme_classic(base_size=11)+
  # theme(panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank(),
  #       panel.background = element_rect(colour = "black", size=1),
  #       axis.title.x = element_blank(),
  #       axis.title.y = element_blank())
#moorea2
# ggsave("moorea2.pdf", width=8, height=7.6, units="in")




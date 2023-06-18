library(sfnetworks)
library(dplyr)
library(sf)
library(tidygraph) #equivalent dplyr pour les graphs
library(ggplot2)

# A partir de roads, on garde les routes qui correspondent au périphérique
periph_simple <- roads %>% 
  filter(!is.na(name))%>% 
  filter(name %in% c("Boulevard Périphérique Intérieur", "Pont Masséna","Tunnel Lilas","Pont Amont","Pont Aval")) %>% 
  select(name)
plot(periph_simple)

# sfnetwork permet de gérer les réseaux géospatiaux.
# On transforme periph_simple en ce type d'objet
net  = as_sfnetwork(st_geometry(periph_simple))
plot(net)
# to_spatial_smooth : on enlève les pseudos-noeuds en 
# préservant la connectivité du réseau
nets = convert(net,to_spatial_smooth)
plot(nets)

# On calcule la longueur des edges, on trie par ordre décroissant
nets = nets %>% 
  activate(edges) %>% #on travaille sur les liens (et non les noeuds)
  mutate(length=st_length(x)) %>% 
  arrange(desc(length)) %>% 
  mutate(lid=1:n()) %>% 
  filter(lid %in% c(1,2,3,5)) #je ne garde que les grands accès
#plot(nets) 

#On transforme le tout en lignes
lines.geom = nets %>% activate(edges) %>% st_geometry() 
lines = st_sf(lines.geom,id=1:length(lines.geom))
plot(lines)

# On prend une ligne et on met un point tous les 100 mètres
points_eqd = lines  %>% st_line_sample(density = 1/100)
lines_eqd = lines  %>% st_line_sample(density = 1/100) %>% st_cast("LINESTRING")
plot(points_eqd)

# Je découpe ma ligne avec mes points
geo_col = lwgeom::st_split(lines_eqd,points_eqd)
lines_final.geom = st_collection_extract(geo_col,type = "LINESTRING")
lines_final = st_sf(lines_final.geom,id=1:length(lines_final.geom))
plot(lines_final)

# On fait la jointure avec les accidents
periph_count= lines_final %>% st_buffer(50) %>% 
  st_join(accidents.2019.paris %>% filter(!duplicated(Num_Acc))) %>% 
  filter(!is.na(Num_Acc)) %>% 
  count(id)
# On met la bonne géométrie à periph_count
st_geometry(periph_count)=lines_final.geom[match(periph_count$id,lines_final$id)]

# On fait une carte
ggplot(periph_count) + 
  #ggspatial::annotation_map_tile(zoom=13,type="stamenbw") + 
  geom_sf(data=roads.geom, colour = "#666666",size=0.5)+
  geom_sf(aes(color=n),size=3)+
  scale_color_distiller("",palette = "Reds",direction=1)+
  coord_sf(crs = 2154, datum = NA) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white",color=NA),
        plot.background = element_rect(fill = "white",color=NA)) +
  labs(title = "Nombre de personnes accidentées sur le périphérique",
       subtitle = "en 2019, par portion de 100m",
       caption = "fichier BAAC 2019, ONISR\nantuki & comeetie, 2021",x="",y="")

# Ajout de st_centroid pour les 100m qui dépassent
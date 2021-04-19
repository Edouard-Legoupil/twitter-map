### setup ----------------------------------------------------------------------

# install and/or load packages
pkg <- c("tidyverse", "maps", "geonames"
         #, "ggmap", "here"
         )
sapply(pkg, function(x){
  if(! x %in% installed.packages()) install.packages(x)
  require(x, character.only = TRUE)
})

# https://taraskaduk.com/posts/2017-11-26-pixel-maps/



### Theme -------------------------------------------------------------------


theme <- theme_void() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))


### dot world grid -------------------------------------------------------------------

# generate worldwide dots
dots <- tibble(lat = seq(-89.5, 89.5, by = 2.65)) %>% 
  merge(tibble(long = seq(-180, 180, by = 2.65)), all = TRUE) %>%  
  # exclude water-based dots 
  mutate(country = map.where('world', long, lat),
         lakes = map.where('lakes', long, lat)) %>% 
  filter(!is.na(country) & is.na(lakes)) %>% 
  select(-lakes)

ggplot() +   
  #base layer of map dots
  geom_point(data = dots, 
             aes(x=long, y = lat), 
            # col = "#3E0097", 
             #col = "#666666",
            col = "#d1d1d1",
             size = 1.3) +
  theme + 
  scale_y_continuous(limits = c(-65, 85), expand = c(0,0)) +
  scale_x_continuous(limits = c(-150,160), expand = c(0,0)) -> dot_map
dot_map
# save plot
ggsave(here("worlmap_dots.png"), dot_map, 
       dpi = 600, width = 8, height = 4.5)


### Better dot world grid -------------------------------------------------------------------

gridnice <- read.csv("gridnice.csv")
ggplot() +   
  #base layer of map dots
  geom_point(data = gridnice, 
             aes(x=x, y = y), 
             col = "#3E0097", 
             #col = "#666666",
             # col = "#d1d1d1",
             size = 1.3) +
  theme-> nicegr 


nicegr
# save plot
ggsave(here("nicegr.png"), nicegr, 
       dpi = 600, width = 8, height = 4.5)

### Matching the 2 grids -------------------------------------------------------------------


### Location to geocode to grid -------------------------------------------------------------------
# https://docs.google.com/spreadsheets/d/1iyWkr7kGhHCNNBq3aaph782bHM2MHguVZeZMWqVQe24/edit#gid=0
locations <- read.csv("CV - locations.csv")

city <- as.character(locations[ 1, c("city")])
ctry <- as.character(locations[ 1, c("ctry")])
flag <- as.character(locations[ 1, c("flag")])
cat(paste0("searching for ", city, "\n"))
rm(results)
results <- GNsearch(name_equals = city ,
                    #q = city ,
                    
                    country = ctry,
                    featureClass = "P",
                    #  continentCode = "AF",
                    #  fuzzy = "0.6",
                    maxRows = 20)
cat(paste0("Results potential ", nrow(resulti), "\n"))
results$city  <- city
results$ctry  <- ctry
results$flag  <- flag
  
for (i in 2:nrow(locations)) {
  #i <- 1
  city <- as.character(locations[ i, c("city")])
  ctry <- as.character(locations[ i, c("ctry")])
  flag <- as.character(locations[ i, c("flag")])
  cat(paste0("searching for ", city, "\n"))
  
  resulti <- GNsearch(name_equals = city ,
                      #q = city ,
                      
                      country = ctry,
                      featureClass = "P",
                    #  continentCode = "AF",
                    #  fuzzy = "0.6",
                      maxRows = 20)
  cat(paste0("Results potential ", nrow(resulti), "\n"))
  ## All good only one match...
  if (nrow(resulti) >0 ) {
    resulti$city  <- city
    resulti$ctry  <- ctry
    resulti$flag  <- flag
    results <- dplyr::bind_rows(results, resulti) }
  
  ## If not, passing...
  else { results <- results}
}

results$lat <- as.numeric(results$lat)
results$lng <- as.numeric(results$lng)
results$population <- as.numeric(results$population)


locations2 <- results %>%
               select(city, countryName, lat, lng, flag, population)  %>% 
               group_by(city) %>% 
               top_n(1, population)


### First map with simple grid -------------------------------------------------------------------


dot_map + 
  #plot all the places I've been to
  geom_point(data = locations2, 
             aes(x=lng, y=lat), 
             color="#3E0097", 
             size=0.8) -> dot_map2 
  #plot all the places I lived in, using red
  # geom_point(data = locations2 %>% 
  #              filter(flag == 'lived'), 
  #            aes(x=lng, y=lat), 
  #            color="red", 
  #            size=0.8) +
  # #an extra layer of halo around the places I lived in
  # geom_point(data = locations2 %>% 
  #              filter(flag == 'lived'), 
  #            aes(x=lng, y=lat), 
  #            color="red", 
  #            size=0.6, 
  #           alpha = 0.4) 
dot_map2
# save plot
ggsave(here("visited.png"), dot_map2, 
         dpi = 600, width = 8, height = 4.5)

### Second map with better grid -------------------------------------------------------------------


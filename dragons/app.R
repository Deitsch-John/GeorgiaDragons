library(shiny)
library(sf)
library(tidyverse)
library(ggspatial)

obs_to_coords <- function(df, coord_col, crs_add){
  coords.sfg <- df$coords
  coords.sfc <- st_sfc(coords.sfg, crs = crs_add)
  
  df %>%
    st_as_sf(geometry = coords.sfc)%>%
    st_transform(., crs = crs_add)
  
}

url = "https://github.com/Deitsch-John/GeorgiaDragons/raw/main/data.zip"
download.file(url, "data.zip")
unzip("data.zip")

UScounties = read_sf("US_counties", dsn = ".")
rawdata <- read_tsv("OdonataGA.csv")

GeorgiaCounties <- UScounties%>%
  filter(STATE%in%c("GA"))%>%
  dplyr::select(COUNTYNAME)

data <- rawdata %>%
  dplyr::select(species, family, decimalLatitude, decimalLongitude, eventDate, month, year,
                institutionCode)%>%
  filter(!is.na(eventDate))%>%
  mutate(MonthName = lubridate::month(eventDate, label = TRUE, abbr = FALSE),
         yearday = lubridate::yday(eventDate),
         week = lubridate::week(eventDate))%>%
  rowwise()%>%
  mutate(coords = list(st_point(c(decimalLongitude, decimalLatitude))))%>%
  mutate(species2 = case_when(
    species=="Gomphus apomyius"~"Hylogomphus apomyius",
    species=="Gomphus australis"~"Phanogomphus australis",
    species=="Gomphus cavillaris"~"Phanogomphus cavillaris",
    species=="Gomphus consanguis"~"Stenogomphus consanguis",
    species=="Gomphus exilis"~"Phanogomphus exilis",
    species=="Gomphus hybridus"~"Gomphurus hybridus",
    species=="Gomphus lineatifrons"~"Gomphurus lineatifrons",
    species=="Gomphus lividus"~"Phanogompus lividus",
    species=="Gomphus minutus"~"Phanogomphus minutus",
    species=="Gomphus parvidens"~"Hylogomphus parvidens",
    species=="Gomphus rogersi"~"Stenogomphus rogersi",
    species=="Gomphus vastus"~"Gomphurus vastus",
    species=="Gomphus viridifrons"~"Hylogomphus viridifrons"
  ))%>%
  mutate(Species_cleaned = ifelse(is.na(species2)==TRUE, species, species2))

data.sf <- obs_to_coords(data, coords, 4269)%>%
  dplyr::select(-species2, -species)

SpeciesMap <- function(speciestomap) {
  
  TaxaData <- data.sf %>%
    filter(Species_cleaned==speciestomap)
  
  TaxaByCounty <- st_join(TaxaData, GeorgiaCounties)%>%
    st_drop_geometry()%>%
    group_by(COUNTYNAME)%>%
    summarize(Observations = n())%>%
    filter(!is.na(COUNTYNAME))
  
  TaxaToMap <- GeorgiaCounties %>%
    left_join(TaxaByCounty)%>%
    mutate(Present = ifelse(is.na(Observations), "No", "Yes"))
  
  attach(TaxaToMap)
  
  TaxaMap <- ggplot()+
    geom_sf(data = TaxaToMap, 
            fill = ifelse(Present=="Yes", "seagreen3", "ivory1"), 
            color = "grey41", 
            size = 0.10)+
    theme(
      panel.grid.major = element_line(linetype = "dashed", 
                                      color = "grey88"),
      panel.background = element_rect(fill = "aliceblue"),
      panel.border = element_rect(fill = NA, size = 2))+
    annotation_scale(location="bl")+
    annotation_north_arrow(which_north = "true",
                           style = north_arrow_nautical,
                           location = "tr")+
    labs(title = speciestomap)
  
  return(TaxaMap)
}

SpeciesList <- data.sf%>%
  st_drop_geometry()%>%
  dplyr::select(Species_cleaned)%>%
  distinct()%>%
  arrange(Species_cleaned)%>%
  pull(Species_cleaned)


#__________________________________________________

#Define UI
ui <- fluidPage(
  selectInput(inputId = "species", label = "Species", choices = SpeciesList),
  plotOutput("Map")
)

#Define server
server <- function(input, output, session) {
  output$Map <- renderPlot(SpeciesMap(input$species))
}


# Run the application 
shinyApp(ui = ui, server = server)

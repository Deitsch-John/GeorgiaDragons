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

dobbs_data <- read_csv("mdobbs.csv")%>%
  rename(species = `...1`)%>%
  rename(name = `...2`)%>%
  select(-`...3`, -`...163`, -`...164`)%>%
  mutate_all(~replace(., is.na(.), 0))%>%
  pivot_longer(Appling:Worth, names_to = "County", values_to = "Observed")

GeorgiaCounties <- UScounties%>%
  filter(STATE%in%c("GA"))%>%
  dplyr::select(COUNTYNAME)%>%
  rename(County=COUNTYNAME)

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
  
  TaxaData.dobbs <- dobbs_data %>%
    filter(species==speciestomap)
  
  TaxaByCounty <- st_join(TaxaData, GeorgiaCounties)%>%
    st_drop_geometry()%>%
    group_by(County)%>%
    summarize(Observations = n())%>%
    filter(!is.na(County))
  
  TaxaToMap <- GeorgiaCounties %>%
    left_join(TaxaByCounty)%>%
    mutate(Present = ifelse(is.na(Observations), "No", "Yes"))%>%
    left_join(TaxaData.dobbs)%>%
    mutate(Present_dobbs = ifelse(Present=="No" & Observed=="X", "Yes", Present))

  attach(TaxaToMap)
  
  TaxaMap <- ggplot()+
    geom_sf(data = TaxaToMap, 
            fill = ifelse(Present_dobbs=="Yes", "seagreen3", "ivory1"), 
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
county_table <- function(speciestomap){
  TaxaData <- data.sf %>%
    filter(Species_cleaned==speciestomap)
  
  TaxaByCounty <- st_join(TaxaData, GeorgiaCounties)%>%
    st_drop_geometry()%>%
    group_by(COUNTYNAME)%>%
    summarize(Observations = n())%>%
    filter(!is.na(COUNTYNAME))%>%
    arrange(-Observations)%>%
    gt::gt()
  
  return(TaxaByCounty)
}
species_list <- function(county){
  CountyList <- st_join(data.sf, GeorgiaCounties)%>%
    st_drop_geometry()%>%
    filter(COUNTYNAME==county)%>%
    dplyr::select(Species_cleaned)%>%
    distinct()%>%
    arrange(Species_cleaned)%>%
    rename(Species = Species_cleaned)%>%
    filter(!is.na(Species))%>%
    gt::gt()
  
  return(CountyList)
}
CountyMap <- function(county){
  obs_county <- st_join(data.sf, GeorgiaCounties)%>%
    filter(COUNTYNAME==county)
  
  Extent <- GeorgiaCounties %>%
    filter(COUNTYNAME==county)%>%
    st_bbox()
  
  ggplot()+
    geom_sf(data = GeorgiaCounties,
            fill = "grey96",
            color = "grey41", 
            size = 0.10)+
    geom_sf(data = filter(GeorgiaCounties, COUNTYNAME==county),
            fill = "grey99",
            color = "grey22",
            size = 0.8)+
    geom_sf(data = obs_county,
            color = "firebrick2",
            alpha = 0.8,
            size = 2)+
    coord_sf(ylim = c(Extent[2], Extent[4]), 
             xlim = c(Extent[1], Extent[3]))+
    theme(
      panel.grid.major = element_line(linetype = "dashed", 
                                      color = "grey88"),
      panel.background = element_rect(fill = "aliceblue"),
      panel.border = element_rect(fill = NA, size = 2))+
    annotation_scale(location="bl")+
    annotation_north_arrow(which_north = "true",
                           style = north_arrow_nautical,
                           location = "tr")+
    labs(title = county)
  
}

SpeciesList <- data.sf%>%
  st_drop_geometry()%>%
  dplyr::select(Species_cleaned)%>%
  distinct()%>%
  arrange(Species_cleaned)%>%
  pull(Species_cleaned)

CountyList <- GeorgiaCounties %>%
  st_drop_geometry()%>%
  dplyr::select(COUNTYNAME)%>%
  distinct()%>%
  arrange(COUNTYNAME)%>%
  pull(COUNTYNAME)

#__________________________________________________

#Define UI
ui <- fluidPage(
  titlePanel("Georgia Odonata Mapper"),
  tabsetPanel(
    tabPanel("Explore data by species",
             fluidRow(column(4, 
                             selectInput(inputId = "species", label = "Choose Species", choices = SpeciesList),
                             tableOutput("Summary_Stats"),
             ),
             column(8, 
                    plotOutput("Map")
             )
             )),
    tabPanel("Explore data by county",
             fluidRow(column(4,
             selectInput(inputId = "county", label = "Choose County", choices = CountyList),
             tableOutput("County_List"),
             ),
             column(8,
                    plotOutput("Map_county")
            )
    )
  )
)
)

#Define server
server <- function(input, output, session) {
  output$Map <- renderPlot(SpeciesMap(input$species))
  output$Summary_Stats <- renderTable(county_table(input$species))
  output$County_List <- renderTable(species_list(input$county))
  output$Map_county <- renderPlot(CountyMap(input$county))
}


# Run the application 
shinyApp(ui = ui, server = server)

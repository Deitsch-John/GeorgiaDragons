library(shiny)
library(sf)
library(tidyverse)
library(ggspatial)
library(gt)

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
  select(-`...3`, -`...163`, -`...163`)%>%
  mutate_all(~replace(., is.na(.), 0))%>%
  pivot_longer(Appling:Worth, names_to = "County", values_to = "Observed")%>%
  filter(Observed %in% c("X", "0"))%>%
  mutate(Observed = case_when(
    Observed=="X^"~"X",
    Observed=="X"~"X",
    Observed=="0"~"0"
  ))

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
    select(-species, -name)%>%
    mutate(Present_dobbs = case_when(
      Present=="Yes" & Observed=="X"~"Observed",
      Present=="Yes" & Observed=="0"~"Observed",
      Present=="No" & Observed=="X"~"Observed",
      Present=="No" & Observed=="0"~"Not Observed"
    ))

  attach(TaxaToMap)
  
  TaxaMap <- ggplot()+
    geom_sf(data = TaxaToMap, 
            fill = ifelse(Present_dobbs=="Observed", "seagreen3", "ivory1"), 
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
CountyMap <- function(county){
  obs_county <- st_join(data.sf, GeorgiaCounties)%>%
    filter(County==county)
  
  Extent <- GeorgiaCounties %>%
    filter(County==county)%>%
    st_bbox()
  
  ggplot()+
    geom_sf(data = GeorgiaCounties,
            fill = "grey96",
            color = "grey41", 
            size = 0.10)+
    geom_sf(data = filter(GeorgiaCounties, County==county),
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
dobbs_species_list <- function(county){
  list.df <- dobbs_data %>%
    filter(Observed=="X")%>%
    filter(County==county)%>%
    select(species, Observed)
  
  return(list.df)
}
dobbs_county_table <- function(speciestomap){
  list.df <- dobbs_data %>%
    filter(Observed=="X")%>%
    filter(species==speciestomap)%>%
    select(Observed, County)
  
  return(list.df)
}
county_table <- function(speciestomap){
  OnlineData <- data.sf %>%
    filter(Species_cleaned==speciestomap)
  
  OnlineByCounty <- st_join(OnlineData, GeorgiaCounties)%>%
    st_drop_geometry()%>%
    group_by(County)%>%
    summarize(Observations = n())%>%
    filter(!is.na(County))
  
  DobbsList <- dobbs_county_table(speciestomap)
  
  FullList <- full_join(OnlineByCounty, DobbsList)%>%
    mutate(Observations2 = ifelse(is.na(Observations)==TRUE, "Not reported online", Observations))%>%
    select(County, Observations2)%>%
    rename(Observations=Observations2)
  
  Table <- FullList %>%
    arrange(County)%>%
    gt::gt()%>%
    tab_header(
      title = str_c("County level data for ", speciestomap, "."),
      subtitle = str_c(speciestomap, " has been observed in ", nrow(FullList), " counties in Georgia.")
    )%>%
    tab_source_note(
      source_note = "Taxa with no online observations or museum specimens are sourced from an incredible data archive compiled by Marion Dobbs."
    )
  
  return(Table)
}
species_list <- function(county){
  
  OnlineList <- st_join(data.sf, GeorgiaCounties)%>%
    st_drop_geometry()%>%
    filter(County==county)%>%
    dplyr::select(Species_cleaned)%>%
    group_by(Species_cleaned)%>%
    summarize(Observations = n())%>%
    arrange(Species_cleaned)%>%
    rename(Species = Species_cleaned)%>%
    filter(!is.na(Species))
  
  DobbsList <- dobbs_species_list(county)%>%
    rename(Species=species)
  
  FullList <- full_join(OnlineList, DobbsList)%>%
    mutate(Observations2 = ifelse(is.na(Observations)==TRUE, "Not reported online", Observations))%>%
    select(Species, Observations2)%>%
    rename(Observations=Observations2)
  
  table <- FullList %>%
    arrange(Species)%>%
    gt::gt()%>%
    tab_header(
      title = str_c(county, " county species list."),
      subtitle = str_c(nrow(FullList), " species recorded.")
    )%>%
    tab_source_note(
      source_note = "Taxa with no online observations or museum specimens are sourced from an incredible data archive compiled by Marion Dobbs."
    )
  
  return(table)
}
DiversityMap <- function(dfx){
  online <- st_join(dfx, GeorgiaCounties)%>%
    st_drop_geometry()%>%
    select(Species_cleaned, County)%>%
    rename(species=Species_cleaned)
  
  dobbs <- dobbs_data %>%
    filter(Observed=="X")%>%
    select(species, County)
  
  full <- bind_rows(online, dobbs)
  
  full_stats <- full %>%
    group_by(County)%>%
    summarize(Species = length(unique(species)))%>%
    filter(!is.na(County))
  
  full_stats_mapping <- GeorgiaCounties %>%
    left_join(full_stats)%>%
    mutate(Diversity = case_when(
      Species >= 90 ~ "Very High (90+)",
      Species >= 70 & Species < 90 ~ "High (70-89)",
      Species >= 40 & Species < 70 ~ "Medium (40-69)",
      Species > 15 & Species < 40 ~ "Low (16-39)",
      Species <= 15 ~ "Low Data (<15)",
      is.na(Species)=="TRUE"~"No Data"
    ))
  
  full_stats_mapping$Diversity <- factor(full_stats_mapping$Diversity,
                                         levels = c("Very High (90+)", "High (70-89)",
                                                    "Medium (40-69)", "Low (16-39)", 
                                                    "Low Data (<15)", "No Data"))
  
  Map <- ggplot()+
    geom_sf(data = full_stats_mapping, aes(fill = Diversity), 
            color = "grey41", 
            size = 0.10)+
    scale_fill_brewer(type = "qual", palette = "Spectral", 
                      direction = 1)+
    theme(
      panel.grid.major = element_line(linetype = "dashed", 
                                      color = "grey88"),
      panel.background = element_rect(fill = "aliceblue"),
      panel.border = element_rect(fill = NA, size = 2))+
    annotation_scale(location="bl")+
    annotation_north_arrow(which_north = "true",
                           style = north_arrow_nautical,
                           location = "tr")+
    labs(title = "Species Diversity of Odonates in Georgia")
  
  return(Map)
}

SpeciesList <- data.sf%>%
  st_drop_geometry()%>%
  dplyr::select(Species_cleaned)%>%
  distinct()%>%
  arrange(Species_cleaned)%>%
  pull(Species_cleaned)

CountyList <- GeorgiaCounties %>%
  st_drop_geometry()%>%
  dplyr::select(County)%>%
  distinct()%>%
  arrange(County)%>%
  pull(County)

#__________________________________________________

#Define UI
ui <- fluidPage(
  titlePanel("Georgia Odonata Mapper"),
  tabsetPanel(
    tabPanel("Explore data by species",
             fluidRow(column(4, 
                             selectInput(inputId = "species", label = "Choose Species", choices = SpeciesList),
                             gt_output("Summary_Stats"),
             ),
             column(8, 
                    plotOutput("Map")
             )
             )),
    tabPanel("Explore data by county",
             fluidRow(column(4,
             selectInput(inputId = "county", label = "Choose County", choices = CountyList),
             gt_output("County_List"),
             ),
             column(8,
                    plotOutput("Map_county")
            )
    )
  ),
    tabPanel("Statewide Species Diversity",
             fluidRow(column(12,
                             plotOutput("Map_diversity")
                             )))
)
)

#Define server
server <- function(input, output, session) {
  output$Map <- renderPlot(SpeciesMap(input$species))
  output$Summary_Stats <- render_gt(county_table(input$species))
  output$County_List <- render_gt(species_list(input$county))
  output$Map_county <- renderPlot(CountyMap(input$county))
  output$Map_diversity <- renderPlot(DiversityMap(data.sf))
}


# Run the application 
shinyApp(ui = ui, server = server)

library(shiny)
library(sf)
library(readxl)
library(tidyverse)
library(leaflet)

# Import base de données
leg <- read_excel("data/legislatives_2024.xlsx", col_names = TRUE)

# Import carte
leg_carte <- st_read("data/contours_circo.shp")

# suppression des 0 à gauche
leg_carte <- leg_carte |> 
  mutate(id_circo = str_remove(id_circo, "^0+"))

# jointure dataframes
legislatives <- left_join(leg, leg_carte, by = c("Code circonscription législative" = "id_circo"))


# colonnes à sélectionner 
colonnes <- c(
  "Code département", "Libellé département",
  "Code circonscription législative",
  "Libellé circonscription législative", "libelle",
  "Nom candidat 1", "Prénom candidat 1", "Nuance candidat 1", "Elu 1",
  "Nom candidat 2", "Prénom candidat 2", "Nuance candidat 2", "Elu 2",
  "Nom candidat 3", "Prénom candidat 3", "Nuance candidat 3", "Elu 3",
  "Nom candidat 4", "Prénom candidat 4", "Nuance candidat 4", "Elu 4",
  "Nom candidat 5", "Prénom candidat 5", "Nuance candidat 5", "Elu 5",
  "Nom candidat 6", "Prénom candidat 6", "Nuance candidat 6", "Elu 6",
  "Nom candidat 7", "Prénom candidat 7", "Nuance candidat 7", "Elu 7",
  "Nom candidat 8", "Prénom candidat 8", "Nuance candidat 8", "Elu 8",
  "Nom candidat 9", "Prénom candidat 9", "Nuance candidat 9", "Elu 9",
  "Nom candidat 10", "Prénom candidat 10", "Nuance candidat 10", "Elu 10",
  "Nom candidat 11", "Prénom candidat 11", "Nuance candidat 11", "Elu 11",
  "Nom candidat 12", "Prénom candidat 12", "Nuance candidat 12", "Elu 12",
  "Nom candidat 13", "Prénom candidat 13", "Nuance candidat 13", "Elu 13",
  "Nom candidat 14", "Prénom candidat 14", "Nuance candidat 14", "Elu 14",
  "Nom candidat 15", "Prénom candidat 15", "Nuance candidat 15", "Elu 15",
  "Nom candidat 16", "Prénom candidat 16", "Nuance candidat 16", "Elu 16",
  "Nom candidat 17", "Prénom candidat 17", "Nuance candidat 17", "Elu 17",
  "Nom candidat 18", "Prénom candidat 18", "Nuance candidat 18", "Elu 18",
  "Nom candidat 19", "Prénom candidat 19", "Nuance candidat 19", "Elu 19",
  "geometry"
)

legislatives <- legislatives |> 
  select(all_of(colonnes))


# Onglet 1 :






# Onglet 2 : Florian

## Carte interactive

leaflet(leg_carte) |>
  addTiles() |>  # Fond de carte par défaut
  addPolygons(
    label = lapply(1:nrow(legislatives), function(i) {
      sprintf(
        "<div style='width:120px; text-align:center;'>
           <div style='display:flex;'>
             <div style='width:50%%; height:30px; background-color:red; color:white;'>%s</div>
             <div style='width:50%%; height:30px; background-color:blue; color:white;'>%s</div>
           </div>
           <div style='display:flex;'>
             <div style='width:50%%; height:30px; background-color:green; color:white;'>%s</div>
             <div style='width:50%%; height:30px; background-color:orange; color:white;'>%s</div>
           </div>
         </div>",
        legislatives$val1[i], legislatives$val2[i], legislatives$val3[i], legislatives$val4[i]
      )
    }) |> lapply(htmltools::HTML),  
    labelOptions = labelOptions(
      style = list(
        "background-color" = "white", "border" = "1px solid black", "padding" = "5px"
      ),
      direction = "auto"
    )
  )
# Polygons représentant les circonscriptions
  





# Onglet 3 :







# Onglet 4 :








# Onglet 5 : Juliette










#---- app principale ------

library(shiny)

ui<-fluidPage(
  fluidRow(
    tabsetPanel(
      tabPanel(
        "Onglet un"
      ),
      tabPanel("Onglet deux"),
      tabPanel("Onglet trois"),
      tabPanel(
        "onglet quatre"
      ),
      tabPanel(
        "onglet cinq"
      )
    )
  ),
  fluidRow(
    column(6,
           "truc"
    ),
    column(6,
           "machin"
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # draw the histogram with the specified number of bins
    hist(x,
         breaks = bins, col = "lightblue", border = "white",
         xlab = "Temps d'attente avant le prochain jet (en minutes)",
         ylab = "Fréquence",
         main = "Histogramme du temps d'attente"
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)



# ----------------





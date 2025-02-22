# ----- PACKAGES -----

library(shiny)
library(sf)
library(tidyverse)
library(leaflet)
library(htmltools)
library(stringi)
library(DT)





# ----- IMPORTS -----

## Circonscriptions et candidats et résultats
legislatives <- read_rds("data/legislatives.rds")

## Circonscriptions non affichées sur la carte
legislatives_empty <- read_rds("data/legislatives_empty.rds")

## Villes et coordonnées géographiques
villes <- read_rds("data/villes.rds")





# ----- ONGLET 1 : INFORMATIONS GENERALES -----










# ----- ONGLET 2 : CARTE DES CIRCONSCRIPTIONS - Florian -----

# Couleurs par parti
couleurs <- c(
  "Extrême Gauche"          = "#A50026",  # Rouge très foncé
  "France Insoumise"        = "#D1191B",  # Rouge foncé
  "Union de la Gauche"      = "#D73027",  # Rouge vif
  "Parti Socialiste"        = "#F46D43",  # Rouge orangé
  "Écologistes"             = "#1A9850",  # Vert soutenu
  "Divers Gauche"           = "#FDAE61",  # Orange clair
  "Ensemble"                = "#FFD700",  # Jaune doré
  "Divers Centre"           = "#FEC44F",  # Jaune orangé
  "Union des Démocrates et Indépendants" = "#92C5DE",  # Bleu ciel
  "Les Républicains"        = "#4575B4",  # Bleu moyen
  "Divers Droite"           = "#74ADD1",  # Bleu clair
  "Régionalistes"           = "#7F3B08",  # Brun foncé
  "Divers"                  = "#BEBEBE",  # Gris moyen
  "Divers Souverainistes"   = "#D9D9D9",  # Gris clair
  "Union de la Droite"      = "#313695",  # Bleu foncé
  "Horizons"                = "#542788",  # Violet
  "Reconquête"              = "#2D004B",  # Violet très foncé
  "Extrême Droite"          = "#081D58",  # Bleu nuit profond
  "Rassemblement National"  = "#041E42"   # Bleu marine presque noir
)

# Ordre des partis de l'extrême gauche à l'extrême droite
ordre_partis <- c(
  "Extrême Gauche", "France Insoumise", "Union de la Gauche", "Parti Socialiste", "Écologistes",
  "Divers Gauche", "Ensemble", "Divers Centre", "Union des Démocrates et Indépendants",
  "Les Républicains", "Divers Droite", "Régionalistes", "Divers",
  "Divers Souverainistes", "Union de la Droite", "Horizons", "Reconquête",
  "Extrême Droite", "Rassemblement National"
)


# Génération du contenu des popups
legislatives$popup_content <- apply(legislatives, 1, function(row) {
  # En-tête du tableau
  lignes <- sprintf("<strong>%s</strong><br>
                    <table style='border-collapse: collapse; width: 100%%;'>
                    <tr style='background-color: #333; color: white;'>
                      <th>Prénom</th><th>Nom</th><th>Parti</th>
                    </tr>", row["Libellé"])
  
  # Filtrage des candidats valides
  candidats <- lapply(1:19, function(i) {
    nom <- row[[paste("Nom candidat", i)]]
    prenom <- row[[paste("Prénom candidat", i)]]
    nuance <- row[[paste("Nuance candidat", i)]]
    
    if (!is.na(nom) && !is.na(prenom) && !is.na(nuance) && nom != "" && prenom != "" && nuance != "") {
      couleur <- if (nuance %in% names(couleurs)) couleurs[[nuance]] else "#FFFFFF"  # Assurer une couleur de fond même sans correspondance
      return(list(prenom = prenom, nom = nom, nuance = nuance, couleur = couleur))
    } else {
      return(NULL)  # Ignore les candidats incomplets
    }
  })
  
  # Suppression des NULL (candidats invalides)
  candidats <- Filter(Negate(is.null), candidats)
  
  # Tri des candidats selon l'ordre des partis
  if (length(candidats) > 0) {
    candidats_sorted <- candidats[order(match(sapply(candidats, function(x) x$nuance), ordre_partis))]
    
    # Génération des lignes du tableau avec couleurs
    lignes_candidats <- sapply(candidats_sorted, function(candidat) {
      sprintf("<tr style='background-color: %s; color: white;'>
                <td>%s</td><td>%s</td><td>%s</td></tr>", 
              candidat$couleur, candidat$prenom, candidat$nom, candidat$nuance)
    })
    
    # Fusionner les lignes des candidats dans le tableau
    popup_text <- paste0(lignes, paste(lignes_candidats, collapse = ""), "</table>")
  } else {
    popup_text <- paste0(lignes, "<tr><td colspan='3' style='text-align:center;'>Aucun candidat</td></tr></table>")
  }
  
  HTML(popup_text)
})


# Conversion des données géospatiales en format sf compatible avec leaflet
legislatives_sf <- st_as_sf(legislatives)

# Création de la carte avec leaflet
leaflet(legislatives_sf) |>
  addTiles() |>
  addPolygons(
    popup = legislatives_sf$popup_content,
    label = ~Libellé,  # Affiche le nom de la circonscription au survol
    labelOptions = labelOptions(
      direction = "auto",  # La direction du label
      style = list(
        "font-weight" = "bold",
        "color" = "black",
        "background" = "white",
        "padding" = "5px"
      )
    ),
    highlight = highlightOptions(
      weight = 5,
      color = "#666",  # Color of the border when highlighted
      fillOpacity = 0.7,  # Opacity of the fill on hover
      bringToFront = TRUE  # Bring the polygon to front when hovered
    )
  )

## Autres circonscriptions

legislatives_empty <- legislatives_empty %>%
  rowwise() %>%
  mutate(
    candidats_info = list(
      tibble(
        Prénom = c(`Prénom candidat 1`, `Prénom candidat 2`, `Prénom candidat 3`, 
                   `Prénom candidat 4`, `Prénom candidat 5`, `Prénom candidat 6`, 
                   `Prénom candidat 7`, `Prénom candidat 8`, `Prénom candidat 9`, 
                   `Prénom candidat 10`, `Prénom candidat 11`, `Prénom candidat 12`, 
                   `Prénom candidat 13`, `Prénom candidat 14`, `Prénom candidat 15`, 
                   `Prénom candidat 16`, `Prénom candidat 17`, `Prénom candidat 18`, 
                   `Prénom candidat 19`),
        Nom = c(`Nom candidat 1`, `Nom candidat 2`, `Nom candidat 3`, 
                `Nom candidat 4`, `Nom candidat 5`, `Nom candidat 6`, 
                `Nom candidat 7`, `Nom candidat 8`, `Nom candidat 9`, 
                `Nom candidat 10`, `Nom candidat 11`, `Nom candidat 12`, 
                `Nom candidat 13`, `Nom candidat 14`, `Nom candidat 15`, 
                `Nom candidat 16`, `Nom candidat 17`, `Nom candidat 18`, 
                `Nom candidat 19`),
        Parti = c(`Nuance candidat 1`, `Nuance candidat 2`, `Nuance candidat 3`, 
                  `Nuance candidat 4`, `Nuance candidat 5`, `Nuance candidat 6`, 
                  `Nuance candidat 7`, `Nuance candidat 8`, `Nuance candidat 9`, 
                  `Nuance candidat 10`, `Nuance candidat 11`, `Nuance candidat 12`, 
                  `Nuance candidat 13`, `Nuance candidat 14`, `Nuance candidat 15`, 
                  `Nuance candidat 16`, `Nuance candidat 17`, `Nuance candidat 18`, 
                  `Nuance candidat 19`)
      )
    )
  ) %>%
  ungroup()




# ----- ONGLET 3 : PARTIS POLITIQUES -----










# ----- ONGLET 4 : COMMENT VOTER ? -----










# ----- ONGLET 5 : RESULTATS - Juliette -----

library(ggplot2)
library(ggiraph)

# Fonction pour créer le graphique
create_assemblee_graph <- function() {
  # Création du DataFrame des résultats de l'Assemblée
  resultats_assemblee <- data.frame(
    Parti = c("Nouveau Front Populaire", "Gauche", "Ensemble", "Centre", "Régionalistes", 
              "Les Républicains", "Droite", "Rassemblement National", "Divers"),
    Sieges = c(182, 13, 168, 6, 4, 46, 14, 143, 1),  # Nombre de sièges par parti
    Couleur = c("firebrick4", "firebrick1", "gold", "gold3", "lightgoldenrod4", 
                "dodgerblue", "dodgerblue3", "navy", "gray47") # Couleurs associées
  )
  
  # Nombre total de sièges et de rangées
  nb_sieges <- sum(resultats_assemblee$Sieges) # 577
  nb_rangs <- 14  # Nombre de rangées dans le demi-cercle
  
  # Définition du nombre de sièges par rangée (moins en bas, plus en haut)
  sieges_par_rang <- round(seq(20, 60, length.out = nb_rangs)) 
  sieges_par_rang[length(sieges_par_rang)] <- sieges_par_rang[length(sieges_par_rang)] + 
    (577 - sum(sieges_par_rang)) # Ajustement final
  
  # Génération des coordonnées pour chaque siège
  df_sieges <- data.frame()
  index <- 1
  rayon_depart <- 8  # Rayon initial pour laisser un espace vide au centre
  
  for (rang in 1:nb_rangs) {
    nb_points <- sieges_par_rang[rang]  
    angles <- seq(pi, 0, length.out = nb_points)  # Symétrie parfaite autour de l'axe vertical
    rayon <- rayon_depart + rang * 1.5  # Les rangées s'éloignent progressivement du centre
    
    # Calcul des positions x, y
    temp_df <- data.frame(
      Siege = index:(index + nb_points - 1),
      x = rayon * cos(angles),
      y = rayon * sin(angles),
      Rang = rang
    )
    
    df_sieges <- rbind(df_sieges, temp_df)
    index <- index + nb_points
  }
  
  # Trier les points de gauche à droite
  df_sieges <- df_sieges[order(df_sieges$x), ]  
  
  # Attribuer les partis dans cet ordre
  df_sieges$Parti <- rep(resultats_assemblee$Parti, resultats_assemblee$Sieges)
  df_sieges <- df_sieges %>%
    left_join(resultats_assemblee, by = "Parti")  # Jointure pour obtenir les couleurs
  
  # Création de la colonne pour les tooltips interactifs (seulement le nom du parti, pas le numéro de siège)
  df_sieges$tooltip <- df_sieges$Parti
  
  # Création du graphique interactif
  gg <- ggplot(df_sieges, aes(x, y, color = Parti, tooltip = tooltip, data_id = Parti)) +
    geom_point_interactive(size = 3, shape = 16, alpha = 1) +  # Points interactifs
    scale_color_manual(values = setNames(resultats_assemblee$Couleur, resultats_assemblee$Parti)) +
    coord_fixed() +  # Conserver l'aspect circulaire
    theme_void() +  # Supprimer les axes
    ggtitle("Répartition des sièges à l'Assemblée Nationale") +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),  # Centrer et mettre en gras le titre
      plot.margin = margin(1, 1, 1, 1)  # Ajouter un peu de marge pour éviter que l'annotation touche le graphique
    ) +  
    annotate("text", x = 0, y = -3, label = "Source : Le Monde (2024)", size = 3, hjust = 0.5) +  # Réduire la taille de l'annotation
    labs(color = "Tendance politique")  # Changer le titre de la légende
  
  # Rendu interactif avec ggiraph, sans effet de sélection
  girafe(ggobj = gg, options = list(
    opts_hover(css = "opacity: 1;"),  # Garde la couleur normale au survol
    opts_hover_inv(css = "opacity: 0.2;"),  # Reste des points deviennent plus transparents
    opts_selection(type = "none")  # Désactive le changement de couleur au clic
  ))
  
}

#----- APP PRINCIPALE -----

library(shiny)
library(shinythemes)
library(leaflet)
library(DT)
library(stringi)
library(dplyr)
library(tidyr)

ui <- fluidPage(
  theme = shinytheme("readable"),
  fluidRow(
    tabsetPanel(
      tabPanel("Informations générales"),
      tabPanel("Carte des circonscriptions",
               selectizeInput("search_input", "Rechercher une ville", choices = NULL, selected = "", multiple = FALSE),
               actionButton("search_btn", "Chercher"),
               leafletOutput("ma_carte", height = "500px"),
               selectizeInput("search_input2", "Rechercher une autre ville", choices = NULL, selected = "", multiple = FALSE),
               actionButton("show_missing_btn", "Chercher"),
               h3(textOutput("table_title")),
               dataTableOutput("circonscriptions_table")
      ),
      tabPanel("Partis politiques"),
      tabPanel("Comment voter ?"),
      tabPanel("Résultats", fluidRow(
        # Titre de la page
        column(12, h2("Résultats des élections législatives de 2024")),
        
        # Texte explicatif avec moins de marge
        column(12, p("Les élections législatives sont terminées et vous souhaitez connaître les résultats ? Vous êtes au bon endroit ! On vous présente la répartition des 577 sièges dans l'hémicycle de l'Assemblée Nationale avec le programme des principaux partis présents. Vous souhaitez également savoir qui a été élu proche de chez vous ? Pas de soucis ! Vous trouverez sur cette page une carte intéractive avec l’élu de votre circonscription.")),
        
        # Réduire l'espace entre le texte et le graphique
        column(12, girafeOutput("assemblee_graph", height = "600px"))
      ),
    ),
    tags$head(
      tags$style(HTML("
    .leaflet-control-zoom {
      position: absolute !important;
      bottom: 10px;
      left: 10px;
    }
    /* Changer la couleur de la police en blanc pour le tableau */
    .dataTable, .dataTable th, .dataTable td {
      color: white !important;
    }
  "))
    )
  )
  )
)


server <- function(input, output, session) {
  
  # Mise à jour de la liste des villes
  observe({
    updateSelectizeInput(session, "search_input", choices = villes$`Libellé commune`, selected = "", server = TRUE)
  })
  
  # Fonction pour afficher la carte
  output$ma_carte <- renderLeaflet({
    leaflet(legislatives_sf) %>%
      addTiles() %>%
      addPolygons(
        popup = ~popup_content,
        label = ~Libellé,
        highlight = highlightOptions(weight = 5, color = "#666", fillOpacity = 0.7)
      )
  })
  
  # Recherche et zoom sur la ville
  observeEvent(input$search_btn, {
    req(input$search_input)
    
    recherche <- tolower(stri_trans_general(input$search_input, "Latin-ASCII"))
    ville_trouvee <- villes %>%
      mutate(label_clean = tolower(stri_trans_general(`Libellé commune`, "Latin-ASCII"))) %>%
      filter(label_clean == recherche)
    
    if (nrow(ville_trouvee) > 0) {
      leafletProxy("ma_carte") %>%
        setView(lng = ville_trouvee$Longitude[1], lat = ville_trouvee$Latitude[1], zoom = 12)
    } else {
      showNotification("Ville non trouvée.", type = "error")
    }
  })
  
  # Mise à jour de la liste des villes pour le second champ
  observe({
    updateSelectizeInput(session, "search_input2", choices = legislatives_empty$`Libellé commune`, selected = "", server = TRUE)
  })
  
  # Transformation des données pour le tableau
  transform_legislatives <- function(df) {
    df_long <- df %>%
      pivot_longer(cols = starts_with("Prénom candidat"), names_to = "index", values_to = "Prénom") %>%
      mutate(
        index_num = as.integer(sub("Prénom candidat ", "", index)),
        Nom = sapply(index_num, function(i) df[[paste0("Nom candidat ", i)]]),
        Parti = sapply(index_num, function(i) df[[paste0("Nuance candidat ", i)]])
      ) %>%
      select(Prénom, Nom, Parti) %>%
      filter(!is.na(Prénom) & !is.na(Nom) & !is.na(Parti)) %>%
      arrange(match(Parti, ordre_partis))  # Tri en fonction de l'ordre des partis
    
    return(df_long)
  }
  
  # Affichage du tableau des candidats
  observeEvent(input$show_missing_btn, {
    req(input$search_input2)
    
    data_filtered <- legislatives_empty %>%
      filter(`Libellé commune` == input$search_input2)
    
    if (nrow(data_filtered) == 0) {
      showNotification("Aucune donnée trouvée pour cette circonscription.", type = "error")
      return()
    }
    
    table_data <- transform_legislatives(data_filtered)
    
    output$table_title <- renderText({ unique(data_filtered$`Libellé`) })
    
    # Ajout de la couleur de fond dans la table sans ajouter une colonne
    output$circonscriptions_table <- renderDataTable({
      datatable(table_data, options = list(pageLength = 10), 
                escape = FALSE,  # Permet d'afficher du HTML
                rownames = FALSE) %>%
        formatStyle(
          columns = c("Prénom", "Nom", "Parti"),
          target = "row",
          backgroundColor = styleEqual(names(couleurs), couleurs)  # Applique la couleur de fond selon le parti
        )
    })
  })
  {
    # Rendre le graphique interactif
    output$assemblee_graph <- renderGirafe({
      create_assemblee_graph()  # Appeler la fonction pour créer le graphique
    })
  }
}

shinyApp(ui = ui, server = server)

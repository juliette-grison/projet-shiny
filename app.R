# ----- PACKAGES -----

library(shiny)
library(sf)
library(tidyverse)
library(leaflet)
library(htmltools)
library(stringi)
library(DT)
library(shinyjs)





# ----- IMPORTS -----

## Circonscriptions, candidats et résultats 1er et 2nd tour (candidats 20 à 23)
legislatives <- read_rds("data/legislatives.rds")

## Circonscriptions non affichées sur la carte et résultats 1er tour
legislatives_empty <- read_rds("data/legislatives_empty.rds")

## Villes et coordonnées géographiques
villes <- read_rds("data/villes.rds")

## Circonscriptions non affichées sur la carte et résultats 1er et 2nd tour (candidats 20 à 23)
legislatives_empty2 <- read_rds("data/legislatives_empty2.rds")





# ----- ONGLET 1 : INFORMATIONS GENERALES -----

intro_onglet1 <- "
Les élections législatives approchent à grand pas et vous n’y connaissez rien en politique ou souhaitez approfondir votre savoir élémentaire sur leur fonctionnement ? 
Ce site est fait pour vous ! Que ce soit la première fois que vous allez voter, ou même si vous êtes un habitué des urnes, ici, on vous explique tout ! 
Vous trouverez une brève présentation concernant l’utilité des élections législatives et de l’Assemblée Nationale, leur utilité, leur fonctionnement, et leur enjeux à l’échelle du pays. 
On vous offre également toutes les ressources nécessaires pour voter pour la première fois (ou pas), afin que votre expérience pour ces élections se fasse en toute sérénité : où voter, quoi apporter, comment voter de l’étranger, etc. Bonne navigation sur notre site !
<br>"

partie1_onglet1 <- "<br>
<p>Les élections législatives servent à élire les députés. Les députés siègent à l'Assemblée nationale. Ils sont élus au suffrage universel direct par les électeurs français inscrits sur les listes électorales. Le mode de scrutin est un scrutin majoritaire à 2 tours.</p>
<p>La durée du mandat des députés est de 5 ans (sauf dissolution de l'Assemblée nationale).
Le 9 juin 2024, le chef de l'Etat a annoncé la dissolution de l'Assemblée nationale.
Les dernières élections législatives ont eu lieu les 29 et 30 juin 2024 pour le 1er tour, et les 6 et 7 juillet 2024 pour le 2d tour.</p>
<p>Sauf en cas de dissolution, les élections législatives sont organisées :</p>
<p> - dans les 70 jours avant la fin du mandat de l'Assemblée nationale précédemment élue </p>
<p> - et le 7e dimanche qui suit la publication du décret convoquant les électeurs. Mais les élections sont organisées le samedi en Guadeloupe, en Guyane et en Martinique. </p>
<br>"

partie2_onglet1 <- "<br>
<p> Le suffrage universel est un principe fondamental de la République. À l’origine, lors de la Révolution française (1789 et 1792), le vote se faisait en plusieurs étapes, mais c’est finalement la IIe République qui, en 1848, instaure le suffrage universel masculin pour tous les hommes âgés d’au moins 21 ans. La IIIe République le rend définitif. </p>
<p> Cependant, tout au long du XIXe siècle, le droit de vote connaît des restrictions, souvent liées à des critères comme le niveau d’impôt payé (en 1814, 1830, 1849, 1852). En 1879, une large victoire des républicains marque un tournant en ancrant durablement le suffrage universel dans le régime politique.</p>
<p> Mais ce système reste longtemps imparfait : les femmes, par exemple, n’obtiennent le droit de vote qu’en 1944 (et peuvent voter aux municipales dès 1945). De plus, le mode de scrutin fait l’objet de nombreux débats : faut-il un ou deux tours ? Un vote par liste ou individuel ? Une élection à la majorité absolue ou à la proportionnelle ? Différents systèmes sont testés au fil du temps, souvent sous l’influence des partis politiques et de leurs stratégies électorales.</p>
<p>Les grands partis, mieux organisés, prennent l’avantage sur les petites formations politiques. La démocratie, en imposant des compromis et des alliances entre partis (comme les coalitions, unions ou cartels), modifie la manière dont les élections sont jouées et gagnées.</p>
<p>La Constitution mise en place par le général de Gaulle avait pour but de donner plus de pouvoir au président et au gouvernement, tout en réduisant l’influence du Parlement. Cependant, depuis le début de la Ve République, le président, élu directement par les citoyens, a parfois partagé son pouvoir avec le Premier ministre, notamment en période de cohabitation (lorsque le président et la majorité parlementaire sont de bords politiques opposés).</p>
<p> Un élément clé du système politique français est le fait qu'une majorité claire à l'Assemblée nationale renforce la stabilité du gouvernement. En 1958, Michel Debré, un des principaux architectes de la Constitution, espérait justement qu'une majorité solide puisse émerger après chaque élection.</p>
<p>Ironiquement, alors que la Ve République voulait initialement limiter l’influence des partis politiques, elle a finalement renforcé leur rôle au fil des années. Aujourd’hui, même si le président a seul le pouvoir de nommer le Premier ministre, ce choix dépend toujours du parti ou de la coalition majoritaire à l’Assemblée nationale. Ce fonctionnement ressemble à celui des régimes parlementaires, où le chef du gouvernement est issu du parti vainqueur des élections.</p>
<br>"

partie3_onglet1 <- "<br>
<p>Les députés sont élus au suffrage universel direct, au scrutin majoritaire à 2 tours et par circonscriptions.</p>
<ul style=\"list-style-type:square;\">
  <li><p>1er tour :</p>
<p>Pour être élu au 1er tour, un candidat doit obtenir :</p>
<p>- Plus de 50 % des suffrages exprimés</p>
<p>- Et un nombre de voix au moins égal à 25 % du nombre des électeurs inscrits.</p>
<p>Si aucun candidat n'est élu dès le 1er tour, un 2d tour est organisé une semaine plus tard.
</p></li>
<br>
  <li><p>2d tour :</p>
<p>Seuls certains candidats peuvent se présenter au 2d tour :</p>
<p>- Les 2 candidats qui sont arrivés en tête</p>
<p>- Les candidats suivants, à condition d'avoir obtenu un nombre de voix au moins égal à 12,5 % du nombre des électeurs inscrits.</p>
<p>Au 2d tour, le candidat qui obtient le plus grand nombre de voix est élu. En cas d'égalité, le plus âgé des candidats est élu.</li>
</ul>
<br>

<ul><li><p><B>Qui peut voter ?</B></p>
</p>Le scrutin législatif est ouvert à tous les électeurs des scrutins nationaux inscrits sur les listes électorales, c’est-à-dire à toute personne :</p>
<p>- âgée de 18 ans au moins à la date du premier tour</p>
<p>- n'étant pas déchue de ses droits civiques</p>
<p>- de nationalité française</p>
<p>- jouissant de ses droits civils et politiques</p>
<p>- inscrite sur une liste électorale.</p></li>
<br>
<p><B><li>Comment sont réparties les circonscriptions ?</B></p>
<p>Depuis 2012, la France est découpée en 577 circonscriptions législatives, chacune élisant un député à l'Assemblée nationale. Les circonscriptions sont principalement formées de cantons, qui regroupent plusieurs communes ou, parfois, des parties d’une grande commune. 
Cependant, certaines circonscriptions ne respectent pas le seuil moyen de 120 000 habitants par député, notamment dans les territoires d'outre-mer et certaines collectivités spécifiques comme les Terres australes et antarctiques françaises (TAAF), en raison de leur faible population.
Il y a en tout 577 circonscriptions réparties de la sorte :</p>
<p>- <B>France métropolitaine </B> : 539</p>
<p>- <B>Départements d’outre-mer (DOM) </B> : 19</p>
<p>- <B>Collectivités d’outre-mer (COM) </B> : 8</p>
<p>- <B>Français établis hors de France </B> : 11</p>
<p>Pour des raisons logistiques, les élections se déroulent en plusieurs étapes : d'abord pour les Français de l'étranger, puis pour les territoires d’outre-mer, et enfin en métropole.</p></li></ul>
<br>"

partie4_onglet1 <- "<br>
<p>Le pouvoir législatif exercé par l’Assemblée nationale joue un rôle essentiel dans l’équilibre politique du pays. Tantôt un soutien direct au gouvernement élu lors des élections présidentielles, tantôt un contre-pouvoir, il requiert plus que jamais une participation citoyenne active. 
Bien que l’Assemblée serve généralement à constituer une majorité pour le président ou la présidente fraîchement élu(e), elle peut également entraîner des périodes de cohabitation et permettre aux partis d’opposition d’exercer une influence sur la représentation politique durant les cinq années du mandat.</p>
<br>
<ol>
<li><p><B>Un rôle législatif fondamental</B></p>
Votre député(e) a pour mission de proposer, encadrer et examiner les lois. Il ou elle vous représente en votant pour ou contre les textes soumis à l’Assemblée Nationale.</p></li>

<li><p><B>Un relais de la voix citoyenne</B></p>
Élu(e) national, votre député(e) est un porte-parole qui peut interpeller directement les membres du gouvernement sur des sujets qui préoccupent les citoyens. Son rôle est de défendre les intérêts de l’ensemble de la population.</p></li>

<li><p><B>Un moyen d’exprimer une opposition au gouvernement</B></p>
Si le gouvernement propose des mesures controversées, l’Assemblée nationale peut, avec le soutien de plusieurs députés, voter une motion de censure pour s’y opposer et ainsi bloquer certaines décisions.</p></li>

<li><p><B>Un représentant au service des citoyens</B></p>
La fonction principale d’un député est de représenter et défendre les intérêts des citoyens. Afin de garantir son indépendance, il bénéficie d’une indemnité mensuelle de plus de 5 500 euros net. De plus, pour éviter tout conflit d'intérêt, un député ne peut cumuler son mandat avec certaines autres fonctions, comme celle de maire.</p></li></ol>
<br>"





# ----- ONGLET 2 : CARTE DES CIRCONSCRIPTIONS - Florian -----

# Couleurs par parti
couleurs <- c(
  "Extrême Gauche"                     = "#A50026",  # Rouge très foncé  
  "France Insoumise"                    = "#D1191B",  # Rouge foncé  
  "Union de la Gauche"                  = "#D73027",  # Rouge vif  
  "Parti Socialiste"                    = "#F46D43",  # Rouge orangé  
  "Écologistes"                         = "#1A9850",  # Vert soutenu  
  "Divers Gauche"                       = "#FDAE61",  # Orange clair  
  "Ensemble"                            = "#FFD700",  # Jaune doré  
  "Divers Centre"                        = "#FEC44F",  # Jaune orangé  
  "Union des Démocrates et Indépendants" = "#92C5DE",  # Bleu ciel  
  "Les Républicains"                     = "#4575B4",  # Bleu moyen  
  "Divers Droite"                        = "#74ADD1",  # Bleu clair  
  "Régionalistes"                        = "#7F3B08",  # Brun foncé  
  "Divers"                               = "#BEBEBE",  # Gris moyen  
  "Divers Souverainistes"                = "#D9D9D9",  # Gris clair  
  "Union de la Droite"                   = "#313695",  # Bleu foncé  
  "Horizons"                             = "#542788",  # Violet  
  "Reconquête"                           = "#2D004B",  # Violet très foncé  
  "Extrême Droite"                       = "#081D58",  # Bleu nuit profond  
  "Rassemblement National"               = "#041E42"   # Bleu marine presque noir  
)

# Ordre des partis de l'extrême gauche à l'extrême droite
ordre_partis <- c(
  "Extrême Gauche", "France Insoumise", "Union de la Gauche", "Parti Socialiste", "Écologistes",
  "Divers Gauche", "Ensemble", "Divers Centre", 
  "Union des Démocrates et Indépendants", "Les Républicains", "Divers Droite", 
  "Régionalistes", "Divers", "Divers Souverainistes", "Union de la Droite", 
  "Horizons", "Reconquête", "Extrême Droite", "Rassemblement National"
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





# ----- ONGLET 3 : PARTIS POLITIQUES -----

tetes_de_liste <- c(
  "Extrême Gauche" = "Manon Aubry",
  "France Insoumise" = "Manon Aubry",
  "Union de la Gauche" = "Léon Deffontaines",
  "Parti Socialiste" = "Raphaël Glucksmann",
  "Écologistes" = "Marie Toussaint",
  "Divers Gauche" = "Léon Deffontaines",
  "Ensemble" = "Valérie Hayer",
  "Divers Centre" = "Valérie Hayer",
  "Union des Démocrates et Indépendants" = "Valérie Hayer",
  "Les Républicains" = "François-Xavier Bellamy",
  "Divers Droite" = "François-Xavier Bellamy",
  "Régionalistes" = "Léon Deffontaines",
  "Divers" = "Léon Deffontaines",
  "Divers Souverainistes" = "Léon Deffontaines",
  "Union de la Droite" = "François-Xavier Bellamy",
  "Horizons" = "Valérie Hayer",
  "Reconquête" = "Marion Maréchal",
  "Extrême Droite" = "Jordan Bardella",
  "Rassemblement National" = "Jordan Bardella"
)



programmes_partis <- c(
  "Extrême Gauche" = "Le programme de l'Extrême Gauche prône une transformation radicale de la société. Leur objectif est d'abolir le capitalisme et d'instaurer un système économique basé sur la planification, la collectivisation et la redistribution des richesses. Ils proposent une nationalisation de secteurs stratégiques tels que l'énergie, les transports et les banques. Sur le plan social, ils défendent une augmentation des salaires, la réduction du temps de travail, la protection des droits des travailleurs, et une sécurité sociale renforcée. En matière écologique, l'Extrême Gauche propose une transition énergétique rapide vers des énergies renouvelables, une réduction drastique des émissions de gaz à effet de serre et une transformation des modes de production pour limiter les inégalités environnementales.",
  "France Insoumise" = "La France Insoumise propose une 'Sixième République' avec une refonte des institutions pour renforcer la démocratie directe et la participation citoyenne. Leur programme économique repose sur une économie verte et solidaire, avec un renforcement du service public, des investissements dans les énergies renouvelables, et une relocalisation de la production. Ils plaident pour la mise en place d'une taxe sur les grandes fortunes, l'augmentation des salaires, la réduction du temps de travail, et l'instauration d'un revenu universel. La France Insoumise propose aussi de sortir des traités européens actuels et de construire une autre Europe, plus sociale et solidaire, ainsi qu'une politique étrangère plus indépendante et de solidarité internationale.",
  "Union de la Gauche" = "L'Union de la Gauche vise une politique de justice sociale, avec des réformes fiscales pour redistribuer les richesses, un renforcement des services publics, et une transition énergétique verte. Ils soutiennent la création d'une 'banque publique d'investissement' pour financer des projets industriels et sociaux, notamment dans les secteurs de la santé, de l'éducation et de l'écologie. Sur le plan écologique, ils proposent une sortie progressive du nucléaire, une transition vers les énergies renouvelables, et la mise en place d'un 'Green New Deal' européen. En matière de droits sociaux, ils soutiennent l'augmentation du salaire minimum et une réforme des retraites pour assurer une retraite à taux plein pour tous.",
  "Parti Socialiste" = "Le Parti Socialiste se concentre sur une politique de croissance économique inclusive, en soutenant les secteurs productifs tout en réduisant les inégalités sociales. Ils proposent une réforme fiscale avec une hausse de l'impôt sur les grandes fortunes et une réduction de la fiscalité sur les classes moyennes. Leur programme inclut des investissements dans la transition énergétique, la décarbonation des secteurs industriels et la mise en place d'un plan de relance verte. Sur le plan social, ils soutiennent la création de nouveaux services publics dans les secteurs de la santé et de l'éducation, une réforme du marché du travail pour une meilleure protection des travailleurs, et une lutte contre la pauvreté et l'exclusion sociale.",
  "Écologistes" = "Les Écologistes mettent l'accent sur la transition écologique pour lutter contre le dérèglement climatique, la pollution et la destruction des écosystèmes. Ils prônent un modèle économique basé sur la durabilité et l'économie circulaire, avec des investissements massifs dans les énergies renouvelables, les transports écologiques et l'agriculture bio. Leur programme inclut une sortie rapide du nucléaire et la fermeture des centrales à charbon, ainsi qu'une politique de zéro déchet, un système alimentaire local et durable, et une politique de mobilité verte. En matière sociale, ils soutiennent l'accès universel à la santé, à l'éducation et au logement, et une redistribution des richesses pour réduire les inégalités.",
  "Divers Gauche" = "Les partis de la Divers Gauche plaident pour une répartition plus juste des ressources et un renforcement des services publics. Leur programme inclut une réforme fiscale pour lutter contre l'évasion fiscale et augmenter les dépenses publiques dans des secteurs comme l'éducation, la santé et les transports publics. Sur le plan écologique, ils soutiennent une transition énergétique rapide, une économie de la décroissance, la protection des écosystèmes et la lutte contre les inégalités environnementales. Ils sont aussi favorables à une démocratie plus directe et plus participative, avec un renforcement du rôle des citoyens dans les décisions politiques.",
  "Ensemble" = "Ensemble est un mouvement progressiste qui défend une politique d'union de la gauche pour lutter contre les inégalités économiques et sociales. Leur programme repose sur une politique sociale ambitieuse, avec l'instauration d'un revenu de base universel, l'augmentation du salaire minimum, et des investissements dans la transition énergétique. Ils soutiennent également la mise en place d'une fiscalité plus progressive et une réforme de l'impôt sur les sociétés pour mieux répartir les richesses. En matière d'environnement, Ensemble prône une décarbonation rapide de l'économie et un soutien aux industries écologiques et locales.",
  "Divers Centre" = "Les partis du Divers Centre prônent une politique économique libérale et modérée. Leur programme inclut une réduction des dépenses publiques, une baisse des impôts pour les entreprises et les classes moyennes, et la réduction des déficits publics. En matière sociale, ils soutiennent des réformes pour moderniser les services publics, tout en préservant un état-providence pour les plus vulnérables. En matière de politique étrangère, ils défendent une Europe plus compétitive, la relance de la croissance économique et un renforcement des partenariats commerciaux internationaux.",
  "Union des Démocrates et Indépendants" = "L'Union des Démocrates et Indépendants se positionne sur une ligne pro-européenne et libérale. Leur programme inclut la réduction des impôts, la flexibilisation du marché du travail, et des réformes pour favoriser l'innovation et la compétitivité des entreprises. Ils soutiennent la décentralisation des pouvoirs et la mise en place d'une gouvernance locale plus forte. Ils souhaitent également une réforme de l'Union Européenne pour la rendre plus efficace et moins bureaucratique. En matière sociale, ils promeuvent la réduction des dépenses publiques et un système de protection sociale plus ciblé.",
  "Les Républicains" = "Les Républicains défendent une politique de droite traditionnelle, centrée sur la réduction des impôts, la réduction des dépenses publiques, et un renforcement de la sécurité intérieure. Ils soutiennent une politique de fermeté sur l'immigration et la lutte contre le terrorisme, ainsi qu'une modernisation du code du travail pour plus de flexibilité. Leur programme inclut également des réformes pour soutenir les entreprises et l'entrepreneuriat, avec un accent sur la compétitivité de la France sur la scène internationale. Ils plaident pour une Europe plus forte mais moins intrusive.",
  "Divers Droite" = "Les partis de la Divers Droite proposent une politique plus conservatrice et économique, avec un programme centré sur la réduction de l'impôt, la réforme du marché du travail, et la défense de l'ordre public. Ils sont en faveur de la réduction des aides sociales et du contrôle renforcé de l'immigration. Ils soutiennent également une Europe plus ouverte au libre-échange et une politique de défense nationale plus indépendante.",
  "Régionalistes" = "Les Régionalistes prônent une politique de décentralisation accrue et un renforcement de l'autonomie des régions. Leur programme inclut une gestion des ressources locales, une répartition plus équitable des richesses entre les régions, et un soutien aux cultures et aux langues régionales. Ils souhaitent également un renforcement des infrastructures régionales et une plus grande prise en compte des spécificités locales dans les décisions politiques nationales.",
  "Divers" = "Les partis Divers se caractérisent par une pluralité de positions politiques, allant de la défense des droits des minorités à la promotion de politiques inclusives et progressistes. Leur programme commun inclut la lutte contre les discriminations, une réforme des institutions pour garantir une plus grande transparence et démocratie, et un soutien aux initiatives locales pour renforcer la cohésion sociale et territoriale.",
  "Divers Souverainistes" = "Les partis Divers Souverainistes défendent la restauration de la souveraineté nationale, en particulier en matière économique et politique. Leur programme inclut un renforcement des frontières, une politique de sécurité stricte, et un retrait des engagements internationaux, notamment vis-à-vis de l'Union Européenne. Ils sont en faveur de la mise en place de politiques protectionnistes pour soutenir l'industrie nationale et la réduction des flux migratoires.",
  "Union de la Droite" = "L'Union de la Droite défend une politique conservatrice et libérale, centrée sur la réduction des impôts, la suppression des réglementations économiques, et un renforcement des valeurs traditionnelles. Leur programme économique repose sur une relance de l'investissement privé, une réforme du marché du travail pour le rendre plus flexible, et une politique d'immigration stricte. Ils souhaitent également une coopération renforcée avec l'Union Européenne tout en préservant la souveraineté nationale.",
  "Horizons" = "Horizons propose une politique centrée sur le pragmatisme et la modernité. Leur programme inclut des réformes pour renforcer la compétitivité économique, une transition énergétique ambitieuse, et un soutien à l'innovation technologique. Ils sont en faveur de la solidarité européenne et souhaitent renforcer la place de la France dans le monde. Ils défendent une politique économique libérale et une révision des institutions françaises pour une plus grande efficacité.",
  "Reconquête" = "Reconquête propose une vision conservatrice de la société, avec un programme centré sur la protection de l'identité nationale, la sécurité, et la famille. Leur programme inclut une politique stricte de contrôle de l'immigration, un renforcement des mesures de sécurité publique, et des réformes pour soutenir les entreprises et l'industrie nationale. Ils sont en faveur d'un protectionnisme économique et d'une politique nationale de préférence.",
  "Extrême Droite" = "L'Extrême Droite soutient une vision nationaliste et autoritaire de la société, avec des priorités sur la sécurité, l'immigration et la souveraineté nationale. Leur programme inclut la fermeture des frontières, la suppression des droits sociaux pour les étrangers, et une politique économique protectionniste pour protéger les industries nationales. Ils prônent également une politique stricte en matière de sécurité et une réduction de l'influence de l'Union Européenne sur la politique française.",
  "Rassemblement National" = "Le Rassemblement National se concentre sur la défense de l'identité nationale, le renforcement de la sécurité intérieure et une politique ferme d'immigration. Leur programme inclut la suppression de l'immigration illégale, une réduction des droits sociaux pour les étrangers, un retour à la souveraineté nationale totale et un contrôle strict des frontières. En matière économique, ils soutiennent un protectionnisme fort, une baisse des impôts, et un soutien aux industries françaises."
)






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

# Carte et tableau concernant le candidat élu

legislatives$popup_content2 <- apply(legislatives, 1, function(row) {
  # En-tête du tableau
  lignes <- sprintf("<strong>%s</strong><br>
                    <table style='border-collapse: collapse; width: 100%%;'>
                    <tr style='background-color: #333; color: white;'>
                      <th>Prénom</th><th>Nom</th><th>Parti</th>
                    </tr>", row["Libellé"])
  
  # Filtrage des candidats élus
  candidats <- lapply(1:23, function(i) {
    nom <- row[[paste("Nom candidat", i)]]
    prenom <- row[[paste("Prénom candidat", i)]]
    nuance <- row[[paste("Nuance candidat", i)]]
    
    # Chercher le candidat élu (où "Elu i" contient "élu")
    if (grepl("élu", row[[paste("Elu", i)]], ignore.case = TRUE)) {
      if (!is.na(nom) && !is.na(prenom) && !is.na(nuance) && nom != "" && prenom != "" && nuance != "") {
        couleur <- if (nuance %in% names(couleurs)) couleurs[[nuance]] else "#FFFFFF"  # Assurer une couleur de fond même sans correspondance
        return(list(prenom = prenom, nom = nom, nuance = nuance, couleur = couleur))
      } else {
        return(NULL)  # Ignore les candidats incomplets
      }
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





#----- APP PRINCIPALE -----

library(shiny)
library(shinythemes)
library(leaflet)
library(DT)
library(stringi)
library(dplyr)
library(tidyr)

ui <- fluidPage(useShinyjs(),
                theme = shinytheme("readable"),
                
                # Onglets
                navbarPage("Les législatives 2024 pour les nuls",
                           
                           # Onglet Informations générales
                           tabPanel("Informations générales",
                                    div(class = "titre-page", h2("Les législatives 2024 POUR LES NULS !")),
                                    div(class = "texte-informations",
                                        p(intro_onglet1)
                                    ),
                                    
                                    tags$div(
                                      img(src = "https://www.savigny-le-temple.fr/Statics/Actus/Actus_2024/06_Juin_2024/300624-elections-legislatives-resultats.jpg", 
                                          class = "image-intro")
                                    ),
                                    
                                    # Bouton déroulant avec flèche pour la première section
                                    div(class = "titre-page",
                                        actionButton("toggle_presentation", "Présentation des élections législatives ⬇", class = "btn btn-primary btn-deroulant")
                                    ),
                                    
                                    # Contenu caché par défaut pour la première section
                                    hidden(
                                      div(id = "texte_presentation", class = "texte-informations",
                                          HTML(partie1_onglet1)
                                      )
                                    ),
                                    
                                    
                                    # Bouton déroulant pour l'historique des élections
                                    div(class = "titre-page",
                                        actionButton("toggle_histoire", "Histoire des élections de l’Assemblée Nationale ⬇", class = "btn btn-primary btn-deroulant")
                                    ),
                                    
                                    # Contenu caché pour la section "Histoire des élections"
                                    hidden(
                                      div(id = "texte_histoire", class = "texte-informations",
                                          HTML(partie2_onglet1)
                                      )
                                    ),
                                    
                                    # Bouton déroulant pour le fonctionnement des élections
                                    div(class = "titre-page",
                                        actionButton("toggle_fonctionnement", "Fonctionnement des élections ⬇", class = "btn btn-primary btn-deroulant")
                                    ),
                                    
                                    # Contenu caché pour la section "Fonctionnement des élections"
                                    hidden(
                                      div(id = "texte_fonctionnement", class = "texte-informations",
                                          HTML(partie3_onglet1)
                                      )
                                    ),
                                    
                                    # Bouton déroulant pour l'importance des élections
                                    div(class = "titre-page",
                                        actionButton("toggle_important", "Pourquoi est-ce important d'aller voter ? ⬇", class = "btn btn-primary btn-deroulant")
                                    ),
                                    
                                    # Contenu caché pour la section "Fonctionnement des élections"
                                    hidden(
                                      div(id = "texte_important", class = "texte-informations",
                                          HTML(partie4_onglet1)
                                      )
                                    )
                           ),
                           
                           # Onglet Carte des circonscriptions
                           tabPanel("Carte des circonscriptions",
                                    h2("Candidats et circonscriptions : faites votre recherche"),
                                    selectizeInput("recherche", "Rechercher une ville", choices = NULL, selected = "", multiple = FALSE),
                                    actionButton("bouton", "Chercher"),
                                    leafletOutput("ma_carte", height = "500px"),
                                    selectizeInput("recherche2", "Ville non listée ? Recherchez-la ici.", choices = NULL, selected = "", multiple = FALSE),
                                    actionButton("bouton2", "Chercher"),
                                    h3(textOutput("titre_tableau")),
                                    dataTableOutput("mon_tableau")
                           ),
                           
                           # Onglet Partis politiques
                           tabPanel("Partis politiques",
                                    h2("Tout savoir sur les partis politiques"),
                                    fluidRow(
                                      # Création des boutons pour chaque parti politique avec couleur, taille agrandie, texte en blanc et gestion du texte sur deux lignes
                                      lapply(names(couleurs), function(parti) {
                                        actionButton(inputId = paste0("parti_", gsub(" ", "_", parti)), 
                                                     label = HTML(paste0(parti, "<br><br>", tetes_de_liste[parti])),  # Deux sauts de ligne entre le nom du parti et la tête de liste
                                                     icon = icon("check-square"), 
                                                     style = paste0("width: 285px; height: 285px; font-size: 20px; font-weight: bold; color: white; margin: 10px; background-color: ", couleurs[parti], 
                                                                    "; border: 2px solid #ddd; border-radius: 10px; white-space: normal; text-align: center;"))
                                      })
                                    )
                           ),
                           
                           # Onglet Comment voter ?
                           tabPanel("Comment voter ?"),
                           
                           # Onglet Résultats
                           tabPanel("Résultats", fluidRow(
                             # Titre de la page
                             column(12, h2("Résultats des élections législatives de 2024")),
                             
                             # Texte explicatif avec moins de marge
                             column(12, p("Les élections législatives sont terminées et vous souhaitez connaître les résultats ? Vous êtes au bon endroit ! On vous présente la répartition des 577 sièges dans l'hémicycle de l'Assemblée Nationale avec le programme des principaux partis présents. Vous souhaitez également savoir qui a été élu proche de chez vous ? Pas de soucis ! Vous trouverez sur cette page une carte intéractive avec l’élu de votre circonscription.")),
                             
                             # Réduire l'espace entre le texte et le graphique
                             column(12, girafeOutput("assemblee_graph", height = "600px")),
                             
                             # Carte et tableau
                             selectizeInput("recherche3", "Rechercher une ville", choices = NULL, selected = "", multiple = FALSE),
                             actionButton("bouton3", "Chercher"),
                             leafletOutput("ma_carte2", height = "500px"),
                             selectizeInput("recherche4", "Ville non listée ? Recherchez-la ici.", choices = NULL, selected = "", multiple = FALSE),
                             actionButton("bouton4", "Chercher"),
                             h3(textOutput("titre_tableau2")),
                             dataTableOutput("mon_tableau2")
                           ))
                ),
                
                # Ajout des styles CSS
                tags$head(
                  tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Fredoka+One&display=swap"),
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
      
      /* Centrer le titre et justifier le texte */
      .titre-page h2 {
    font-family: 'Fredoka One', sans-serif;
    font-size: 50px; /* Augmenter la taille */
    text-align: center; /* Centrer */
    margin-bottom: 20px; /* Ajouter un espace en dessous */
    }
      
      .texte-informations {
        text-align: justify;
        max-width: 90%;
        margin: auto;
      }
      
    .image-intro {
    display: block;
    margin: 20px auto; /* Centre horizontalement et ajoute un peu d'espace */
    width: 50%; /* Ajuste la taille à 80% de la largeur de la page */
    max-width: 1000px; /* Limite la taille max pour éviter une image trop grande */
    }
  
      /* Espacer les boutons déroulants */
      .titre-page .btn {
        margin-bottom: 10px; /* Ajoute un espace entre les boutons */
      }

      /* Modifier le style des boutons */
      .btn.btn-primary {
        background-color: yellow !important; /* Fond jaune */
        color: black !important; /* Texte noir */
        border: 1px solid black !important; /* Bordure noire */
      }
      
      /* Modifier le style des boutons au survol */
      .btn.btn-primary:hover {
        background-color: gold !important; /* Jaune plus foncé au survol */
      }
    "))
                )
)

server <- function(input, output, session) {
  
  # Onglet 1
  
  # Action pour le bouton "Présentation des élections législatives"
  observeEvent(input$toggle_presentation, {
    toggle("texte_presentation")  # Affiche ou cache le contenu de "texte_presentation"
  })
  
  # Action pour le bouton "Histoire des élections de l’Assemblée Nationale"
  observeEvent(input$toggle_histoire, {
    toggle("texte_histoire")  # Affiche ou cache le contenu de "texte_histoire"
  })
  
  # Action pour le bouton "Fonctionnement des élections"
  observeEvent(input$toggle_fonctionnement, {
    toggle("texte_fonctionnement")  # Affiche ou cache le contenu de "texte_fonctionnement"
  })
  
  # Action pour le bouton "Importance des élections"
  observeEvent(input$toggle_important, {
    toggle("texte_important")  # Affiche ou cache le contenu de "texte_important"
  })
  
  
  
  # Onglet 2
  
  # Mise à jour de la liste des villes
  observe({
    updateSelectizeInput(session, "recherche", choices = villes$`Libellé commune`, selected = "", server = TRUE)
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
  observeEvent(input$bouton, {
    req(input$recherche)
    
    recherche <- tolower(stri_trans_general(input$recherche, "Latin-ASCII"))
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
    updateSelectizeInput(session, "recherche2", choices = legislatives_empty$`Libellé commune`, selected = "", server = TRUE)
  })
  
  # Transformation des données pour le tableau
  legislatives_long <- function(df) {
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
  observeEvent(input$bouton2, {
    req(input$recherche2)
    
    data_filtre <- legislatives_empty %>%
      filter(`Libellé commune` == input$recherche2)
    
    if (nrow(data_filtre) == 0) {
      showNotification("Aucune donnée trouvée pour cette circonscription.", type = "error")
      return()
    }
    
    table_data <- legislatives_long(data_filtre)
    
    output$titre_tableau <- renderText({ unique(data_filtre$`Libellé`) })
    
    # Ajout de la couleur de fond dans la table sans ajouter une colonne
    output$mon_tableau <- renderDataTable({
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

  
  # Onglet 3

  observe({
    # On écoute chaque bouton et on ouvre une fenêtre modale quand un bouton est cliqué
    lapply(ordre_partis, function(parti) {
      
      observeEvent(input[[paste0("parti_", gsub(" ", "_", parti))]], {
        
        showModal(modalDialog(
          title = paste("Informations sur le parti :", parti),
          renderUI({
            # Tu peux mettre ici les informations détaillées sur chaque parti
            p(paste("Détails du parti", parti, "ici..."))
          }),
          easyClose = TRUE,
          footer = modalButton("Fermer")
        ))
      })
    })
  })
  
  
  # Onglet 5
  
  # Rendre le graphique interactif
  output$assemblee_graph <- renderGirafe({
    create_assemblee_graph()  # Appeler la fonction pour créer le graphique
  })
  
  
  # Mise à jour de la liste des villes
  observe({
    updateSelectizeInput(session, "recherche3", choices = villes$`Libellé commune`, selected = "", server = TRUE)
  })
  
  # Fonction pour afficher la carte
  output$ma_carte2 <- renderLeaflet({
    leaflet(legislatives_sf) %>%
      addTiles() %>%
      addPolygons(
        popup = ~popup_content2,
        label = ~Libellé,
        highlight = highlightOptions(weight = 5, color = "#666", fillOpacity = 0.7)
      )
  })
  
  # Recherche et zoom sur la ville
  observeEvent(input$bouton3, {
    req(input$recherche3)
    
    recherche <- tolower(stri_trans_general(input$recherche3, "Latin-ASCII"))
    ville_trouvee <- villes %>%
      mutate(label_clean = tolower(stri_trans_general(`Libellé commune`, "Latin-ASCII"))) %>%
      filter(label_clean == recherche)
    
    if (nrow(ville_trouvee) > 0) {
      leafletProxy("ma_carte2") %>%
        setView(lng = ville_trouvee$Longitude[1], lat = ville_trouvee$Latitude[1], zoom = 12)
    } else {
      showNotification("Ville non trouvée.", type = "error")
    }
  })
  
  # Mise à jour de la liste des villes pour le second champ
  observe({
    updateSelectizeInput(session, "recherche4", choices = legislatives_empty2$`Libellé commune`, selected = "", server = TRUE)
  })
  
  # Transformation des données pour le tableau (garder seulement les candidats élus)
  legislatives_long2 <- function(df) {
    df_long <- df %>%
      pivot_longer(cols = starts_with("Prénom candidat"), names_to = "index", values_to = "Prénom") %>%
      mutate(
        index_num = as.integer(sub("Prénom candidat ", "", index)),
        Nom = sapply(index_num, function(i) df[[paste0("Nom candidat ", i)]]),
        Parti = sapply(index_num, function(i) df[[paste0("Nuance candidat ", i)]]),
        Elu = sapply(index_num, function(i) df[[paste0("Elu ", i)]])  # Vérification de la colonne "Elu i"
      ) %>%
      # Garder uniquement les candidats élus
      filter(!is.na(Prénom) & !is.na(Nom) & !is.na(Parti) & grepl("élu", Elu, ignore.case = TRUE)) %>%
      select(Prénom, Nom, Parti) %>%
      arrange(match(Parti, ordre_partis))  # Tri en fonction de l'ordre des partis
    
    return(df_long)
  }
  
  # Affichage du tableau des candidats
  observeEvent(input$bouton4, {
    req(input$recherche4)
    
    data_filtre2 <- legislatives_empty2 %>%
      filter(`Libellé commune` == input$recherche4)
    
    if (nrow(data_filtre2) == 0) {
      showNotification("Aucune donnée trouvée pour cette circonscription.", type = "error")
      return()
    }
    
    table_data <- legislatives_long2(data_filtre2)
    
    output$titre_tableau2 <- renderText({ unique(data_filtre2$`Libellé`) })
    
    # Ajout de la couleur de fond dans la table sans ajouter une colonne
    output$mon_tableau2 <- renderDataTable({
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
  
}
  
shinyApp(ui = ui, server = server)
  
  
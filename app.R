# ============================================================================
# Application Shiny - Analyse BRFSS
# Projet Master 2 Data Science - Visualisation de données
# VERSION FINALE COMPLÈTE
# ============================================================================

# Chargement des bibliothèques
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(grid)
library(gridExtra)
library(scales)

# Packages optionnels
use_shinyWidgets <- requireNamespace("shinyWidgets", quietly = TRUE)
use_spinners      <- requireNamespace("shinycssloaders", quietly = TRUE)

# Options globales
options(shiny.maxRequestSize = 100*1024^2)  # 100 MB max
options(DT.options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE))

# ============================================================================
# GÉOM PERSONNALISÉ AVEC GGPROTO (PARTIE OBLIGATOIRE DU PROJET)
# ============================================================================

# Création d'un géom personnalisé pour afficher des barres avec labels intégrés
StatLabeledBar <- ggproto("StatLabeledBar", Stat,
                          compute_group = function(data, scales) {
                            data$label <- paste0(round(data$y), "\n(", round(data$y / sum(data$y) * 100, 1), "%)")
                            data
                          },
                          required_aes = c("x", "y")
)

GeomLabeledBar <- ggproto("GeomLabeledBar", Geom,
                          required_aes = c("x", "y"),
                          default_aes = aes(
                            colour = "white",
                            fill = "steelblue",
                            size = 3,
                            linetype = 1,
                            alpha = 0.8,
                            label_colour = "white"
                          ),
                          
                          draw_key = draw_key_polygon,
                          
                          draw_panel = function(data, panel_params, coord) {
                            coords <- coord$transform(data, panel_params)
                            
                            # Créer les barres
                            bars <- grid::rectGrob(
                              x = coords$x,
                              y = 0,
                              width = 0.9 / length(unique(coords$x)),
                              height = coords$y,
                              just = c("center", "bottom"),
                              gp = grid::gpar(
                                col = coords$colour,
                                fill = coords$fill,
                                lwd = 2,
                                alpha = coords$alpha
                              )
                            )
                            
                            # Créer les labels
                            labels <- grid::textGrob(
                              label = coords$label,
                              x = coords$x,
                              y = coords$y / 2,
                              gp = grid::gpar(
                                col = coords$label_colour,
                                fontsize = coords$size * .pt,
                                fontface = "bold"
                              )
                            )
                            
                            # Combiner les grobs
                            grid::gTree(children = grid::gList(bars, labels))
                          }
)

# Fonction utilisateur pour le géom personnalisé
geom_labeled_bar <- function(mapping = NULL, data = NULL, stat = "labeled_bar",
                             position = "identity", na.rm = FALSE, show.legend = NA,
                             inherit.aes = TRUE, ...) {
  layer(
    geom = GeomLabeledBar, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

stat_labeled_bar <- function(mapping = NULL, data = NULL, geom = "labeled_bar",
                             position = "identity", na.rm = FALSE, show.legend = NA,
                             inherit.aes = TRUE, ...) {
  layer(
    stat = StatLabeledBar, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# ============================================================================
# CHARGEMENT ET PRÉPARATION DES DONNÉES
# ============================================================================
load_and_prepare_data <- function(file_path, year) {
  tryCatch({
    data <- read.csv(file_path, stringsAsFactors = FALSE)
    
    # Échantillonnage si nécessaire pour performances
    if (nrow(data) > 50000) {
      set.seed(123)
      data <- data[sample(nrow(data), 50000), ]
    }
    
    data %>%
      mutate(
        Year = year,
        GENHLTH   = factor(GENHLTH,   levels = 1:5, labels = c("Excellent", "Très bon", "Bon", "Moyen", "Mauvais")),
        SEX       = factor(SEX,       levels = 1:2, labels = c("Homme", "Femme")),
        X_AGE_G   = factor(X_AGE_G,   levels = 1:6, labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")),
        EDUCA     = factor(EDUCA,     levels = 1:6, labels = c("Jamais scolarisé", "Primaire", "Collège", "Lycée", "Université", "Diplômé")),
        INCOME2   = factor(INCOME2,   levels = 1:8, labels = c("<10k", "10-15k", "15-20k", "20-25k", "25-35k", "35-50k", "50-75k", "75k+")),
        HLTHPLN1  = factor(HLTHPLN1,  levels = 1:2, labels = c("Oui", "Non")),
        EXERANY2  = factor(EXERANY2,  levels = 1:2, labels = c("Oui", "Non")),
        X_SMOKER3 = factor(X_SMOKER3, levels = 1:4, labels = c("Actuel quotidien", "Actuel occasionnel", "Ancien", "Jamais")),
        BMI       = ifelse(is.na(X_BMI5) | X_BMI5 == 0, NA_real_, X_BMI5 / 100),
        BMI_CAT   = cut(BMI, breaks = c(0, 18.5, 25, 30, 100),
                        labels = c("Insuffisant", "Normal", "Surpoids", "Obèse"))
      )
  }, error = function(e) {
    message("Erreur lors du chargement de ", file_path, " → Données simulées utilisées pour ", year)
    data.frame(
      Year      = year,
      GENHLTH   = sample(c("Excellent", "Très bon", "Bon", "Moyen", "Mauvais"), 1000, TRUE),
      SEX       = sample(c("Homme", "Femme"), 1000, TRUE),
      X_AGE_G   = sample(c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"), 1000, TRUE),
      EDUCA     = sample(c("Jamais scolarisé", "Primaire", "Collège", "Lycée", "Université", "Diplômé"), 1000, TRUE),
      INCOME2   = sample(c("<10k", "10-15k", "15-20k", "20-25k", "25-35k", "35-50k", "50-75k", "75k+"), 1000, TRUE),
      HLTHPLN1  = sample(c("Oui", "Non"), 1000, TRUE),
      EXERANY2  = sample(c("Oui", "Non"), 1000, TRUE),
      X_SMOKER3 = sample(c("Actuel quotidien", "Actuel occasionnel", "Ancien", "Jamais"), 1000, TRUE),
      BMI       = rnorm(1000, mean = 26, sd = 5),
      BMI_CAT   = sample(c("Insuffisant", "Normal", "Surpoids", "Obèse"), 1000, TRUE),
      X_LLCPWT  = runif(1000, 0.5, 2)
    )
  })
}

# Chargement des deux jeux de données
data2014 <- load_and_prepare_data("data/2014_light.csv", 2014)
data2015 <- load_and_prepare_data("data/2015_light.csv", 2015)
data <- bind_rows(data2014, data2015)

# ============================================================================
# CSS PERSONNALISÉ
# ============================================================================
custom_css <- "
/* Animation de pulsation pour les éléments importants */
@keyframes pulse {
  0% { transform: scale(1); }
  50% { transform: scale(1.05); }
  100% { transform: scale(1); }
}

/* Animation de glow */
@keyframes glow {
  from { text-shadow: 0 0 20px rgba(52, 152, 219, 0.5); }
  to { text-shadow: 0 0 30px rgba(52, 152, 219, 0.8), 0 0 40px rgba(52, 152, 219, 0.3); }
}

/* Animation de slide-in */
@keyframes slideInFromLeft {
  0% { transform: translateX(-100%); opacity: 0; }
  100% { transform: translateX(0); opacity: 1; }
}

body {
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
  min-height: 100vh;
}

.content-wrapper, .right-side {
  background: transparent !important;
}

.main-header .navbar {
  background: linear-gradient(45deg, #2c3e50, #3498db) !important;
  border: none !important;
  box-shadow: 0 4px 20px rgba(0,0,0,0.2);
  animation: slideInFromLeft 0.5s ease-out;
}

.main-sidebar {
  background: linear-gradient(180deg, #2c3e50 0%, #34495e 100%) !important;
  box-shadow: 4px 0 20px rgba(0,0,0,0.1);
}

.sidebar-menu > li > a {
  color: #ecf0f1 !important;
  border-left: 3px solid transparent;
  transition: all 0.3s ease;
  padding: 15px 20px !important;
}

.sidebar-menu > li > a:hover {
  background: rgba(52, 152, 219, 0.3) !important;
  border-left-color: #3498db;
  transform: translateX(8px);
  box-shadow: 0 4px 15px rgba(52, 152, 219, 0.3);
}

.sidebar-menu > li.active > a {
  background: rgba(52, 152, 219, 0.4) !important;
  border-left-color: #3498db;
  font-weight: 600;
}

.box {
  background: rgba(255, 255, 255, 0.15) !important;
  backdrop-filter: blur(15px);
  border: 1px solid rgba(255, 255, 255, 0.25);
  border-radius: 20px !important;
  box-shadow: 0 8px 32px rgba(0, 0, 0, 0.15);
  margin-bottom: 20px;
  transition: all 0.4s cubic-bezier(0.175, 0.885, 0.32, 1.275);
}

.box:hover {
  transform: translateY(-8px) scale(1.02);
  box-shadow: 0 15px 45px rgba(0, 0, 0, 0.25);
  background: rgba(255, 255, 255, 0.2) !important;
}

.box-header {
  background: transparent !important;
  border-bottom: 2px solid rgba(255, 255, 255, 0.2) !important;
  color: white !important;
  padding: 20px !important;
}

.box-title {
  color: white !important;
  font-weight: 700;
  font-size: 19px;
  text-align: center;
  text-shadow: 0 2px 8px rgba(0,0,0,0.3);
  letter-spacing: 0.5px;
}

.small-box {
  border-radius: 18px !important;
  background: linear-gradient(135deg, rgba(255,255,255,0.15), rgba(255,255,255,0.08)) !important;
  backdrop-filter: blur(12px);
  border: 1px solid rgba(255, 255, 255, 0.25);
  transition: all 0.4s cubic-bezier(0.175, 0.885, 0.32, 1.275);
  cursor: pointer;
}

.small-box:hover {
  transform: translateY(-12px) scale(1.05);
  box-shadow: 0 20px 40px rgba(0, 0, 0, 0.3);
  animation: pulse 1.5s infinite;
}

.small-box h3, .small-box p {
  color: white !important;
  text-shadow: 0 2px 6px rgba(0,0,0,0.3);
}

.small-box h3 {
  font-size: 42px !important;
  font-weight: 800 !important;
}

.small-box .icon {
  color: rgba(255, 255, 255, 0.35) !important;
  font-size: 85px !important;
}

.control-section {
  background: rgba(255, 255, 255, 0.15) !important;
  backdrop-filter: blur(12px);
  border: 1px solid rgba(255, 255, 255, 0.25);
  border-radius: 18px;
  padding: 25px;
  margin: 15px;
  box-shadow: 0 10px 35px rgba(0, 0, 0, 0.15);
  transition: all 0.3s ease;
}

.control-section:hover {
  box-shadow: 0 15px 45px rgba(0, 0, 0, 0.2);
  transform: translateY(-3px);
}

.control-section h4 {
  color: white !important;
  margin-bottom: 25px !important;
  text-align: center;
  font-weight: 700;
  text-shadow: 0 3px 6px rgba(0,0,0,0.4);
  font-size: 20px !important;
  letter-spacing: 1px;
}

.form-group label {
  color: white !important;
  font-weight: 600;
  margin-bottom: 10px;
  display: block;
  font-size: 15px;
  text-shadow: 0 2px 4px rgba(0,0,0,0.2);
}

.form-control, .selectize-input {
  background: rgba(255, 255, 255, 0.95) !important;
  border: 2px solid rgba(255, 255, 255, 0.4) !important;
  color: #2c3e50 !important;
  border-radius: 12px !important;
  padding: 10px 15px;
  font-size: 15px;
  font-weight: 500;
  transition: all 0.3s ease;
  box-shadow: 0 4px 10px rgba(0,0,0,0.1);
}

.form-control:focus, .selectize-input.focus {
  background: white !important;
  border-color: #3498db !important;
  box-shadow: 0 0 0 0.3rem rgba(52, 152, 219, 0.35) !important;
  transform: translateY(-2px);
}

.main-title {
  text-align: center;
  color: white;
  font-size: 2.8em;
  font-weight: 900;
  text-shadow: 0 0 25px rgba(52, 152, 219, 0.6);
  margin-bottom: 35px;
  animation: glow 2s ease-in-out infinite alternate;
  letter-spacing: 2px;
}

/* Style pour le tableau DataTable */
.dataTables_wrapper {
  background: white !important;
  padding: 25px 35px !important;
  border-radius: 15px;
  box-shadow: 0 8px 25px rgba(0,0,0,0.1);
  margin: 0 auto !important;
  max-width: 900px !important;
}

table.dataTable {
  background: white !important;
  border-radius: 12px;
  overflow: hidden;
  width: 100% !important;
  margin: 0 auto !important;
}

table.dataTable thead th {
  background: linear-gradient(135deg, #3498db, #2980b9) !important;
  color: white !important;
  font-weight: 700 !important;
  font-size: 18px !important;
  padding: 18px 20px !important;
  border: none !important;
  text-align: left !important;
}

table.dataTable thead th:nth-child(2) {
  text-align: center !important;
}

table.dataTable tbody td {
  color: #2c3e50 !important;
  font-size: 17px !important;
  padding: 16px 20px !important;
  font-weight: 600 !important;
  border-bottom: 2px solid rgba(52, 152, 219, 0.15) !important;
}

table.dataTable tbody td:first-child {
  text-align: left !important;
}

table.dataTable tbody td:nth-child(2) {
  text-align: center !important;
}

table.dataTable tbody tr {
  transition: all 0.3s ease;
}

table.dataTable tbody tr:hover {
  background: linear-gradient(90deg, rgba(52, 152, 219, 0.08), rgba(52, 152, 219, 0.04)) !important;
  transform: translateX(5px);
  box-shadow: 0 4px 15px rgba(52, 152, 219, 0.15);
}

/* Animations d'entrée pour les boxes */
.box {
  animation: fadeInUp 0.6s ease-out;
}

@keyframes fadeInUp {
  from {
    opacity: 0;
    transform: translateY(30px);
  }
  to {
    opacity: 1;
    transform: translateY(0);
  }
}

/* Style pour les graphiques plotly */
.plotly {
  border-radius: 15px;
  overflow: hidden;
}
"

# ============================================================================
# INTERFACE UTILISATEUR
# ============================================================================
ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center;",
      tags$i(class = "fa fa-heartbeat", style = "margin-right: 10px; font-size: 22px;"),
      "Analyse BRFSS – Santé et Comportements"
    )
  ),
  
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("🏠 Accueil",                tabName = "overview",   icon = icon("tachometer-alt")),
      menuItem("👥 Population",             tabName = "population", icon = icon("users")),
      menuItem("🏥 Accès aux soins",        tabName = "healthcare", icon = icon("hospital")),
      menuItem("⚠️ Comportements à risque",  tabName = "behaviors",  icon = icon("running")),
      menuItem("📊 Analyses croisées",      tabName = "analysis",   icon = icon("chart-line")),
      menuItem("🎨 Visualisation avancée",  tabName = "advanced",   icon = icon("layer-group")),
      menuItem("🔬 Analyse gtable/grob",    tabName = "grob",       icon = icon("microscope")),
      menuItem("✨ Géom personnalisé",      tabName = "custom",     icon = icon("magic"))
    ),
    
    div(class = "control-section",
        h4("⚙️ FILTRES D'ANALYSE"),
        
        selectInput("year_filter",
                    label = "📅 Année des données",
                    choices = c("Toutes les années" = "all", "2014" = "2014", "2015" = "2015"),
                    selected = "all"),
        
        selectInput("health_var",
                    label = "❤️ Indicateur de santé",
                    choices = c("État de santé perçu" = "GENHLTH",
                                "Indice de masse corporelle" = "BMI",
                                "Catégorie d'IMC" = "BMI_CAT"),
                    selected = "GENHLTH"),
        
        selectInput("demo_var",
                    label = "👤 Caractéristique socio-démographique",
                    choices = c("Sexe" = "SEX",
                                "Groupe d'âge" = "X_AGE_G",
                                "Niveau d'éducation" = "EDUCA",
                                "Niveau de revenu" = "INCOME2"),
                    selected = "X_AGE_G"),
        
        selectInput("behavior_var",
                    label = "🏃 Comportement à risque",
                    choices = c("Activité physique" = "EXERANY2",
                                "Tabagisme" = "X_SMOKER3"),
                    selected = "EXERANY2")
    )
  ),
  
  # Body
  dashboardBody(
    tags$head(
      tags$style(HTML(custom_css)),
      tags$script(HTML("
        $(document).ready(function() {
          // Animation au clic sur les value boxes
          $('.small-box').on('click', function() {
            $(this).addClass('pulse');
            setTimeout(() => $(this).removeClass('pulse'), 1000);
          });
        });
      "))
    ),
    
    tabItems(
      # Vue d'ensemble
      tabItem("overview",
              div(class = "main-title", "📈 ANALYSE DES COMPORTEMENTS ET DE LA SANTÉ PERÇUE"),
              fluidRow(
                valueBoxOutput("total_obs", width = 3),
                valueBoxOutput("excellent_health", width = 3),
                valueBoxOutput("with_insurance", width = 3),
                valueBoxOutput("physically_active", width = 3)
              ),
              fluidRow(
                box(title = "📋 RÉSUMÉ GÉNÉRAL DES INDICATEURS", status = "primary",
                    solidHeader = TRUE, width = 12,
                    DTOutput("summary_table"))
              )
      ),
      
      # Population
      tabItem("population",
              fluidRow(
                box(title = "📊 Répartition de la population selon les caractéristiques choisies",
                    status = "primary", solidHeader = TRUE, width = 6,
                    plotlyOutput("demo_distribution", height = "420px")),
                box(title = "💚 État de santé perçu par les répondants",
                    status = "info", solidHeader = TRUE, width = 6,
                    plotlyOutput("health_status", height = "420px"))
              ),
              fluidRow(
                box(title = "👫 Répartition détaillée par âge et sexe",
                    status = "success", solidHeader = TRUE, width = 12,
                    plotlyOutput("age_sex_distribution", height = "480px"))
              )
      ),
      
      # Accès aux soins
      tabItem("healthcare",
              fluidRow(
                box(title = "🛡️ Taux de couverture par une assurance santé",
                    status = "warning", solidHeader = TRUE, width = 6,
                    plotlyOutput("insurance_coverage", height = "420px")),
                box(title = "💰 Couverture assurance selon le niveau de revenu",
                    status = "danger", solidHeader = TRUE, width = 6,
                    plotlyOutput("insurance_by_income", height = "420px"))
              ),
              fluidRow(
                box(title = "⚖️ Comparaison de l'état de santé perçu : assurés vs non-assurés",
                    status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("health_by_insurance", height = "480px"))
              )
      ),
      
      # Comportements
      tabItem("behaviors",
              fluidRow(
                box(title = "🏃‍♂️ Niveau d'activité physique déclarée",
                    status = "success", solidHeader = TRUE, width = 6,
                    plotlyOutput("physical_activity", height = "420px")),
                box(title = "🚬 Statut tabagique des répondants",
                    status = "warning", solidHeader = TRUE, width = 6,
                    plotlyOutput("smoking_status", height = "420px"))
              ),
              fluidRow(
                box(title = "⚖️ Indice de masse corporelle selon les caractéristiques démographiques",
                    status = "info", solidHeader = TRUE, width = 12,
                    plotlyOutput("bmi_by_demo", height = "480px"))
              )
      ),
      
      # Analyses croisées
      tabItem("analysis",
              fluidRow(
                box(title = "🔗 Interactions entre caractéristiques démographiques, comportements et santé",
                    status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("multivariate_analysis", height = "520px"))
              ),
              fluidRow(
                box(title = "📈 Relations entre âge, activité physique et état de santé perçu",
                    status = "success", solidHeader = TRUE, width = 12,
                    plotlyOutput("correlation_plot", height = "520px"))
              )
      ),
      
      # Visualisation avancée
      tabItem("advanced",
              fluidRow(
                box(title = "🎨 COMPOSITION AVANCÉE DE VISUALISATIONS",
                    status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("grid_plot", height = "800px"))
              )
      ),
      
      # Analyse gtable/grob
      tabItem("grob",
              fluidRow(
                box(title = "🔬 ANALYSE STRUCTURELLE D'UN GRAPHIQUE GGPLOT",
                    status = "primary", solidHeader = TRUE, width = 12,
                    h4("Exploration de la structure interne gtable/grob", 
                       style = "color: white; text-align: center; margin-bottom: 20px;"),
                    plotOutput("original_ggplot", height = "400px")
                )
              ),
              fluidRow(
                box(title = "📊 Structure gtable extraite", status = "info",
                    solidHeader = TRUE, width = 6,
                    verbatimTextOutput("gtable_structure")),
                box(title = "🧩 Liste des grobs constitutifs", status = "warning",
                    solidHeader = TRUE, width = 6,
                    verbatimTextOutput("grobs_list"))
              ),
              fluidRow(
                box(title = "✏️ MANIPULATION DIRECTE AVEC GRID",
                    status = "success", solidHeader = TRUE, width = 12,
                    h4("Graphique modifié avec ajout d'annotations grid", 
                       style = "color: white; text-align: center; margin-bottom: 20px;"),
                    plotOutput("modified_grob", height = "450px"))
              )
      ),
      
      # Géom personnalisé
      tabItem("custom",
              fluidRow(
                box(title = "✨ GÉOM PERSONNALISÉ AVEC GGPROTO",
                    status = "primary", solidHeader = TRUE, width = 12,
                    h4("Implémentation d'un géom custom utilisant des primitives grid", 
                       style = "color: white; text-align: center; margin-bottom: 20px;"),
                    tags$div(
                      style = "background: rgba(255,255,255,0.1); padding: 20px; border-radius: 10px; margin-bottom: 20px;",
                      tags$h5("📝 Description du géom personnalisé:", style = "color: white; font-weight: bold;"),
                      tags$p("Ce géom crée des barres avec labels intégrés montrant la valeur et le pourcentage.", 
                             style = "color: white; font-size: 15px;"),
                      tags$p("• Utilisation de ggproto pour créer GeomLabeledBar et StatLabeledBar", 
                             style = "color: white; font-size: 14px;"),
                      tags$p("• Primitives grid utilisées: rectGrob() pour les barres, textGrob() pour les labels", 
                             style = "color: white; font-size: 14px;"),
                      tags$p("• Combinaison des grobs via gTree() et gList()", 
                             style = "color: white; font-size: 14px;")
                    ),
                    plotOutput("custom_geom_plot", height = "500px")
                )
              ),
              fluidRow(
                box(title = "💡 Comparaison: Géom standard vs Géom personnalisé",
                    status = "info", solidHeader = TRUE, width = 12,
                    plotOutput("geom_comparison", height = "400px"))
              )
      )
    )
  )
)

# ============================================================================
# SERVEUR
# ============================================================================
server <- function(input, output, session) {
  
  # Données filtrées par année
  filtered_data <- reactive({
    if (input$year_filter == "all") data else data %>% filter(Year == as.numeric(input$year_filter))
  })
  
  # Value Boxes (pondérées)
  output$total_obs <- renderValueBox({
    valueBox(
      formatC(round(sum(filtered_data()$X_LLCPWT, na.rm = TRUE), 0), format="d", big.mark=" "),
      "Observations pondérées (total)",
      icon = icon("database"), 
      color = "blue"
    )
  })
  
  output$excellent_health <- renderValueBox({
    pct <- round(100 * weighted.mean(filtered_data()$GENHLTH == "Excellent", filtered_data()$X_LLCPWT, na.rm = TRUE), 1)
    valueBox(paste0(pct, "%"), "Santé perçue excellente", icon = icon("heart"), color = "green")
  })
  
  output$with_insurance <- renderValueBox({
    pct <- round(100 * weighted.mean(filtered_data()$HLTHPLN1 == "Oui", filtered_data()$X_LLCPWT, na.rm = TRUE), 1)
    valueBox(paste0(pct, "%"), "Personnes couvertes par assurance", icon = icon("shield-alt"), color = "yellow")
  })
  
  output$physically_active <- renderValueBox({
    pct <- round(100 * weighted.mean(filtered_data()$EXERANY2 == "Oui", filtered_data()$X_LLCPWT, na.rm = TRUE), 1)
    valueBox(paste0(pct, "%"), "Personnes physiquement actives", icon = icon("running"), color = "purple")
  })
  
  # Tableau résumé pondéré
  output$summary_table <- renderDT({
    fd <- filtered_data()
    data.frame(
      Indicateur = c("📊 IMC moyen pondéré", 
                     "👩 Proportion de femmes", 
                     "🛡️ Proportion assurées", 
                     "🏃 Proportion actives physiquement",
                     "🚬 Proportion fumeurs actuels"),
      Valeur = c(
        round(weighted.mean(fd$BMI, fd$X_LLCPWT, na.rm = TRUE), 1),
        paste0(round(100 * weighted.mean(fd$SEX == "Femme", fd$X_LLCPWT, na.rm = TRUE), 1), "%"),
        paste0(round(100 * weighted.mean(fd$HLTHPLN1 == "Oui", fd$X_LLCPWT, na.rm = TRUE), 1), "%"),
        paste0(round(100 * weighted.mean(fd$EXERANY2 == "Oui", fd$X_LLCPWT, na.rm = TRUE), 1), "%"),
        paste0(round(100 * weighted.mean(grepl("Actuel", fd$X_SMOKER3), fd$X_LLCPWT, na.rm = TRUE), 1), "%")
      )
    ) %>%
      datatable(
        options = list(
          dom = 't',
          ordering = FALSE,
          autoWidth = FALSE,
          scrollX = FALSE,
          columnDefs = list(
            list(className = 'dt-left', targets = 0, width = '70%'),
            list(className = 'dt-center', targets = 1, width = '30%')
          )
        ),
        rownames = FALSE,
        class = 'display compact'
      ) %>%
      formatStyle(
        columns = c('Indicateur', 'Valeur'),
        color = '#2c3e50',
        backgroundColor = 'white',
        fontWeight = '600',
        fontSize = '17px'
      ) %>%
      formatStyle(
        'Indicateur',
        fontWeight = 'bold',
        color = '#2980b9',
        paddingLeft = '20px'
      ) %>%
      formatStyle(
        'Valeur',
        fontWeight = 'bold',
        color = '#27ae60',
        fontSize = '19px'
      )
  })
  
  # Graphiques interactifs avec plotly
  output$demo_distribution <- renderPlotly({
    filtered_data() %>%
      group_by(!!sym(input$demo_var)) %>%
      summarise(count = sum(X_LLCPWT, na.rm = TRUE)) %>%
      mutate(pct = count / sum(count) * 100) %>%
      plot_ly(x = ~.data[[input$demo_var]], y = ~pct, type = "bar", 
              marker = list(color = '#3498db',
                            line = list(color = '#2980b9', width = 2)),
              hovertemplate = paste('<b>%{x}</b><br>',
                                    'Pourcentage: %{y:.1f}%<br>',
                                    '<extra></extra>')) %>%
      layout(title = list(text = paste("Répartition selon", input$demo_var),
                          font = list(size = 18, color = '#2c3e50', family = 'Arial, sans-serif')),
             xaxis = list(title = "", tickfont = list(size = 14)),
             yaxis = list(title = "Pourcentage (%)", tickfont = list(size = 14)),
             plot_bgcolor = 'rgba(255,255,255,0.95)',
             paper_bgcolor = 'rgba(255,255,255,0.95)')
  })
  
  output$health_status <- renderPlotly({
    filtered_data() %>%
      group_by(GENHLTH) %>%
      summarise(count = sum(X_LLCPWT, na.rm = TRUE)) %>%
      plot_ly(labels = ~GENHLTH, values = ~count, type = "pie",
              marker = list(colors = c('#27ae60','#2ecc71','#f39c12','#e67e22','#e74c3c'),
                            line = list(color = '#fff', width = 2)),
              textposition = 'inside',
              textinfo = 'label+percent',
              hoverinfo = 'label+value+percent') %>%
      layout(title = list(text = "État de santé perçu",
                          font = list(size = 18, color = '#2c3e50')),
             showlegend = TRUE,
             plot_bgcolor = 'rgba(255,255,255,0.95)',
             paper_bgcolor = 'rgba(255,255,255,0.95)')
  })
  
  output$age_sex_distribution <- renderPlotly({
    filtered_data() %>%
      group_by(X_AGE_G, SEX) %>%
      summarise(count = sum(X_LLCPWT, na.rm = TRUE)) %>%
      plot_ly(x = ~X_AGE_G, y = ~count, color = ~SEX, type = "bar", 
              colors = c('#3498db', '#e74c3c'),
              hovertemplate = paste('<b>%{x}</b><br>',
                                    '%{fullData.name}<br>',
                                    'Nombre: %{y:,.0f}<br>',
                                    '<extra></extra>')) %>%
      layout(barmode = "group",
             title = list(text = "Répartition par groupe d'âge et sexe",
                          font = list(size = 18, color = '#2c3e50')),
             xaxis = list(title = "Groupe d'âge", tickfont = list(size = 14)),
             yaxis = list(title = "Nombre pondéré", tickfont = list(size = 14)),
             plot_bgcolor = 'rgba(255,255,255,0.95)',
             paper_bgcolor = 'rgba(255,255,255,0.95)')
  })
  
  output$insurance_coverage <- renderPlotly({
    filtered_data() %>%
      group_by(HLTHPLN1) %>%
      summarise(count = sum(X_LLCPWT, na.rm = TRUE)) %>%
      plot_ly(labels = ~HLTHPLN1, values = ~count, type = "pie",
              marker = list(colors = c('#27ae60', '#e74c3c'),
                            line = list(color = '#fff', width = 2)),
              textposition = 'inside',
              textinfo = 'label+percent') %>%
      layout(title = list(text = "Couverture par assurance santé",
                          font = list(size = 18, color = '#2c3e50')),
             plot_bgcolor = 'rgba(255,255,255,0.95)',
             paper_bgcolor = 'rgba(255,255,255,0.95)')
  })
  
  output$insurance_by_income <- renderPlotly({
    filtered_data() %>%
      group_by(INCOME2, HLTHPLN1) %>%
      summarise(count = sum(X_LLCPWT, na.rm = TRUE)) %>%
      group_by(INCOME2) %>%
      mutate(pct = count / sum(count) * 100) %>%
      filter(HLTHPLN1 == "Oui") %>%
      plot_ly(x = ~INCOME2, y = ~pct, type = "bar", 
              marker = list(color = '#f39c12',
                            line = list(color = '#e67e22', width = 2))) %>%
      layout(title = list(text = "Proportion de personnes assurées selon le revenu",
                          font = list(size = 18, color = '#2c3e50')),
             xaxis = list(title = "Niveau de revenu", tickfont = list(size = 14)),
             yaxis = list(title = "Pourcentage (%)", tickfont = list(size = 14)),
             plot_bgcolor = 'rgba(255,255,255,0.95)',
             paper_bgcolor = 'rgba(255,255,255,0.95)')
  })
  
  output$health_by_insurance <- renderPlotly({
    filtered_data() %>%
      group_by(HLTHPLN1, GENHLTH) %>%
      summarise(count = sum(X_LLCPWT, na.rm = TRUE)) %>%
      group_by(HLTHPLN1) %>%
      mutate(pct = count / sum(count) * 100) %>%
      plot_ly(x = ~GENHLTH, y = ~pct, color = ~HLTHPLN1, type = "bar", 
              colors = c('#27ae60', '#e74c3c')) %>%
      layout(barmode = "group",
             title = list(text = "État de santé perçu selon le statut d'assurance",
                          font = list(size = 18, color = '#2c3e50')),
             xaxis = list(title = "État de santé", tickfont = list(size = 14)),
             yaxis = list(title = "Pourcentage (%)", tickfont = list(size = 14)),
             plot_bgcolor = 'rgba(255,255,255,0.95)',
             paper_bgcolor = 'rgba(255,255,255,0.95)')
  })
  
  output$physical_activity <- renderPlotly({
    filtered_data() %>%
      group_by(EXERANY2) %>%
      summarise(count = sum(X_LLCPWT, na.rm = TRUE)) %>%
      plot_ly(labels = ~EXERANY2, values = ~count, type = "pie",
              marker = list(colors = c('#27ae60', '#95a5a6'),
                            line = list(color = '#fff', width = 2)),
              textposition = 'inside',
              textinfo = 'label+percent') %>%
      layout(title = list(text = "Niveau d'activité physique",
                          font = list(size = 18, color = '#2c3e50')),
             plot_bgcolor = 'rgba(255,255,255,0.95)',
             paper_bgcolor = 'rgba(255,255,255,0.95)')
  })
  
  output$smoking_status <- renderPlotly({
    filtered_data() %>%
      group_by(X_SMOKER3) %>%
      summarise(count = sum(X_LLCPWT, na.rm = TRUE)) %>%
      mutate(pct = count / sum(count) * 100) %>%
      plot_ly(x = ~X_SMOKER3, y = ~pct, type = "bar",
              marker = list(color = c('#e74c3c', '#e67e22', '#f39c12', '#27ae60'),
                            line = list(color = '#c0392b', width = 2))) %>%
      layout(title = list(text = "Statut tabagique des répondants",
                          font = list(size = 18, color = '#2c3e50')),
             xaxis = list(title = "", tickfont = list(size = 14)),
             yaxis = list(title = "Pourcentage (%)", tickfont = list(size = 14)),
             plot_bgcolor = 'rgba(255,255,255,0.95)',
             paper_bgcolor = 'rgba(255,255,255,0.95)')
  })
  
  output$bmi_by_demo <- renderPlotly({
    filtered_data() %>%
      group_by(!!sym(input$demo_var)) %>%
      summarise(mean_bmi = weighted.mean(BMI, X_LLCPWT, na.rm = TRUE)) %>%
      plot_ly(x = ~.data[[input$demo_var]], y = ~mean_bmi, type = "bar",
              marker = list(color = '#3498db',
                            line = list(color = '#2980b9', width = 2))) %>%
      layout(title = list(text = paste("IMC moyen selon", input$demo_var),
                          font = list(size = 18, color = '#2c3e50')),
             xaxis = list(title = "", tickfont = list(size = 14)),
             yaxis = list(title = "IMC moyen", tickfont = list(size = 14)),
             plot_bgcolor = 'rgba(255,255,255,0.95)',
             paper_bgcolor = 'rgba(255,255,255,0.95)')
  })
  
  output$multivariate_analysis <- renderPlotly({
    filtered_data() %>%
      group_by(!!sym(input$demo_var), !!sym(input$behavior_var), !!sym(input$health_var)) %>%
      summarise(count = sum(X_LLCPWT, na.rm = TRUE)) %>%
      plot_ly(x = ~.data[[input$demo_var]], y = ~count,
              color = ~.data[[input$behavior_var]], type = "bar") %>%
      layout(barmode = "stack",
             title = list(text = "Interactions multivariées",
                          font = list(size = 18, color = '#2c3e50')),
             xaxis = list(tickfont = list(size = 14)),
             yaxis = list(title = "Nombre pondéré", tickfont = list(size = 14)),
             plot_bgcolor = 'rgba(255,255,255,0.95)',
             paper_bgcolor = 'rgba(255,255,255,0.95)')
  })
  
  output$correlation_plot <- renderPlotly({
    filtered_data() %>%
      filter(GENHLTH %in% c("Excellent", "Très bon", "Bon")) %>%
      group_by(X_AGE_G, EXERANY2, GENHLTH) %>%
      summarise(count = sum(X_LLCPWT, na.rm = TRUE)) %>%
      plot_ly(x = ~X_AGE_G, y = ~count, color = ~EXERANY2,
              facet_col = ~GENHLTH, type = "bar") %>%
      layout(title = list(text = "Relations âge, activité physique et santé",
                          font = list(size = 18, color = '#2c3e50')),
             plot_bgcolor = 'rgba(255,255,255,0.95)',
             paper_bgcolor = 'rgba(255,255,255,0.95)')
  })
  
  # Visualisation Grid avancée - VERSION AMÉLIORÉE ET ÉPURÉE
  output$grid_plot <- renderPlot({
    par(family = "sans")
    
    grid.newpage()
    
    # Fond blanc propre
    grid.rect(gp = gpar(fill = "white", col = NA))
    
    # Layout avec plus d'espace pour le titre
    pushViewport(viewport(layout = grid.layout(3, 2,
                                               widths = unit(c(0.5, 0.5), "npc"),
                                               heights = unit(c(0.08, 0.46, 0.46), "npc"))))
    
    # ========== TITRE GLOBAL ==========
    title_vp <- viewport(layout.pos.row = 1, layout.pos.col = 1:2)
    pushViewport(title_vp)
    
    grid.rect(gp = gpar(fill = "#2c3e50", col = NA))
    grid.text("TABLEAU DE BORD VISUEL - ANALYSE BRFSS 2014-2015",
              x = 0.5, y = 0.65,
              gp = gpar(fontsize = 20, fontface = "bold", col = "white", fontfamily = "sans"))
    grid.text("Comportements de santé et facteurs socio-démographiques",
              x = 0.5, y = 0.3,
              gp = gpar(fontsize = 13, fontface = "italic", col = "#ecf0f1", fontfamily = "sans"))
    
    popViewport()
    
    # ========== GRAPHIQUE 1: État de santé (chiffres réduits) ==========
    vp1 <- viewport(layout.pos.row = 2, layout.pos.col = 1)
    pushViewport(vp1)
    
    df1 <- filtered_data() %>%
      group_by(GENHLTH) %>%
      summarise(count = sum(X_LLCPWT, na.rm = TRUE))
    
    p1 <- ggplot(df1, aes(x = GENHLTH, y = count, fill = GENHLTH)) +
      geom_bar(stat = "identity", width = 0.75, alpha = 0.9) +
      geom_text(aes(label = scales::comma(round(count / 1000), accuracy = 1)),
                vjust = -0.6, size = 3.8, fontface = "bold", color = "#2c3e50") +  # taille réduite + en milliers
      scale_fill_manual(values = c('#27ae60', '#2ecc71', '#f39c12', '#e67e22', '#e74c3c')) +
      scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.18))) +
      theme_minimal(base_size = 14) +
      theme(
        plot.background = element_rect(fill = "white", color = "#bdc3c7", size = 1.5),
        panel.background = element_rect(fill = "#f8f9fa", color = NA),
        panel.grid.major.y = element_line(color = "grey90", size = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 11, face = "bold", color = "#2c3e50"),
        axis.text.y = element_text(size = 10, color = "#495057"),
        axis.title.y = element_text(size = 12, face = "bold", color = "#2c3e50", margin = margin(r = 8)),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5, color = "#2c3e50", margin = margin(b = 10)),
        legend.position = "none",
        plot.margin = margin(15, 15, 15, 15)
      ) +
      labs(title = "État de santé perçu", y = "Effectif pondéré (en milliers)")
    
    print(p1, newpage = FALSE)
    popViewport()
    
    # ========== GRAPHIQUE 2: IMC par sexe ==========
    vp2 <- viewport(layout.pos.row = 2, layout.pos.col = 2)
    pushViewport(vp2)
    
    df2 <- filtered_data() %>% filter(!is.na(BMI), !is.na(SEX))
    
    p2 <- ggplot(df2, aes(x = SEX, y = BMI, fill = SEX)) +
      geom_violin(alpha = 0.6, trim = FALSE) +
      geom_boxplot(width = 0.25, alpha = 0.9, outlier.alpha = 0.3, outlier.size = 1) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3.5,
                   fill = "white", color = "black", stroke = 1.2) +
      scale_fill_manual(values = c('#3498db', '#e74c3c')) +
      theme_minimal(base_size = 14) +
      theme(
        plot.background = element_rect(fill = "white", color = "#bdc3c7", size = 1.5),
        panel.background = element_rect(fill = "#f8f9fa", color = NA),
        panel.grid.major.y = element_line(color = "grey90", size = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 11, face = "bold", color = "#2c3e50"),
        axis.text.y = element_text(size = 10, color = "#495057"),
        axis.title.y = element_text(size = 12, face = "bold", color = "#2c3e50", margin = margin(r = 8)),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5, color = "#2c3e50", margin = margin(b = 10)),
        legend.position = "none",
        plot.margin = margin(15, 15, 15, 15)
      ) +
      labs(title = "Distribution IMC par sexe", y = "IMC (kg/m²)")
    
    print(p2, newpage = FALSE)
    popViewport()
    
    # ========== GRAPHIQUE 3: Couverture santé ==========
    vp3 <- viewport(layout.pos.row = 3, layout.pos.col = 1)
    pushViewport(vp3)
    
    df3 <- filtered_data() %>%
      group_by(INCOME2, HLTHPLN1) %>%
      summarise(count = sum(X_LLCPWT, na.rm = TRUE)) %>%
      group_by(INCOME2) %>%
      mutate(percentage = count / sum(count) * 100)
    
    p3 <- ggplot(df3, aes(x = INCOME2, y = percentage, fill = HLTHPLN1)) +
      geom_bar(stat = "identity", position = "fill", width = 0.8, alpha = 0.9) +
      geom_text(aes(label = ifelse(percentage > 8, paste0(round(percentage), "%"), "")),
                position = position_fill(vjust = 0.5),
                size = 3.2, fontface = "bold", color = "white") +  # taille réduite
      scale_fill_manual(values = c('#27ae60', '#e74c3c'),
                        labels = c("Assuré", "Non assuré")) +
      scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
      coord_flip() +
      theme_minimal(base_size = 14) +
      theme(
        plot.background = element_rect(fill = "white", color = "#bdc3c7", size = 1.5),
        panel.background = element_rect(fill = "#f8f9fa", color = NA),
        panel.grid.major.x = element_line(color = "#dee2e6", size = 0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 10, face = "bold", color = "#2c3e50"),
        axis.text.x = element_text(size = 9, color = "#495057"),
        axis.title = element_text(size = 12, face = "bold", color = "#2c3e50"),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5, color = "#2c3e50", margin = margin(b = 10)),
        legend.title = element_blank(),
        legend.text = element_text(size = 10, face = "bold", color = "#2c3e50"),
        legend.position = "bottom",
        legend.background = element_rect(fill = "white", color = NA),
        legend.key.size = unit(0.8, "cm"),
        plot.margin = margin(15, 15, 15, 15)
      ) +
      labs(title = "Couverture santé / revenu", y = "Proportion", x = NULL)
    
    print(p3, newpage = FALSE)
    popViewport()
    
    # ========== GRAPHIQUE 4: Activité physique ==========
    vp4 <- viewport(layout.pos.row = 3, layout.pos.col = 2)
    pushViewport(vp4)
    
    df4 <- filtered_data() %>%
      group_by(X_AGE_G, EXERANY2) %>%
      summarise(count = sum(X_LLCPWT, na.rm = TRUE)) %>%
      group_by(X_AGE_G) %>%
      mutate(pct = count / sum(count) * 100) %>%
      filter(EXERANY2 == "Oui")
    
    p4 <- ggplot(df4, aes(x = X_AGE_G, y = pct)) +
      geom_segment(aes(x = X_AGE_G, xend = X_AGE_G, y = 0, yend = pct),
                   color = "#3498db", size = 2.5, alpha = 0.8) +
      geom_point(size = 6, color = "#27ae60", alpha = 0.95) +
      geom_text(aes(label = paste0(round(pct), "%")),
                vjust = -1.2, size = 3.6, fontface = "bold", color = "#2c3e50") +  # taille réduite
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25),
                         labels = function(x) paste0(x, "%"),
                         expand = expansion(mult = c(0, 0.1))) +
      theme_minimal(base_size = 14) +
      theme(
        plot.background = element_rect(fill = "white", color = "#bdc3c7", size = 1.5),
        panel.background = element_rect(fill = "#f8f9fa", color = NA),
        panel.grid.major.y = element_line(color = "grey90", size = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 10, face = "bold", color = "#2c3e50"),
        axis.text.y = element_text(size = 9, color = "#495057"),
        axis.title.y = element_text(size = 12, face = "bold", color = "#2c3e50", margin = margin(r = 8)),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5, color = "#2c3e50", margin = margin(b = 10)),
        plot.margin = margin(15, 15, 15, 15)
      ) +
      labs(title = "Activité physique par âge", y = "% actifs")
    
    print(p4, newpage = FALSE)
    popViewport()
    
  }, res = 120, bg = "white")  # Résolution élevée pour netteté
  
  # ============================================================================
  # ANALYSE GTABLE/GROB (PARTIE OBLIGATOIRE DU PROJET)
  # ============================================================================
  
  # Graphique original pour analyse
  output$original_ggplot <- renderPlot({
    df <- filtered_data() %>%
      group_by(X_AGE_G, GENHLTH) %>%
      summarise(count = sum(X_LLCPWT, na.rm = TRUE))
    
    ggplot(df, aes(x = X_AGE_G, y = count, fill = GENHLTH)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c('#27ae60', '#2ecc71', '#f39c12', '#e67e22', '#e74c3c')) +
      theme_minimal(base_size = 14) +
      labs(title = "Santé perçue par groupe d'âge",
           x = "Groupe d'âge", y = "Nombre pondéré", fill = "État de santé")
  }, bg = "white")
  
  # Structure gtable
  output$gtable_structure <- renderPrint({
    df <- filtered_data() %>%
      group_by(X_AGE_G, GENHLTH) %>%
      summarise(count = sum(X_LLCPWT, na.rm = TRUE))
    
    p <- ggplot(df, aes(x = X_AGE_G, y = count, fill = GENHLTH)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c('#27ae60', '#2ecc71', '#f39c12', '#e67e22', '#e74c3c')) +
      theme_minimal()
    
    gt <- ggplotGrob(p)
    
    cat("=== STRUCTURE GTABLE ===\n\n")
    cat("Classe:", class(gt), "\n")
    cat("Dimensions:", nrow(gt), "lignes x", ncol(gt), "colonnes\n\n")
    cat("Layout:\n")
    print(gt$layout[1:15, ])
    cat("\n... (", nrow(gt$layout) - 15, "lignes supplémentaires)\n")
  })
  
  # Liste des grobs
  output$grobs_list <- renderPrint({
    df <- filtered_data() %>%
      group_by(X_AGE_G, GENHLTH) %>%
      summarise(count = sum(X_LLCPWT, na.rm = TRUE))
    
    p <- ggplot(df, aes(x = X_AGE_G, y = count, fill = GENHLTH)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal()
    
    gt <- ggplotGrob(p)
    
    cat("=== LISTE DES GROBS CONSTITUTIFS ===\n\n")
    cat("Nombre total de grobs:", length(gt$grobs), "\n\n")
    
    for (i in 1:min(15, length(gt$grobs))) {
      cat(sprintf("Grob %d: %s\n", i, gt$layout$name[i]))
      cat(sprintf("  - Classe: %s\n", class(gt$grobs[[i]])[1]))
      cat(sprintf("  - Position: t=%d, l=%d, b=%d, r=%d\n", 
                  gt$layout$t[i], gt$layout$l[i], gt$layout$b[i], gt$layout$r[i]))
      cat("\n")
    }
    
    cat("\n=== GROBS PRINCIPAUX ===\n")
    cat("• panel: zone de tracé des données\n")
    cat("• axis-l/axis-b: axes gauche et bas\n")
    cat("• xlab-b/ylab-l: labels des axes\n")
    cat("• guide-box: légende\n")
    cat("• title: titre du graphique\n")
  })
  
  # Graphique modifié avec grid
  output$modified_grob <- renderPlot({
    df <- filtered_data() %>%
      group_by(X_AGE_G, GENHLTH) %>%
      summarise(count = sum(X_LLCPWT, na.rm = TRUE))
    
    p <- ggplot(df, aes(x = X_AGE_G, y = count, fill = GENHLTH)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c('#27ae60', '#2ecc71', '#f39c12', '#e67e22', '#e74c3c')) +
      theme_minimal(base_size = 14) +
      labs(title = "Santé perçue par groupe d'âge (MODIFIÉ)",
           x = "Groupe d'âge", y = "Nombre pondéré", fill = "État de santé")
    
    # Extraction du gtable
    gt <- ggplotGrob(p)
    
    # Modification manuelle: changer la couleur du titre
    title_grob <- gt$grobs[[which(gt$layout$name == "title")]]
    title_grob$children[[1]]$gp$col <- "#e74c3c"
    title_grob$children[[1]]$gp$fontsize <- 18
    gt$grobs[[which(gt$layout$name == "title")]] <- title_grob
    
    # Dessiner le graphique
    grid.newpage()
    grid.draw(gt)
    
    # Ajout d'annotations avec primitives grid
    grid.text("★ ANNOTATION AJOUTÉE AVEC GRID ★", 
              x = 0.5, y = 0.95, 
              gp = gpar(col = "#3498db", fontsize = 14, fontface = "bold"))
    
    # Ajout d'un rectangle en surbrillance
    grid.rect(x = 0.1, y = 0.1, width = 0.15, height = 0.08,
              gp = gpar(fill = "#f39c12", alpha = 0.3, col = "#e67e22", lwd = 2))
    
    grid.text("Zone modifiée\navec grid", 
              x = 0.1, y = 0.1,
              gp = gpar(col = "#2c3e50", fontsize = 10, fontface = "bold"))
    
    # Ajout d'une flèche
    grid.lines(x = c(0.15, 0.25), y = c(0.5, 0.4),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
               gp = gpar(col = "#e74c3c", lwd = 3))
    
    grid.text("Exemple d'annotation\navec flèche", 
              x = 0.27, y = 0.42,
              just = "left",
              gp = gpar(col = "#e74c3c", fontsize = 11, fontface = "italic"))
    
  }, bg = "white")
  
  # ============================================================================
  # GÉOM PERSONNALISÉ (PARTIE OBLIGATOIRE DU PROJET)
  # ============================================================================
  
  # Utilisation du géom personnalisé
  output$custom_geom_plot <- renderPlot({
    df <- filtered_data() %>%
      group_by(GENHLTH) %>%
      summarise(count = sum(X_LLCPWT, na.rm = TRUE))
    
    ggplot(df, aes(x = GENHLTH, y = count, fill = GENHLTH)) +
      geom_labeled_bar() +
      scale_fill_manual(values = c('#27ae60', '#2ecc71', '#f39c12', '#e67e22', '#e74c3c')) +
      theme_minimal(base_size = 15) +
      theme(
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold", size = 13),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 12, color = "gray40"),
        legend.position = "none"
      ) +
      labs(
        title = "Géom Personnalisé avec ggproto",
        subtitle = "Les labels sont intégrés directement dans les barres via des primitives grid",
        x = NULL,
        y = "Nombre pondéré"
      )
  }, bg = "white")
  
  # Comparaison géom standard vs personnalisé
  output$geom_comparison <- renderPlot({
    df <- filtered_data() %>%
      group_by(X_AGE_G) %>%
      summarise(count = sum(X_LLCPWT, na.rm = TRUE))
    
    # Graphique standard
    p1 <- ggplot(df, aes(x = X_AGE_G, y = count)) +
      geom_bar(stat = "identity", fill = "#3498db", alpha = 0.8) +
      geom_text(aes(label = scales::comma(round(count))), vjust = -0.5, size = 4) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      labs(title = "Géom Standard (geom_bar + geom_text)", x = NULL, y = "Nombre")
    
    # Graphique avec géom personnalisé
    p2 <- ggplot(df, aes(x = X_AGE_G, y = count)) +
      geom_labeled_bar(fill = "#e74c3c") +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      labs(title = "Géom Personnalisé (geom_labeled_bar)", x = NULL, y = "Nombre")
    
    # Affichage côte à côte avec grid
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(1, 2)))
    
    pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
    print(p1, newpage = FALSE)
    popViewport()
    
    pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 2))
    print(p2, newpage = FALSE)
    popViewport()
    
  }, bg = "white")
}

# ============================================================================
# LANCEMENT DE L'APPLICATION
# ============================================================================
shinyApp(ui = ui, server = server)
# 
# ---- Aphantasia Project Simulation - Source code -----------------------------
# 
# Delem, Fourment, Junoy, Leal De Almeida
# Email : m.delem@univ-lyon2.fr
# Last update : February 15, 2022

# -------------------------------- setup -------------------------------------------------------------------

# librairian (if not here already) for efficient package management
if (!require(librarian)) install.packages(librarian)
library(librarian)

# now putting packages on our shelf :
shelf(
  # the basics...
  tidyverse,
  # ... and our current needs :
  # --- data analysis packages: ---
  easystats,      # framework for data analysis functions
  cluster,        # self-explanatory (i.e. cluster analyses)
  factoextra,     # multivariate data analysis visualization
  # --- data visualization/exploration packages: ---
  ggpubr,         # publication plots
  ggradar,        # radar charts
)

# global theme
theme_set(
  theme_bw(
    base_size = 14, 
    base_family = "serif"
    )
  )

# fixing a seed for reproducibility
set.seed(14051998)

# Simulation des donnees

# definition des variables et groupes
# groupe non-aphantasique
Non_A <- data.frame(
  name = c("OSIQ_O", "OSIQ_S", "VVIQ",
           "Raven", "Simili", "Wason",
           "Empan_MDT", "WCST", "Lecture",
           "Corsi","MRT", "SRI"),
  mean = c(54.6,  46.2,  63.8,
           20.9,  37.8,  32.2,
           6.43,  32.1,  50.2,
           5.81,  16.5,  35.7),
  sd = c(8.45,  9.54,  9.67,
         5.34,  4.25,  3.78,
         2.12,  5.32,  8.89,
         1.87,  3.54,  6.23),
  group = ("Non_A") %>% factor(),
  n_subjects = 200
)

# groupe aphantasique
Aph <- data.frame(
  name = c("OSIQ_O", "OSIQ_S", "VVIQ",
           "Raven", "Simili", "Wason",
           "Empan_MDT", "WCST", "Lecture",
           "Corsi","MRT", "SRI"),
  
  mean = c(32.5,  58.9,  30.2,
           23.6,  42.2,  36.1,
           7.53,  33.8,  48.4,
           6.8,   18.2,  38.5),
  
  sd = c(8.45,  9.54,  9.67,
         4.24,  6.15,  3.47,
         1.45,  2.62,  9.67,
         1.65,  5.78,  8.21),
  group = ("Aph") %>% factor(),
  n_subjects = 200
)

# dataset fusionné
variables <- bind_rows(Aph,Non_A)
rm(Aph,Non_A)

# liens variables-capacites cognitives
fmodel <- matrix(c ( .8,   0,  0,  0,  0, # OSIQ-O = img objet
                     0,  .9,  0,  0,  0, # OSIQ-S = img spatiale
                     .9,   0,  0,  0,  0, # VVIQ = img objet
                     .1,  .3, .8,  0,.05, # Raven = raisonnmt > img s/o > flex
                     -.2,   0, .6,  0, .1, # Simili = raisonnmt >  flex
                     -.1,   0, .3,  0,  0, # Wason = raisonnmt
                     0,   0,  0, .8,  0, # Empan = MDT 
                     -.1,   0, .2,  0, .6, # WCST = Flex > raisonnmt
                     .4,   0, .6,  0,  0, # Lecture = img objet > raisonnmt
                     .1,  .7,  0, .8,  0, # Corsi = MDT > img s
                     .2, .85,  0,  0,  0, # MRT = img s
                     .1,  .9,  0,  0,  0  # SRI = img s
), 
nrow=12, ncol=5, byrow=TRUE)

# liens entre capacites cognitives
effect <- matrix(c (  1,-.1,-.1, .2,  0, # img o
                      -.1,  1, .3, .2,  0, # img s
                      -.1, .3,  1,  0, .2, # raisonnmt
                      .2, .2,  0,  1,  0, # MDT
                      0,  0, .2,  0,  1  # flex
),
nrow=5, ncol=5, byrow=TRUE)

# fonction de simulation
simulation <- function(variables, fmodel, effect) {
  
  ### preparatifs ###
  n_variables <- dim(fmodel)[1] # notre nb de mesures/variables (rows)
  n_skills <- dim(fmodel)[2]    # les capacites sous-jacentes evaluees (columns)
  
  # matrice de poids des erreurs
  errorweight <- (1 - diag(fmodel %*% t(fmodel))) %>% 
    abs() %>%   # necessaire pour la racine carree
    sqrt() %>%  # doit avoir des arguments positifs
    diag()      # recree une matrice diagonalisee
  
  # initialisation d'un dataframe vide
  data <- data.frame()
  
  ### simulation ###
  for (i in levels(variables$group)){   # on simule separement chaque groupe
    
    var_group = variables %>% filter(group == i) # donnees du groupe isolees
    n_subjects = var_group$n_subjects[1]         # nb de sujets dans le groupe
    group = i                                    # nom du groupe
    
    # generation de scores aleatoires normaux pour chaque capacite cognitive
    randomscores <- matrix(rnorm(n_subjects * (n_skills)),
                           nrow = n_subjects,
                           ncol = n_skills)
    # ponderation par la matrice d'effets = les scores sont desormais correles
    # entre eux
    skillscores <- randomscores %*% effect
    
    # genere les valeurs standardisees des mesures/variables grace a fmodel 
    observedscores <- skillscores %*% t(fmodel)
    
    # generation d'erreurs normales pour chaque mesure/variable
    randomerror <- matrix(rnorm(n_subjects * (n_variables)), 
                          nrow = n_subjects,
                          ncol = n_variables)
    # ponderation par notre matrice de poids des erreurs
    error <- randomerror %*% errorweight
    
    # nos mesures effectives = les valeurs reeles + une erreur standard
    measures <- observedscores + error
    
    # on cree un dataframe avec le nom de groupe
    data_group <- data.frame(measures) %>% 
      mutate(Group = group %>% factor())
    
    # ajout des valeurs reeles de moyenne et d'ecart-type pour chaque variable 
    # et renommage
    for (i in 1:length(var_group$name)){
      data_group[,i] = data_group[,i]*var_group$sd[i] + var_group$mean[i]
      colnames(data_group)[i] = var_group$name[i]
    }
    
    # fusion avec le dataframe complet
    data <- bind_rows(data,data_group)
  }
  
  # ajout d'id individuels et stats demographiques
  n = length(data[,1])  # nombre total de participants
  data <- data %>% 
    mutate(Subject_nr = row_number() %>% as.character(),
           Sex = (c("H","F") %>% rep(times = n/2) %>% factor()),
           Age = seq(from = 16, to = 55, by = 1) %>% sample(size = n, 
                                                            replace = TRUE)
    ) %>% 
    relocate(Subject_nr)
  
  # mission accomplished!
  return(data)  
}

# la fonction est donc clefs en main
data <- simulation(variables,fmodel,effect)

# nettoyage de l'environnement (considérations écologiques)
rm(variables, fmodel, effect)

# Pré-traitement

# on crée un dataset avec les scores uniquement
datascores <- data %>% select(-c(Subject_nr, Group, Sex, Age))

# Analyses

# le dataset réduit à 3 facteurs contre 12 initialement
data_latent <- 
  factor_analysis(datascores,
                  rotation = "cluster",
                  n = 3,
                  sort = TRUE,
                  standardize = TRUE) %>% 
  predict(names = c("Imagerie Spatiale", 
                    "Imagerie Objet",
                    "Raisonnement"))

# ajout des clusters identifiés
data_latent$cluster <- 
  cluster_analysis(datascores, n = 4, method = "hkmeans") %>%
  predict() %>% 
  as.factor()

# ---- descriptives ------------------------------------------------------------
datascores %>% 
  report() %>% 
  as.data.frame() %>% 
  summary() %>%
  select(Variable:Max) %>%
  mutate(
    Variable=replace(Variable,Variable=="OSIQ_O","OSIQ-Objet"),
    Variable=replace(Variable,Variable=="OSIQ_S","OSIQ-Spatial"),
    Variable=replace(Variable,Variable=="Empan_MDT","Empan verbal"),
    Variable=replace(Variable,Variable=="Lecture","Compréhension en lecture")
  ) %>% 
  knitr::kable(row.names = FALSE,
               caption = "Statistiques descriptives de l'ensemble des variables mesurées : Moyenne (*Mean*), Écart-type (*SD*), Minimum (*Min*) et Maximum (*Max*).\\label{descriptives}")

# ---- correlation_matrix ------------------------------------------------------
datascores %>% 
  mutate(across(everything(), ~ scale(.x))) %>% 
  correlation(partial = TRUE) %>% 
  cor_sort() %>% 
  summary() %>% 
  plot() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = NULL)

# ---- ggm_graph ---------------------------------------------------------------
datascores %>%
  correlation(partial = FALSE) %>%
  filter(abs(r) >= .3) %>% 
  mutate(Parameter1 = replace(Parameter1, Parameter1 == "Empan_MDT", "Empan"),
         Parameter2 = replace(Parameter2, Parameter2 == "Empan_MDT", "Empan")) %>%
  plot() +
  geom_edge_arc(strength = 0.1,
                aes(edge_alpha = abs(r),
                    edge_width = abs(r),
                    colour = r)) +
  guides(edge_alpha = "none", edge_width = "none") +
  scale_edge_colour_gradientn(limits = c(-1, 1), 
                              colors = c("firebrick2", "dodgerblue2")) +
  geom_node_point(size = 20) +
  geom_node_text(aes(label = name,
                     family = "serif"),
                 colour = "white",
                 repel = FALSE) +
  theme_graph(base_family = "serif", base_size = 10)

# ---- mfa_graph ---------------------------------------------------------------
# check_factorstructure(datascores)
# n_factors(datascores, rotation="cluster") 

datascores %>% 
  prcomp(scale = TRUE) %>% 
  fviz_pca_var(repel = TRUE,
               col.var = "contrib",
               title = "Analyse Factorielle Multiple des variables",
  ) + 
  theme_bw(base_size = 14, base_family = "serif") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(x = "Dimension 1 (43.2%)",
       y = "Dimension 2 (34.4%)")

# ---- loadings_graph ----------------------------------------------------------
factor_analysis(datascores, 
                rotation = "cluster",
                n = 3,
                sort = TRUE,
                standardize = TRUE) %>% 
  plot() + labs(title=NULL)

# Rotated loadings from factor analysis

# ---- loadings_tbl ------------------------------------------------------------
factor_analysis(datascores, 
                rotation = "cluster",
                n = 3,
                sort = TRUE,
                standardize = TRUE,) %>% 
  summary() %>%
  knitr::kable(caption = "Analyses des performances en termes de variance expliquée des facteurs détérminés selon une rotation adaptée à une analyse de clusters.\\label{loadings_tbl}",
               digits = 3)

# ---- loadings_graph_annex ----------------------------------------------------------
factor_analysis(datascores, 
                rotation = "cluster",
                n = 4,
                sort = TRUE,
                standardize = TRUE) %>% 
  plot() + labs(title=NULL)

# Rotated loadings from factor analysis

# ---- kmeans_plot -------------------------------------------------------------

kmeans(datascores %>% mutate(across(everything(), ~ scale(.x))), 
       centers = 4,
       nstart = 100) %>%
  fviz_cluster(
    datascores,
    geom = "point",
    repel = TRUE,
    ellipse.type = "convex",
    shape = "circle", pointsize = 1.2,
    xlab = "Dimension 1 (40.2%)",
    ylab = "Dimension 2 (18.7%)",
  ) +
  theme_bw(base_size = 14, base_family = "serif") +
  labs(title = NULL)

#"Représentation des clusters selon les deux composantes principales"

# ---- radar -------------------------------------------------------------------
p <- data_latent %>%
  group_by(cluster) %>%  
  summarise(across(everything(),mean)) %>% 
  mutate(across(-cluster, ~ rescale(.x, to = c(0,1))),) %>% 
  ggradar(base.size = 10,
          font.radar = "serif",
          values.radar = c("0","0.5","1"),
          grid.label.size = 4,
          grid.min = 0, grid.mid = .5, grid.max = 1,
          label.gridline.min = FALSE,
          group.line.width = 1, group.point.size = 3,
          group.colours =,
          background.circle.transparency = .1,
          legend.title = "Clusters",
          legend.text.size = 8,
          axis.label.size = 4,
          legend.position = "bottom",
          fill = TRUE,
          fill.alpha = 0.1
  ) + 
  theme_bw(base_size = 14, base_family = "serif") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

facet(p,facet.by = "cluster")

# Profils cognitifs des clusters identifies par partition non-supervisee (*hierachical k-means*)"

# ---- repartition ------------------------------------------------------
data_latent %>% 
  group_by(cluster) %>% 
  summarise(across(everything(),mean)) %>% 
  rename(Cluster = cluster) %>% 
  mutate(`Aphantasiques` = c(29,42,126,0) ,
         `Non-Aphantasiques` = c(49,73,0,81)) %>% 
  knitr::kable(
    caption =
      "Moyennes évaluées à chaque compétence et répartion des effectifs par cluster.\\label{repartition}",
    row.names = FALSE
  )

# ---- lollipop ----------------------------------------------------------------
data_latent %>% 
  mutate(across(-cluster, ~ rescale(.x, to = c(0,1))),) %>% 
  gather(key = variable, value = value, -cluster) %>% 
  group_by(variable, cluster) %>% 
  summarise(mean = mean(value)) %>% 
  ggdotchart(
    x = "cluster",
    y = "mean",
    group = "variable",
    color = "variable", size = 1, dot.size = 3,
    palette = "aas",
    add = "segment",
    position = position_dodge(.5),
    sorting = "descending",
    #facet.by = "cluster",
    rotate = TRUE,
    #legend = "none",
    ggtheme = theme_bw(base_size = 14, base_family = "serif"),
    xlab = "Cluster",
    ylab = "Moyennes",
    # title =
    #   "Scores aux differentes fonctions cognitives en fonction des clusters"
  ) +
  # geom_smooth(aes(group = cluster, color = cluster),size = .8) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
  )

# Représentation des moyennes de chaque cluster pour les trois capacités cognitives

# ---- analyses pour clusters
# data_latent %>% check_clusterstructure()
# data_latent %>% n_clusters() %>% plot()
# cluster_analysis(datascores, n = 2, method = "hkmeans")
# cluster_analysis(data_latent, n = 4, method = "hkmeans")

# # ---- modélisation et tests
# model1c <- lm(Raisonnement ~ 0 + cluster,data_latent)
# model2c <- lm(`Imagerie Objet` ~ 0 + cluster,data_latent)
# model3c <- lm(`Imagerie Spatiale` ~ 0 + cluster,data_latent)
# 
# model1c %>% check_model()
# model1c %>% report()
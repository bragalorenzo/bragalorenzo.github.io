# Libraries ####

library(compositions)
library(tidyverse)
library(factoextra)

# Dati ####

## Iris

data(iris)

## Pazienti (AI-generated)

patients <- data.frame(
    ID = paste0("P", sprintf("%02d", 1:30)),
    Sex = c(
        "F", "M", "F", "M", "F", "M", "M", "F", "F", "M",
        "F", "M", "F", "M", "F", "M", "F", "M", "F", "M",
        "F", "M", "F", "M", "F", "M", "F", "M", "F", "M"
    ),
    Age = c(
        24, 31, 45, 52, 61, 36, 71, 39, 58, 47,
        29, 63, 55, 42, 68, 34, 49, 74, 28, 53,
        37, 66, 41, 59, 33, 48, 57, 69, 44, 51
    ),
    Weight_kg = c(
        56, 73, 68, 85, 70, 77, 92, 64, 81, 75,
        59, 89, 72, 80, 67, 78, 62, 94, 58, 82,
        63, 88, 70, 86, 60, 79, 71, 91, 66, 83
    ),
    SystolicBP = c(
        110, 122, 118, 140, 128, 126, 168, 116, 148, 132,
        112, 158, 136, 130, 144, 124, 120, 170, 114, 146,
        118, 152, 126, 150, 115, 134, 140, 166, 122, 138
    ),
    Cholesterol = c(
        165, 190, 175, 220, 205, 198, 255, 180, 230, 200,
        170, 245, 210, 195, 225, 185, 178, 260, 168, 232,
        180, 240, 192, 228, 172, 205, 214, 250, 188, 215
    ),
    Glucose = c(
        82, 95, 89, 102, 97, 100, 115, 90, 108, 101,
        84, 112, 98, 96, 105, 92, 88, 118, 87, 110,
        90, 113, 94, 107, 85, 99, 103, 117, 92, 104
    ),
    WBC = c(
        4.6, 5.4, 5.1, 6.3, 5.8, 6.0, 7.6, 5.2, 6.7, 5.9,
        4.9, 7.1, 6.0, 5.6, 6.4, 5.3, 5.0, 8.1, 4.8, 6.8,
        5.1, 7.0, 5.4, 6.6, 4.9, 5.7, 6.1, 7.5, 5.2, 6.2
    )
)

## Leucociti

data(Blood23)

## Vongole

data(ClamWest)

## Boxite

data(Boxite)

Boxite <- as.data.frame(Boxite)

Boxite$depth_bin <- as.factor(case_when(
    Boxite$depth <= 5 ~ "surface",
    Boxite$depth > 5 & Boxite$depth <= 15 ~ "mid",
    Boxite$depth > 15 ~ "deep"
))

# PCA Iris ####

iris_pca <- prcomp(iris[, 1:4],
    center = TRUE, scale. = TRUE
)

## Screeplot
svg("pca_screeplot_iris.svg")
factoextra::fviz_eig(iris_pca,
    barfill = "seagreen",
    barcolor = "black",
    linecolor = "magenta",
    addlabels = TRUE
) +
    ggtitle("Iris - Screeplot") +
    theme_classic() +
    ylab("Percentuale di varianza spiegata (eigenvalue)") +
    xlab("Componente principale")
dev.off()

## Varianza spiegata da ogni PC
iris_var_comp <- iris_pca$sdev^2
iris_prop_var <- iris_var_comp / sum(iris_var_comp)
iris_percent_var <- iris_prop_var * 100

## Biplot
svg("pca_biplot_iris.svg")
factoextra::fviz_pca_biplot(iris_pca,
    geom.ind = "point",
    pointshape = 21,
    pointsize = 2,
    fill.ind = iris$Species,
    col.ind = iris$Species,
    palette = c("seagreen", "magenta", "brown"),
    addEllipses = TRUE,
    ellipse.palette = c("seagreen", "magenta"),
    label = "var",
    col.var = "blue",
    repel = TRUE
) +
    ggtitle("Iris - PC1 vs PC2") +
    theme_classic() +
    theme(legend.position = "none") +
    xlab(paste0("PC1 - ", round(iris_percent_var[1], 2), "%")) +
    ylab(paste0("PC2 - ", round(iris_percent_var[2], 2), "%"))
dev.off()

# PCA Pazienti ####

patients_pca <- prcomp(patients[, c("Age", "Weight_kg", "SystolicBP", "Cholesterol", "Glucose", "WBC")],
    center = TRUE, scale. = TRUE
)

## Screeplot
svg("pca_screeplot_pazienti.svg")
factoextra::fviz_eig(patients_pca,
    barfill = "seagreen",
    barcolor = "black",
    linecolor = "magenta",
    addlabels = TRUE
) +
    ggtitle("Pazienti - Screeplot") +
    theme_classic() +
    ylab("Percentuale di varianza spiegata (eigenvalue)") +
    xlab("Componente principale")
dev.off()

## Varianza spiegata da ogni PC
patients_var_comp <- patients_pca$sdev^2
patients_prop_var <- patients_var_comp / sum(patients_var_comp)
patients_percent_var <- patients_prop_var * 100

## Biplot
svg("pca_biplot_pazienti.svg")
factoextra::fviz_pca_biplot(patients_pca,
    geom.ind = "point",
    pointshape = 21,
    pointsize = 2,
    fill.ind = patients$Sex,
    col.ind = patients$Sex,
    palette = c("seagreen", "magenta"),
    addEllipses = TRUE,
    ellipse.palette = c("seagreen", "magenta"),
    label = "var",
    col.var = "blue",
    repel = TRUE
) +
    ggtitle("Pazienti - PC1 vs PC2") +
    theme_classic() +
    guides(fill = "none") +
    xlab(paste0("PC1 - ", round(patients_percent_var[1], 2), "%")) +
    ylab(paste0("PC2 - ", round(patients_percent_var[2], 2), "%"))
dev.off()

# PCA Leucociti ####

leuko_clr <- clr(Blood23)

leuko_pca <- prcomp(leuko_clr,
    center = TRUE, scale. = FALSE
)

## Screeplot
svg("pca_screeplot_leuko.svg")
factoextra::fviz_eig(leuko_pca,
    barfill = "seagreen",
    barcolor = "black",
    linecolor = "magenta",
    addlabels = TRUE
) +
    ggtitle("Leucociti - Screeplot") +
    theme_classic() +
    ylab("Percentuale di varianza spiegata (eigenvalue)") +
    xlab("Componente principale")
dev.off()

## Varianza spiegata da ogni PC
leuko_var_comp <- leuko_pca$sdev^2
leuko_prop_var <- leuko_var_comp / sum(leuko_var_comp)
leuko_percent_var <- leuko_prop_var * 100

## Biplot Leucociti
svg("pca_biplot_leuko.svg")
factoextra::fviz_pca_biplot(leuko_pca,
    geom.ind = c("point", "text"),
    pointshape = 21,
    pointsize = 2,
    col.ind = "seagreen",
    fill.ind = "seagreen",
    addEllipses = TRUE,
    ellipse.palette = "seagreen",
    label = "all",
    col.var = "blue",
    repel = TRUE
) +
    ggtitle("Leucociti - PC1 vs PC2") +
    theme_classic() +
    theme(legend.position = "none") +
    xlab(paste0("PC1 - ", round(leuko_percent_var[1], 2), "%")) +
    ylab(paste0("PC2 - ", round(leuko_percent_var[2], 2), "%"))
dev.off()

# PCA Vongole ####

clams_clr <- clr(ClamWest)

clams_pca <- prcomp(clams_clr,
    center = TRUE, scale. = FALSE
)

## Screeplot
svg("pca_screeplot_clams.svg")
factoextra::fviz_eig(clams_pca,
    barfill = "seagreen",
    barcolor = "black",
    linecolor = "magenta",
    addlabels = TRUE
) +
    ggtitle("Vongole - Screeplot") +
    theme_classic() +
    ylab("Percentuale di varianza spiegata (eigenvalue)") +
    xlab("Componente principale")
dev.off()

## Varianza spiegata da ogni PC
clams_var_comp <- clams_pca$sdev^2
clams_prop_var <- clams_var_comp / sum(clams_var_comp)
clams_percent_var <- clams_prop_var * 100

## Biplot Vongole PC1 vs PC2

svg("pca_biplot_clams_pc1_pc2.svg")
factoextra::fviz_pca_biplot(clams_pca,
    geom.ind = c("point", "text"),
    pointshape = 21,
    pointsize = 2,
    col.ind = "seagreen",
    fill.ind = "seagreen",
    addEllipses = TRUE,
    ellipse.palette = "seagreen",
    label = "all",
    col.var = "blue",
    repel = TRUE
) +
    ggtitle("Vongole - PC1 vs PC2") +
    theme_classic() +
    theme(legend.position = "none") +
    xlab(paste0("PC1 - ", round(clams_percent_var[1], 2), "%")) +
    ylab(paste0("PC2 - ", round(clams_percent_var[2], 2), "%"))
dev.off()

## Biplot Vongole PC1 vs PC3

svg("pca_biplot_clams_pc1_pc3.svg")
factoextra::fviz_pca_biplot(clams_pca,
    axes = c(1, 3),
    geom.ind = c("point", "text"),
    pointshape = 21,
    pointsize = 2,
    col.ind = "seagreen",
    fill.ind = "seagreen",
    addEllipses = TRUE,
    ellipse.palette = "seagreen",
    label = "all",
    col.var = "blue",
    repel = TRUE
) +
    ggtitle("Vongole - PC1 vs PC3") +
    theme_classic() +
    theme(legend.position = "none") +
    xlab(paste0("PC1 - ", round(clams_percent_var[1], 2), "%")) +
    ylab(paste0("PC3 - ", round(clams_percent_var[3], 2), "%"))
dev.off()

# PCA Boxite ####

boxite_clr <- clr(Boxite[, 1:5])

boxite_pca <- prcomp(boxite_clr,
    center = TRUE, scale. = FALSE
)

## Screeplot
svg("pca_screeplot_boxite.svg")
factoextra::fviz_eig(boxite_pca,
    barfill = "seagreen",
    barcolor = "black",
    linecolor = "magenta",
    addlabels = TRUE
) +
    ggtitle("Boxite - Screeplot") +
    theme_classic() +
    ylab("Percentuale di varianza spiegata (eigenvalue)") +
    xlab("Componente principale")
dev.off()

## Varianza spiegata da ogni PC
boxite_var_comp <- boxite_pca$sdev^2
boxite_prop_var <- boxite_var_comp / sum(boxite_var_comp)
boxite_percent_var <- boxite_prop_var * 100

## Biplot Boxite PC1 vs PC2

svg("pca_biplot_boxite_pc1_pc2.svg")
factoextra::fviz_pca_biplot(boxite_pca,
    geom.ind = "point",
    pointshape = 21,
    pointsize = 2,
    fill.ind = Boxite$depth_bin,
    col.ind = Boxite$depth_bin,
    palette = c("seagreen", "magenta", "brown"),
    label = "var",
    col.var = "blue",
    repel = TRUE
) +
    ggtitle("Boxite - PC1 vs PC2") +
    theme_classic() +
    guides(fill = "none") +
    xlab(paste0("PC1 - ", round(boxite_percent_var[1], 2), "%")) +
    ylab(paste0("PC2 - ", round(boxite_percent_var[2], 2), "%"))
dev.off()

## Biplot Boxite PC1 vs PC3

svg("pca_biplot_boxite_pc1_pc3.svg")
factoextra::fviz_pca_biplot(boxite_pca,
    axes = c(1, 3),
    geom.ind = "point",
    pointshape = 21,
    pointsize = 2,
    fill.ind = Boxite$depth_bin,
    col.ind = Boxite$depth_bin,
    palette = c("seagreen", "magenta", "brown"),
    label = "var",
    col.var = "blue",
    repel = TRUE
) +
    ggtitle("Boxite - PC1 vs PC3") +
    theme_classic() +
    guides(fill = "none") +
    xlab(paste0("PC1 - ", round(boxite_percent_var[1], 2), "%")) +
    ylab(paste0("PC3 - ", round(boxite_percent_var[3], 2), "%"))
dev.off()

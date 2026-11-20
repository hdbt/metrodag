# The six annotated gallery DAGs. Shared by the geometry snapshot test and
# data-raw/annotated_gallery.R, which sources this file from the package root.
# Edges, labels, roles, ranks and alignment groups stay fixed across releases.

.gallery_edge_list <- function(text) {
  matrix(strsplit(text, "[[:space:]]+")[[1L]], ncol = 2L, byrow = TRUE,
    dimnames = list(NULL, c("from", "to")))
}

.metro_gallery_examples <- list(
  list(
    id = "01_alcohol_heart", title = "Alcohol and coronary heart disease",
    structure = "Parallel mediators and three observed common causes",
    inspect = "Follow both mediator paths; inspect the long arrows from income, health and smoking.",
    edges = .gallery_edge_list("I X I Y H X H Y D X D S X M M Y X S S Y X Y"),
    labels = c(I = "Household income", H = "Baseline health",
      D = "Smoking at baseline", X = "Usual alcohol use",
      M = "Systolic BP rise", S = "Blood triglycerides", Y = "Coronary events"),
    exposure = "X", outcome = "Y", adjusted = c("I", "H", "D")
  ),
  list(
    id = "02_air_pollution", title = "Air pollution and asthma attacks",
    structure = "Long bypass arrows and two routes into respiratory infections",
    inspect = "Trace exposure through airway swelling and infections, and compare the separate lung-function path.",
    edges = .gallery_edge_list("I N I Y R N R X N X B N B Y X L L P X P P Y X T T Y"),
    labels = c(I = "Household income", N = "Indoor air pollution",
      R = "Distance to main road", B = "Household gas cooking",
      X = "PM2.5 exposure", L = "Airway swelling", P = "Respiratory infections",
      T = "Lung function (FEV1)", Y = "Asthma attacks"),
    exposure = "X", outcome = "Y", adjusted = c("I", "B", "R")
  ),
  list(
    id = "03_sugary_drinks", title = "Sugary drinks, body fat and HbA1c",
    structure = "Several converging parents and a latent common genotype",
    inspect = "Inspect the crowded body-fat junction and the dashed arrows from metabolic genotype.",
    edges = .gallery_edge_list("W O W Y O Y A O S O C A C S C D D Y U O U Y H O H Y"),
    labels = c(W = "Sugary drinks", O = "Body fat mass", Y = "HbA1c level",
      A = "Food environment", S = "Fast-food consumption", C = "Household income",
      D = "Primary care", U = "Metabolic genotype", H = "Shift-work hours"),
    exposure = "W", outcome = "Y", unobserved = "U", unadjusted = TRUE
  ),
  list(
    id = "04_statin_events", title = "Statin therapy and vascular events",
    structure = "A longer mediation chain with healthy-user confounding",
    inspect = "Follow the therapy-to-pills-to-LDL chain; inspect long bypasses, dashed confounding and arrow attachments.",
    edges = .gallery_edge_list("X P P A A O O Y I X I R I Y R A T A S O U X U O G P G O G Y"),
    labels = c(X = "Statin therapy given", P = "Pills collected",
      A = "Serum LDL cholesterol", O = "Arterial plaques", Y = "Vascular events",
      I = "Household income", R = "Comorbidity profile", T = "Dietary fat level",
      S = "Current smoking", U = "Health consciousness", G = "Age at baseline visit"),
    exposure = "X", outcome = "Y", adjusted = c("I", "R", "G"), unobserved = "U"
  ),
  list(
    id = "05_obesity_admission", title = "Obesity, hospital admission and heart failure",
    structure = "Dense pathways, latent confounding and a downstream collider",
    inspect = "Check whether the heart-failure outcome remains prominent even though admission is farther downstream.",
    edges = .gallery_edge_list("E X E Y E I X I I Y X M M Y X Y X P Y P U X U Y"),
    labels = c(E = "Educational attainment", X = "Obesity class",
      I = "Diabetes", M = "Hypertension", Y = "Heart failure onset",
      P = "Hospital admission", U = "Underlying frailty"),
    exposure = "X", outcome = "Y", adjusted = "E", unobserved = "U"
  ),
  list(
    id = "06_longitudinal", title = "Blood pressure and creatinine across three waves",
    structure = "Fixed wave ranks, cross-lagged arrows and a long mediated path",
    inspect = "Compare the two aligned repeated-measure chains. Wave ranks and alignment groups are supplied; the algorithm chooses their spacing and routes.",
    edges = .gallery_edge_list("S X1 S Y1 X1 X2 X2 X3 Y1 Y2 Y2 Y3 X1 Y2 Y1 X2 X2 Y3 Y2 X3 X1 M2 M2 Y3 C X2 C Y2"),
    labels = c(S = "Household income", C = "Diabetes duration",
      X1 = "Systolic pressure: wave 1", X2 = "Systolic pressure: wave 2",
      X3 = "Systolic pressure: wave 3", Y1 = "Creatinine: wave 1",
      Y2 = "Creatinine: wave 2", Y3 = "Creatinine: wave 3",
      M2 = "Diuretic exposure: wave 2"),
    exposure = "X2", outcome = "Y3", adjusted = c("S", "C", "X1", "Y1"),
    rank = c(S = 0, C = 0, X1 = 1, Y1 = 1, X2 = 2, Y2 = 2, M2 = 2, X3 = 3, Y3 = 3),
    align = list(c("X1", "X2", "X3"), c("Y1", "Y2", "Y3"))
  )
)

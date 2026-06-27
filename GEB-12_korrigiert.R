# ============================================================
# GEB-12 / GEB-50 — Statistische Auswertung
# Klassische Testtheorie (KTT) + Item-Response-Theorie (IRT)
# ============================================================

# Prepare data ####

# Load packages
librarian::shelf(psych, eRm, car, labelled, dplyr, varhandle,
                 effectsize, lavaan)

# Set working directory
setwd('C:/Users/jedno/Dropbox/M.Sc. Psychologie/B Psychologische Diagnostik/B.2 Testheorie und -konstruktion (SE,Ü)/GEB-50')

## Load data frames ####
Data_S_raw <- read.csv("Data_S.csv", header = TRUE, sep = ";",
                       check.names = FALSE, fileEncoding = "UCS-2LE")

Data_L_raw <- read.csv("Data_L.csv", header = TRUE, sep = ";", check.names = FALSE,
                       fileEncoding = "UCS-2LE")

# Select cases (Ausschluss von Test-/Abbruchfällen)
Data_S_raw <- Data_S_raw[Data_S_raw$CASE != 28 & Data_S_raw$CASE != 29
                     & Data_S_raw$CASE != 30 & Data_S_raw$CASE != 31
                     & Data_S_raw$CASE != 42 & Data_S_raw$CASE != 94, ]

Data_L_raw <- Data_L_raw[Data_L_raw$CASE != 130 & Data_L_raw$CASE != 153
                         & Data_L_raw$CASE != 155 & Data_L_raw$CASE != 156
                         & Data_L_raw$CASE != 159 & Data_L_raw$CASE != 164
                         & Data_L_raw$CASE != 173 & Data_L_raw$CASE != 175
                         & Data_L_raw$CASE != 182 & Data_L_raw$CASE != 196, ]

## Short version ####

# Select columns
Data_S <- Data_S_raw %>% select(KG01_01, KG01_02, KG01_03, KG01_04, KG01_05,
                                KG01_06, KG01_07, KG01_08, KG01_09, KG01_10,
                                KG01_11, KG01_12)

# Check structure
str(Data_S)

# Recode (-1 = fehlend -> 0, 2 = "Nein" -> 0, 1 = "Ja" -> 1)
Data_S <- as.data.frame(sapply(Data_S[c(1:12)], car::recode, "-1=0; 2=0; 1=1"))

# Recode inverse item (Spalte 10 = KG01_10, negativ kodiert)
# HINWEIS: Folie 9 weist 3 negativ kodierte Items aus
# (Abfall-leicht, Konsum-leicht, Recycling-schwer). Hier wird laut
# Absprache nur KG01_10 umgepolt. Bitte mit den Rohdaten gegenprüfen,
# ob die beiden anderen Items bereits positiv abgefragt wurden.
Data_S[c(10)] <- as.data.frame(sapply(Data_S[c(10)], car::recode, "0=1; 1=0"))

# Check structure again
str(Data_S)

## Long version ####
Data_L <- unfactor(Data_L_raw[7:56])

# Inverse dichotomous items
Data_L[c(34,35,36,37,38,40)] <- as.data.frame(sapply(Data_L[c(34,35,36,37,38,40)],
                                                     car::recode, "1=2; 2=1"))

# Recode dichotomous items
Data_L[c(33:50)] <- as.data.frame(sapply(Data_L[c(33:50)], car::recode,
                                         "1=1; 2=0"))

# Inverse polytomous items
Data_L[c(3,4,6,7,10,17,18,23,26,27,28,29,31)] <- as.data.frame(
  sapply(Data_L[c(3,4,6,7,10,17,18,23,26,27,28,29,31)],
         car::recode,  "1=5; 2=4; 3=3; 4=2; 5=1"))

# Dichotomize polytomous items (>=4 -> 1, sonst 0)
Data_L[c(1:32)] <- as.data.frame(sapply(Data_L[c(1:32)], car::recode,
                                        "1=0; 2=0; 3=0; 4=1; 5=1"))

# Display classes of each column
sapply(Data_L, class)

# Data type -> numeric
i <- c(1:50)
Data_L[, i] <- apply(Data_L[, i], 2, function(x) as.numeric(as.character(x)))

# Check again
sapply(Data_L, class)

# ============================================================
# Statistical analysis ####
# ============================================================

## Klassische Testtheorie ####

### Mean, standard deviation, median ####
Data_S_Mean <- rowMeans(Data_S, na.rm = TRUE)
psych::describe(Data_S_Mean)

Data_L_Mean <- rowMeans(Data_L, na.rm = TRUE)
psych::describe(Data_L_Mean)

### Reliability ####

#### Cronbachs Alpha ####
psych::alpha(Data_S, check.keys = TRUE)
psych::alpha(Data_L, check.keys = TRUE)

#### McDonalds Omega ####
# nfactors = 6 entspricht den sechs Handlungsbereichen
psych::omega(Data_S, nfactors = 6)
psych::omega(Data_L, nfactors = 6)

#### Split half reliability ####
splitHalf(Data_S)
splitHalf(Data_L)

### Bravais-Pearson / Spearman correlation ####
Sum_L <- rowSums(Data_L, na.rm = TRUE)
Sum_S <- rowSums(Data_S, na.rm = TRUE)

cor(Sum_L, Sum_S, method = 'spearman')

## Item-Response-Theorie ####

### CFA ####

# Sechs-Faktoren-Modell (je Handlungsbereich ein Faktor mit 2 Items)
CFA_model <- '
Energiesparen   =~ KG01_01 + KG01_02
Mobilitaet      =~ KG01_03 + KG01_04
Abfallvermeiden =~ KG01_05 + KG01_06
Konsum          =~ KG01_07 + KG01_08
Recycling       =~ KG01_09 + KG01_10
Engagement      =~ KG01_11 + KG01_12
'

Items_S <- c("KG01_01", "KG01_02",
             "KG01_03", "KG01_04",
             "KG01_05", "KG01_06",
             "KG01_07", "KG01_08",
             "KG01_09", "KG01_10",
             "KG01_11", "KG01_12")

# WLSMV-Schätzer für dichotome (ordinale) Items
# HINWEIS: 2-Indikator-Faktoren sind einzeln nicht identifiziert;
# das Modell ist nur über die Faktorkovarianzen identifiziert und
# kann bei kleinem N Schätzprobleme/Warnungen erzeugen.
Fit <- lavaan::cfa(CFA_model, data = Data_S,
                   ordered = Items_S, estimator = "WLSMV")

summary(Fit, fit.measures = TRUE, standardized = TRUE)

### Rasch model ####

# ------------------------------------------------------------
# Robuste Vorbereitung für eRm::RM()
# RM() bricht ab, wenn Items konstant sind (keine Varianz),
# wenn NAs/uneindeutige Werte vorliegen oder wenn eine Person
# einen perfekten Score (alle 0 / alle 1) hat. Wir prüfen das
# explizit und bereinigen, bevor das Modell geschätzt wird.
# ------------------------------------------------------------

# Auf Matrix umstellen und sicher numerisch machen
Rasch_data <- as.matrix(Data_S)
storage.mode(Rasch_data) <- "numeric"

# 1) Nur erlaubte Werte 0/1 (alles andere -> NA)
Rasch_data[!(Rasch_data %in% c(0, 1))] <- NA

# 2) Konstante Items (alle gleich) identifizieren und ausschließen
item_var  <- apply(Rasch_data, 2, var, na.rm = TRUE)
const_items <- which(is.na(item_var) | item_var == 0)
if (length(const_items) > 0) {
  message("Konstante Items ausgeschlossen: ",
          paste(colnames(Rasch_data)[const_items], collapse = ", "))
  Rasch_data <- Rasch_data[, -const_items, drop = FALSE]
}

# 3) Personen mit fehlenden Werten oder perfektem Score entfernen
#    (extreme raw scores liefern keine endlichen Personenparameter)
row_scores <- rowSums(Rasch_data, na.rm = TRUE)
k_items    <- ncol(Rasch_data)
drop_rows  <- which(
  rowSums(is.na(Rasch_data)) > 0 |   # fehlende Werte
  row_scores == 0 |                  # alle 0
  row_scores == k_items              # alle 1
)
if (length(drop_rows) > 0) {
  message(length(drop_rows),
          " Personen mit fehlenden/extremen Scores entfernt.")
  Rasch_data <- Rasch_data[-drop_rows, , drop = FALSE]
}

# 4) Rasch-Modell schätzen (nur wenn genug Daten übrig sind)
if (nrow(Rasch_data) >= 10 && ncol(Rasch_data) >= 2) {
  Rasch <- eRm::RM(Rasch_data)
  print(summary(Rasch))           # Itemparameter (Schwierigkeiten)
  print(coef(Rasch))              # Schätzwerte

  #### Person parameters ####
  Person <- eRm::person.parameter(Rasch)
  print(Person)

  #### Item fit (Infit/Outfit) ####
  print(eRm::itemfit(Person))

  #### Separation reliability ####
  print(eRm::SepRel(Person))

  #### Wright map (Personen- vs. Itemverteilung) ####
  eRm::plotPImap(Rasch, sorted = TRUE)

} else {
  warning("Zu wenige gültige Fälle/Items für ein stabiles Rasch-Modell. ",
          "Nach Ausschluss: ", nrow(Rasch_data), " Personen, ",
          ncol(Rasch_data), " Items.")
}

# ------------------------------------------------------------
# Hinweis kleine Stichprobe:
# Bei N ~ 50 und 12 dichotomen Items sind CML-Schätzungen mit
# breiten Konfidenzintervallen behaftet. Für robustere Aussagen
# können nichtparametrische Alternativen geprüft werden:
#   eRm::NPtest(Rasch_data, n = 1000, method = "T11")
# ------------------------------------------------------------

library(dplyr)
library(car)
library(tidyr)
library(glmmTMB)
library(DHARMa)
library(ggplot2)
library(multcomp)
library(scales)  # für pseudo_log_trans

#Rohdaten einladen
DATA<-read.csv("csv/DM31-35_PentenKonzreihe.csv",header = T)
getwd()

#Spalte Animal hinzufügen, Traces anhand des Dateinamens richtigem Animal zuordnen
DATA$Animal<-NA
for (i in 1:length(DATA$Area)) {
  DATA$Animal[i]<-strsplit(DATA$File[i],"-")[[1]][1]
}
#Anzahl der Tiere in Animals speichern
Animals<-unique(DATA$Animal)

#Noisekorrektur der Peakheight
DATA$H_Nhalf = DATA$Height - DATA$Noise/2
table(DATA$File) #each file has 5 peaks

#wird später für Korrektur um Pentenhöhe verwendet
Pentenheight<-NULL
DATA$CorrH<-NA

#für jedes Tier den Median der ersten 5 Peaks in Pentenheight speichern
for (i in 1:length(Animals)){
  Pentenheight<-c(Pentenheight,median(DATA$Height[which(DATA$Animal==Animals[i])][1:5]))
}
#median aller Pentenheights in CorrH speichern
CorrH<-median(Pentenheight)

#for each animal: median of the first 5 heights, assign a correction factor = 1 / median, multiply heights by that correction factor
for (i in 1:length(Animals)){
DATA$CorrH[which(DATA$Animal==Animals[i])] <-CorrH/median(DATA$Height[which(DATA$Animal==Animals[i])][1:5])
}
DATA$CorrHeights<-DATA$Height*DATA$CorrH

# create DATA2: für jedes Treatment die Mediane bei jedem Animal speichern
DATA2 = DATA %>% group_by(File) %>% summarise(median = median(H_Nhalf), medianHeight = median(Height), Treatment = Treatment[1], Animal = Animal[1],medianSignaToNoise=median(SignalToNoise), Concentration = Concentration [1])
#DATA2 = DATA %>% group_by(File) %>% summarise(median = median(H_Nhalf), medianHeight = median(Height), Treatment = Treatment, Animal = Animal,medianSignaToNoise=median(SignalToNoise))


# Extract the first medianHeight value for each animal
first_median_heights <- sapply(Animals, function(animal) {
  DATA2$medianHeight[which(DATA2$Animal == animal)][1]
 })

 # Compute CorrH using the median of these first values
 DATA2$CorrH <- median(first_median_heights)

 # Compute correction factor for each animal
 for (i in 1:length(Animals)) {
   DATA2$CorrH[which(DATA2$Animal == Animals[i])] <- CorrH / DATA2$medianHeight[which(DATA2$Animal == Animals[i])][1]
 }

# Apply the correction to medianHeight
DATA2$CorrHeights <- DATA2$medianHeight * DATA2$CorrH

#überschreibe CorrH mit dem median aus medianheight --> um Referenzwert für gesamten Datensatz zu erlangen
CorrH<-median(DATA2$medianHeight)
for (i in 1:length(Animals)){
  DATA2$CorrH[which(DATA2$Animal==Animals[i])] <-CorrH/DATA2$medianHeight[which(DATA2$Animal==Animals[i])][1]
}
DATA2$CorrHeights<-DATA2$medianHeight*DATA2$CorrH

#extract the number of each trace per animal
DATA2$FileNr <-NA
for(i in 1:nrow(DATA2)){
  DATA2$FileNr[i]<-strsplit(DATA2$File[i],"-")[[1]][2]
  DATA2$FileNr[i]<-strsplit(DATA2$FileNr[i],".A")[[1]][1]
}
DATA2$FileNr <-as.numeric(DATA2$FileNr)

#bring treatments in correct order
DATA2$Treatment <- factor(DATA2$Treatment,
                          levels = c("1-penten-3ol 100µg/µl 1", "milliQ", "1-penten-3ol 1µg/µl", "1-penten-3ol 5µg/µl", "1-penten-3ol 10µg/µl", "1-penten-3ol 20µg/µl", "1-penten-3ol 40µg/µl", "1-penten-3ol 60µg/µl", "1-penten-3ol 80µg/µl", "1-penten-3ol 100µg/µl 2","1-penten-3ol 100µg/µl 3", "1-penten-3ol 10µg/µl 2","1-penten-3ol 20µg/µl 2", "milliQ 2"))


# sicherstellen, dass Treatment & Animal Faktoren sind
DATA2$Treatment <- as.factor(DATA2$Treatment)
DATA2$Animal    <- as.factor(DATA2$Animal)

#subset nur mit den 100µg/µl Daten
DATA2_subset <- subset(DATA2, Treatment %in% c("1-penten-3ol 100µg/µl 1", "1-penten-3ol 100µg/µl 2"))
plot(medianHeight ~ Treatment,data = DATA2_subset)
stripchart(medianHeight ~ Treatment,data = DATA2_subset,vertical=T,add=T,method="jitter")
var.test(medianHeight ~ Treatment,data = DATA2_subset)
# t-Test (gleiches Varianz angenommen) des subsets
t_test_result <- t.test(medianHeight ~ Treatment,
                        data = DATA2_subset,
                        var.equal = TRUE)

t_test_result


#Reihenfolge der Treatments für DATA anpassen
DATA$Treatment <- factor(DATA$Treatment,
                         levels = c("1-penten-3ol 100µg/µl 1", "milliQ", "1-penten-3ol 1µg/µl", "1-penten-3ol 5µg/µl", "1-penten-3ol 10µg/µl", "1-penten-3ol 20µg/µl", "1-penten-3ol 40µg/µl", "1-penten-3ol 60µg/µl", "1-penten-3ol 80µg/µl", "1-penten-3ol 100µg/µl 2","1-penten-3ol 100µg/µl 3", "1-penten-3ol 10µg/µl 2","1-penten-3ol 20µg/µl 2", "milliQ 2"))

boxplot(medianHeight~Treatment,data=DATA2,outline=F,ylim=c(-18000,0))
for (i in 1:length(unique(DATA2$Animal))){
stripchart(medianHeight~Treatment,data=DATA2,vertical=T,pch=16,method="jitter",
           add=T,  col = i,subset=DATA2$Animal==unique(DATA2$Animal)[i])
}

#boxblot with SignalToNoise data
boxplot(SignalToNoise~Treatment,data=DATA,outline=F,ylim=c(0,200))
for (i in 1:length(unique(DATA$Animal))){ 
  stripchart(SignalToNoise~Treatment,data=DATA,vertical=T,pch=16,method="jitter",
             add=T,  col = i,subset=DATA$Animal==unique(DATA$Animal)[i])
}

DATA2$Treatment <- factor(DATA2$Treatment,
                          levels = c("1-penten-3ol 100µg/µl 1", "milliQ", "1-penten-3ol 1µg/µl", "1-penten-3ol 5µg/µl", "1-penten-3ol 10µg/µl", "1-penten-3ol 20µg/µl", "1-penten-3ol 40µg/µl", "1-penten-3ol 60µg/µl", "1-penten-3ol 80µg/µl", "1-penten-3ol 100µg/µl 2","1-penten-3ol 100µg/µl 3", "1-penten-3ol 10µg/µl 2","1-penten-3ol 20µg/µl 2", "milliQ 2"))
DATA2<-DATA2[order(DATA2$Treatment),]

# subset nur mit Konzentrationen 0-100
DATAmod <- subset(DATA, Concentration != "X")

# 0 in SignalToNoise durch kleine Zahl ersetzen, damit glmmTMB funktioniert
DATAmod$SignalToNoise[DATAmod$SignalToNoise == 0] = 0.00000001

#DATA2$medianSignalToNoise[DATA2$SignalToNoise == 0] = 0.00000001


DATAmod$Concentration <- as.numeric(DATAmod$Concentration)
DATAmod$Animal <- factor(DATAmod$Animal)

# 1️⃣ Animal als Faktor
DATAmod$Animal <- factor(DATAmod$Animal)

# 2️⃣ Zentrierte Concentration
DATAmod$cConcentration <- scale(DATAmod$Concentration,
                                center = TRUE, scale = FALSE)

# 3️⃣ Kleine Verschiebung für 0-Werte
epsilon <- 1e-6
DATAmod$SignalToNoise2 <- DATAmod$SignalToNoise + epsilon

# 4️⃣ Modell fitten – Tweedie ohne p-Argument hier
glm_stable <- glmmTMB(
  SignalToNoise2 ~ cConcentration + I(cConcentration^2) + (1 | Animal/Concentration),
  data = DATAmod,
  family = tweedie(link = "log"),  # p wird intern geschätzt
  control = glmmTMBControl(
    optimizer = optim,
    optArgs = list(method="BFGS", maxit=10000)
  )
)

# 5️⃣ Ergebnis prüfen
summary(glm_stable)
resglm_stable <- simulateResiduals(glm_stable)
plot(resglm_stable)

#Wert von x (also Concentration) finden, bei dem die vorhergesagte Response = 3
# 1️⃣ Funktion für uniroot definieren
f <- function(x) {
  # cConcentration = x - Mittelwert von Concentration, weil zentriert
  x_centered <- x - mean(DATAmod$Concentration)
  
  predict(glm_stable,
          newdata = data.frame(cConcentration = x_centered),
          type = "response",
          re.form = NA) - 3
}

# 2️⃣ uniroot suchen
root <- uniroot(f, lower = 0, upper = 100)

# 3️⃣ Ergebnis
x_at_y3 <- root$root
x_at_y3

# Original Concentration-Werte
x_seq <- seq(min(DATAmod$Concentration), max(DATAmod$Concentration), length.out = 100)

# Für das Modell zentrieren
newdata <- data.frame(cConcentration = x_seq - mean(DATAmod$Concentration))

# Fixeffekte vorhersagen (Random Effects ausgeschlossen)
pred <- predict(glm_stable, newdata = newdata, type = "link", se.fit = TRUE, re.form = NA)

# Auf Response-Skala zurücktransformieren
fit <- exp(pred$fit)                # log-Link -> exponent
upper <- exp(pred$fit + 1.96*pred$se.fit)
lower <- exp(pred$fit - 1.96*pred$se.fit)

# newdata auf Response-Skala ergänzen
newdata$fit <- fit
newdata$upper <- upper
newdata$lower <- lower
newdata$Concentration <- x_seq  # Original Concentration


y_target <- 3

# schöne y-Ticks für sqrt-Skala (wie vorher)
y_min <- min(DATAmod$SignalToNoise2)
y_max <- max(DATAmod$SignalToNoise2)

y_breaks <- pretty(sqrt(c(y_min, y_max)), n = 6)^2

# Achsengrenzen (für saubere Linien bis zur Achse)
x_min <- min(DATAmod$Concentration)
y_min2 <- min(DATAmod$SignalToNoise2)
par(xpd = TRUE)

ggplot() +
  # Daten
  geom_point(
    data = DATAmod,
    aes(x = Concentration, y = SignalToNoise2),
    alpha = 0.5, color = "black"
  ) +
  # Vorhersage
  geom_line(
    data = newdata,
    aes(x = Concentration, y = fit),
    color = "blue", linewidth = 1.2
  ) +
  # Konfidenzband
  geom_ribbon(
    data = newdata,
    aes(x = Concentration, ymin = lower, ymax = upper),
    alpha = 0.2, fill = "blue"
  ) +
# horizontal bei y = 3
geom_segment(
  aes(x = 0, xend = x_at_y3,
      y = y_target, yend = y_target),
  colour = "red", linetype = "dashed", linewidth = 1
) +
  # vertikal bei x = x_at_y3
  geom_segment(
    aes(x = x_at_y3, xend = x_at_y3,
        y = y_min2, yend = y_target),
    colour = "red", linetype = "dashed", linewidth = 1
  ) +
  # -------------------------------
# Beschriftung an den Achsen
# -------------------------------
# y = 3 links an der y-Achse
annotate(
  "text",
  x = x_min,
  y = y_target,
  label = "3",
  colour = "red",
  hjust = 5, vjust = 0.5, size = 4
) +
  # x = x_at_y3 unten an der x-Achse
  annotate(
    "text",
    x = x_at_y3,
    y = y_min2,
    label = round(x_at_y3, 2),
    colour = "red",
    hjust = 0.5, vjust = 3, size = 4
  ) +
  # -------------------------------
# Achsen
# -------------------------------
scale_y_sqrt(
    breaks = y_breaks,
    limits = c(0, 200),
    expand = c(0, 0)
  ) +  
  scale_x_continuous(
    breaks = sort(unique(DATAmod$Concentration)),
    limits = c(-1, 105),
    expand = c(0, 0))+
  coord_cartesian(clip = "off") +
  theme_classic() +   # weißer Hintergrund, kein Gitternetz
  theme(
    axis.line = element_line(color = "black", size = 1.0),
    axis.ticks = element_line(color = "black", size = 1.0),
    # Tick-Beschriftungen
    axis.text.x = element_text(color = "black", size = 14),
    axis.text.y = element_text(color = "black", size = 14),# Achsen schwarz
    panel.border = element_blank(),              # kein Rahmen
    panel.grid = element_blank(),
    axis.title.x = element_text(size = 16, color = "black",
                                margin = margin(t = 12)),  # Abstand oben
    axis.title.y = element_text(size = 16, color = "black",
                                margin = margin(r = 12))  # Abstand rechts
  ) +
  labs(
    x = "1-penten-3ol [µg/µl]",
    y = "signal to noise (sqrt scale)",
    title = ""
  )

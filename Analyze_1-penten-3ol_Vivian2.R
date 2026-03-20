library(dplyr)
library(ggplot2)

DATA<-read.csv("csv/1-penten-3ol_Vivian.csv",header = T)
getwd()

#Spalte Animal hinzufügen, Traces anhand des Dateinamens richtigem Animal zuordnen
DATA$Animal<-NA
for (i in 1:length(DATA$Area)) {
  DATA$Animal[i]<-strsplit(DATA$File[i],"_")[[1]][1]
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
DATA2 = DATA %>% group_by(File) %>% summarise(median = median(H_Nhalf), medianHeight = median(Height), Treatment = Treatment[1], Animal = Animal[1])


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

DATA2$Treatment <- factor(DATA2$Treatment,
                          levels = c("milliQ", "1-penten-3ol 10µg/µl", "1-penten-3ol 50µg/µl", "1-penten-3ol 100µg/µl"))
  short_labels <- c("0", "10", "50", "100")

# create a mapping from Treatment to short labels
DATA2$Treatment <- factor(DATA2$Treatment, levels = unique(DATA2$Treatment))

names(short_labels) <- levels(DATA2$Treatment)

ggplot(DATA2, aes(x = Treatment, y = medianHeight)) +
  stat_summary( fun = median, geom = "crossbar", width = 0.5, color = "black") +
  geom_jitter(aes(color = Animal), width = 0.05, size = 3, show.legend = F) +  # points
  scale_color_manual(values = rainbow(length(unique(DATA2$Animal)))) +  # assign colors
  scale_x_discrete(labels = short_labels) +  # short labels for x-axis
  coord_cartesian(ylim = c(-35000, 1000)) +  # y-axis limits
  labs(
    x = "1-penten-3ol [µg/µl]",
    y = "median peak height [nV]",
    title = "",
    color = "Animal"
  ) +
  scale_color_manual(values = c("#08306B",  # sehr dunkles Navy-Blau
                                "#FFA500"))+  # gelbstichiges, helles Orange
  theme_classic() +
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
  )
ggsave(filename = "plots/1-penten-3ol_Vivian_ggplot2.png")

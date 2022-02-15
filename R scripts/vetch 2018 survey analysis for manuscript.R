# Clean R script for 2018 vetch manuscript 

# core code copied over on dec 28 2021

# packages #####
library("ggplot2")
library("xlsx")
library("mgcv")
library("tidymv")
library("tidyverse")
library("lme4")
library("car")
library("effects")
library("emmeans")
library("MASS")

# mapping tools
library("OpenStreetMap")
library("ggpubr")


# Fig 1 Map #####
# Input site level data

aphidsites.dat <- read.xlsx("./Data/pea value sites.xlsx", 1, header=TRUE) %>% filter(Year<2019)

head(aphidsites.dat)

# replace "year" with a new "PEMV present" variable, we should show sites of both types infected


# Get a zoomed in map
base_map_zoom <- openmap(upperLeft = c(46.25, -117.65),
                         lowerRight = c(47.2, -116.75),
                         type = "stamen-terrain", zoom=10) %>%
  openproj()

# type argument for sat images is "bing"

# Build a close up map with satellite imagery & zoomed in map
zoom_map <- autoplot.OpenStreetMap(base_map_zoom) +
  geom_point(data = aphidsites.dat,
             aes(x = Long, y = Lat,
                 color = PEMV,
                 shape = Location.Type),
             size=4) +
  scale_size_continuous(10) +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(color="PEMV", shape="Location Type") +
  scale_color_manual(values=c("Blue", "Red")) +
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin())

zoom_map

# This plot is associated with Figure 1 in the accompanying manuscript.
# save map ####

 ggsave(filename = "./Figures/Vetch Fig 1.png", 
       plot = zoom_map, device = "png",
       width = 9, height = 4.5, units = "in")





# Fig 2 ######
# 2018 aphid abundnace on plants
legume.2018.dat <- read.xlsx("./Data/legume data 2018.xlsx", 1, header=TRUE) %>% filter(Crop.type == "Non-crop")

#convert abundance to density
legume.2018.dat$Total.Aphid.Abudance <- log((legume.2018.dat$Total.Aphid.Abudance/legume.2018.dat$Total.Plant.Coverage)+1)


#aphid density fig for 2018 non-crop legume transects
aphid.hosts.fig <- ggplot(legume.2018.dat, aes(x=Total.Aphid.Abudance, 
                                               y=reorder(Plant.Species,-desc(Total.Aphid.Abudance)),
                                               fill=PEMV.Presence)) +
  geom_bar(stat="identity", width=0.6, position="dodge") +
  theme_bw(base_size = 16) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Legume species from 10m transects", x="Aphid density (Log aphid # per meter)", fill="PEMV Present") + 
  scale_fill_grey() +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  theme(axis.text.y = element_text(face = "italic")) +
  theme(legend.position=c(0.8,0.2))
aphid.hosts.fig

# write figure 2 to folder, use arguments to modify size or file type!
# ggsave(filename = "./Figures/Vetch Fig 2.svg", plot = aphid.hosts.fig, device = "svg",
#       width = 6, height = 5, units = "in")

# Fig 3 #####
# plant coverage figure (2b?)

#
host.coverage.fig <- ggplot(legume.2018.dat, aes(x=Total.Plant.Coverage, 
                                                 y=reorder(Plant.Species,-desc(Total.Plant.Coverage)),
                                                 fill=PEMV.Presence)) +
  geom_bar(stat="identity", width=0.6, position="dodge") +
  theme_bw(base_size = 16) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Legume species from transects", x="Sum plant coverage (m)", fill="PEMV Present") + 
  scale_fill_grey() +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  theme(axis.text.y = element_text(face = "italic")) +
  theme(legend.position=c(0.8,0.2))
host.coverage.fig


# write figure 3 to folder, use arguments to modify size or file type!
ggsave(filename = "./Figures/Vetch Fig 3.svg", plot = host.coverage.fig, device = "svg",
       width = 6, height = 5, units = "in")

# Fig 4 ####

# relationship between hairy vetch abundance and aphid abundance
# should this be the proportional abundance or just raw coverage (does it matter?)

veg.dat <- read.csv("./Data/aphid veg join ML.csv")
head(veg.dat)

plot(log(veg.dat$hemiptera.aphididae+1), veg.dat$hairy.vetch)
veg.dat$log.aphids <- log(veg.dat$hemiptera.aphididae+1)
veg.dat$vetchcm <- veg.dat$hairy.vetch / 100

vetch.glm <- lmer(log.aphids ~ hairy.vetch + SiteElevation + CumulativePrecipitation + (1|site), data=veg.dat)
summary(vetch.glm)
Anova(vetch.glm)

vetch.glm.1 <- glm(hairy.vetch ~ SiteElevation + CumulativePrecipitation, data=veg.dat)
Anova(vetch.glm.1)
summary(vetch.glm.1)

plot(veg.dat$hairy.vetch, veg.dat$CumulativePrecipitation)
hist(veg.dat$hairy.vetch)

# make a new command that is "hairy vetch present or absent at site"

vetch.glm.2 <- glmer.nb(hemiptera.aphididae ~ vetchcm + SiteElevation + CumulativePrecipitation +
                          (1|site), data=veg.dat)
summary(vetch.glm.2)
Anova(vetch.glm.2)

veg.dat$aphid.bin <- ifelse(veg.dat$aphid.presence == "Yes", 1, 0)


# i like the binomial model and may just go with that similar to the ecology fig
vetch.glm.3 <- glm(aphid.bin ~ vetchcm, family=binomial, data=veg.dat)
Anova(vetch.glm.3)

# make it like a simple version of the weevil fig from thunderdorm

#The Interaction
ac.interaction.raw <- effect('vetchcm', vetch.glm.3, se=TRUE, xlevels=100)
#Data Frame
ac.interaction <-as.data.frame(ac.interaction.raw)



#Create plot
Plot.ac <-ggplot(data=ac.interaction, aes(x=vetchcm, y=fit))+
  geom_line(size=1)+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se),alpha=.2)+
  theme_bw() +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(text = element_text(size=16),
        legend.text = element_text(size=16),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top") +
  xlab("Average hairy vetch coverage in transect (m)") +
  ylab("Probability of pea aphid presence") +
  geom_point(data=veg.dat, aes(x=vetchcm, y=aphid.bin), position=position_jitter(w=0, h=0.05))
Plot.ac


# write figure 4 to folder, use arguments to modify size or file type!
ggsave(filename = "./Figures/Vetch Fig 4.svg", plot = Plot.ac, device = "svg",
       width = 6, height = 5, units = "in")


# Figure S1 ######
# Pan trap cumulative counts historical data from Sanford, prepared by Ian Cranston

c_dat <- read.csv("./Data/ian thing.csv")
str(c_dat)

c_dat$Month <- as.factor(c_dat$Month)
c_dat$Year <- as.factor(c_dat$Year)
c_dat$AphidTrap <- c_dat$AphidCount / c_dat$TrapCount

c_glm <- glm.nb(AphidTrap ~ Year, data=c_dat)
Anova(c_glm)
summary(c_glm)

c.lsm.2 <- emmeans(c_glm, ~ Year, adjust="none", type="response")
plot(c.lsm.2)
c.lsm.2 <- as.data.frame(c.lsm.2)

# nice ggplot for yearly aphid counts per pan trap

ian.fig <- ggplot(c.lsm.2, aes(x=Year, y=response)) +
  geom_point(size=4.5, shape = 20) +    
  geom_errorbar(aes(ymin=response-SE, ymax=response+SE), width=0) +
  theme_bw(base_size = 14) + 
  labs(y="Average aphid count per sampled pan trap")
ian.fig      

# ggsave(filename = "./Figures/Appendix Fig S3.svg", plot = ian.fig, device = "svg",
#      width = 6, height = 5, units = "in")



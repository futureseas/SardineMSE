# Example plots of current SST and other assessed HCRs in terms of catch and 
# fishing mortality

library(tidyverse)
library(RColorBrewer)
library(colorspace)
library(gridExtra)

x <- c(0, seq(150000, 5000000, by = 50000))
y <- function(x, Fmsy){
  HG <- (x-150000)*Fmsy*0.87
  if(HG < 0) HG <- 0
  if(HG > 200000) HG <- 200000
  
  return(HG)
}

Emsy <- function(Temp){
  -18.46452 + 3.25209*(Temp) - 0.19723*(Temp^2) + 0.0041863*(Temp^3)
} 

xTemp <- seq(13, 17, by = 0.1)
plot(xTemp, Emsy(Temp = xTemp), type = "l")
abline(h = c(0.05, 0.2), lty = 2, col = "grey")

y5 <- sapply(x, y, Fmsy = 0.05)
y20 <- sapply(x, y, Fmsy = 0.2)  

plot(x, y5, type = "l", xlab = "Biomass (mt)", ylab = "Catch (mt)", lwd = 4)
abline(h = 200000, col = 2, lwd = 4)
lines(x, y20, col = "blue", lwd = 4)

# Plots in terms of F_target

# rough median est of SmryBio_Unfished from all assessments
Bsmry0 <- 314000
hcrPal <- brewer.pal(11, "Set3")[-2]
flexPts <- data.frame(SmryBio = c(8000, 0.1*Bsmry0, 0.4*Bsmry0, 150000, 150000, 0.8*Bsmry0,  350000),
                      NoCat = rep(-0.002, times = 7), 
                      PFMCF018 = c(0, 0, 0, 0, 0.18, 0.18, 0.18), 
                      PFMCFSSTlow = c(0, 0, 0, 0, 0.05, 0.05, 0.05),
                      PFMCFSSThigh = c(0, 0, 0, 0, 0.2, 0.2, 0.2),
                      ConstF = rep(0.183, times = 7),
                      Pikitch = c(0, 0, 0, 
                                  rep(0.0475, #0.181*(150000-0.4*Bsmry0)/(Bsmry0*(0.8-0.4)), 
                                      times = 2), 
                                  0.181, 0.181), 
                      Rule40.10 = c(0, 0, 0.182, 0.182, 0.182, 0.182, 0.182))

sstFs <- data.frame(PFMCFSST = rep(seq(0.05, 0.19, by = 0.01), each = 4),
                    Ftarget = c(0.05, 0.05, 
                                rep(seq(0.06, 0.19, by = 0.01), each = 4), 
                                0.2, 0.2),
                    SmryBio = c(150000, 350000, 350000, 150000)) # real point at which MAXCAT triggered is Bsmry ~ 4.5 billion

p2 <- flexPts %>% pivot_longer(cols = -SmryBio, names_to = "HCR", 
                               values_to = "Ftarget") %>%
  filter(!(HCR == "Pikitch" & SmryBio == 150000)) %>%
  mutate(HCR = factor(HCR, levels = c("NoCat", "PFMCF018", "PFMCFSSThigh",  
                                      "PFMCFSSTlow", "ConstF", 
                                      "Pikitch", "Rule40.10"))) %>%
  ggplot(aes(x = SmryBio, y = Ftarget)) +
  # geom_polygon(data = sstFs, aes(fill = PFMCFSST, group = PFMCFSST)) +
  # scale_fill_gradient(low = lighten(hcrPal[3], 0.85), high = hcrPal[3]) +
  geom_line(aes(color = HCR), size = 1.5) +
  scale_color_manual(values = c(hcrPal[1:3],
                                lighten(hcrPal[3], 0.25), 
                                hcrPal[4:6])) +
  geom_segment(data = data.frame(x1 = c(0.1*Bsmry0, 0.4*Bsmry0, 0.8*Bsmry0),
                                 x2 = c(0.1*Bsmry0, 0.4*Bsmry0, 0.8*Bsmry0),
                                 y1 = rep(0, times = 3),
                                 y2 = rep(0.18, times = 3),
                                 HCR = rep("Dyn", times = 3)),
               aes(x = x1, xend = x2, y = y1, yend = y2),
               color = hcrPal[8], size = 1, linetype = "dashed") +
  # abbreviation of right-hand side of HCR plot
  geom_segment(data = data.frame(x1 = rep(350000, times = 7),
                                 x2 = rep(400000, times = 7),
                                 y1 = c(-0.002, 0.18, 0.2, 0.05, 0.183, 0.1810, 0.182),
                                 y2 = c(-0.002, 0.18, 0.2, 0.05, 0.183, 0.1810, 0.182),
                                 HCR = rep("abrv", times = 7)),
               aes(x = x1, xend = x2, y = y1, yend = y2),
               color = c(hcrPal[1:3], lighten(hcrPal[3], 0.25), hcrPal[4:6]), 
               size = 1.5, linetype = "11") +
  theme_classic() +
  labs(x = "Age1+ Biomass (mt)", y = "Exploitation Rate")

# break out for better visibility
flexPts %>% pivot_longer(cols = -SmryBio, names_to = "HCR", values_to = "Ftarget") %>%
  mutate(HCR = factor(HCR, levels = c("NoCat", "PFMCF018", "ConstF", 
                                      "Pikitch", "Rule40.10"))) %>%
  filter(HCR %in% c("NoCat", "PFMCF018", "ConstF")) %>%
  ggplot(aes(x = log(SmryBio), y = Ftarget)) +
  geom_polygon(data = sstFs, aes(fill = PFMCFSST, group = PFMCFSST)) +
  scale_fill_gradient(low = lighten(hcrPal[3], 0.85), high = hcrPal[3]) +
  geom_line(aes(color = HCR), size = 1.5) +
  scale_color_manual(values = hcrPal[-3]) +
  # geom_segment(data = data.frame(x1 = c(0.1*Bsmry0, 0.4*Bsmry0, 0.8*Bsmry0),
  #                                x2 = c(0.1*Bsmry0, 0.4*Bsmry0, 0.8*Bsmry0),
  #                                y1 = rep(0, times = 3),
  #                                y2 = rep(0.18, times = 3),
  #                                HCR = rep("Dyn", times = 3)),
  #              aes(x = log(x1), xend = log(x2), y = y1, yend = y2),
  #              color = hcrPal[8], size = 1, linetype = "dashed") +
  theme_classic() +
  labs(x = "Age1+ Biomass (log-mt)", y = "Target F Rate")

flexPts %>% pivot_longer(cols = -SmryBio, names_to = "HCR", values_to = "Ftarget") %>%
  mutate(HCR = factor(HCR, levels = c("NoCat", "PFMCF018", "ConstF", 
                                      "Pikitch", "Rule40.10"))) %>%
  filter(HCR %in% c("PFMCF018", "Pikitch", "Rule40.10")) %>%
  ggplot(aes(x = log(SmryBio), y = Ftarget)) +
  # geom_polygon(data = sstFs, aes(fill = PFMCFSST, group = PFMCFSST)) +
  # scale_fill_gradient(low = lighten(hcrPal[3], 0.85), high = hcrPal[3]) +
  geom_line(aes(color = HCR), size = 1.5) +
  scale_color_manual(values = hcrPal[-3]) +
  geom_segment(data = data.frame(x1 = c(0.1*Bsmry0, 0.4*Bsmry0, 0.8*Bsmry0),
                                 x2 = c(0.1*Bsmry0, 0.4*Bsmry0, 0.8*Bsmry0),
                                 y1 = rep(0, times = 3),
                                 y2 = rep(0.18, times = 3),
                                 HCR = rep("Dyn", times = 3)),
               aes(x = log(x1), xend = log(x2), y = y1, yend = y2),
               color = hcrPal[8], size = 1, linetype = "dashed") +
  theme_classic() +
  labs(x = "Age1+ Biomass (log-mt)", y = "Target F Rate")


# HCR Demo Plot -----------------------------------------------------------

demoHCRs <- data.frame(Flim = rep(0.3, times = 6), 
                       constF = c(rep(0.2, times = 5), NA), 
                       shallow = c(0, 0, NA, 0.2, 0.2, NA),
                       steep = c(0, 0, 0, 0.2, 0.2, NA),
                       logB = c(0:3, 6, 8))

tradePolys <- data.frame(tradeoffSpace = rep(c("Spawn", "Bonanza"), each = 4),
                         logB = c(0,1,3,0,
                                  6,8,8,6),
                         Ftarget = c(0,0,0.2,0.2,
                                     0,0,0.3,0.3))
greyPal <- colorRampPalette(colors = c("lightgrey", "dimgrey", "lightgrey"))

p1 <- demoHCRs %>% pivot_longer(cols = -logB, names_to = "HCR", values_to = "Ftarget") %>%
  filter(!(HCR == "shallow" & logB == 2)) %>%
  mutate(HCR = factor(HCR, levels = c("Flim", "constF", "steep", "shallow"))) %>%
  ggplot(aes(x = logB, y = Ftarget)) +
  geom_polygon(data = tradePolys, aes(fill = tradeoffSpace, group = tradeoffSpace)) +
  scale_fill_discrete(type = c("rosybrown2", "slategray1")) +
  geom_segment(data = data.frame(x1 = rep(3, times = 9),
                                 x2 = rep(6, times = 9),
                                 y1 = seq(0.18, 0.22, by = 0.005),
                                 y2 = seq(0.18, 0.22, by = 0.005),
                                 HCR = rep("Variable", times = 9)),
               aes(x = x1, xend = x2, y = y1, yend = y2),
               color = greyPal(n = 9), size = c(1,1.5,2,2.5,3,2.5,2,1.5,1), linetype = "solid") +
  geom_line(aes(color = HCR), size = 1.5) +
  scale_color_manual(values = c("#6A3D9A", "#1F78B4", "#B2DF8A", "#33A02C")) + # from brewer.pal(name = "Paired")
  geom_segment(data = data.frame(x1 = c(3, 6),
                                 x2 = c(3, 6),
                                 y1 = c(0, 0.2),
                                 y2 = c(0.2, 0.3),
                                 HCR = rep("Dyn", times = 2)),
               aes(x = x1, xend = x2, y = y1, yend = y2),
               color = "black", size = 0.5, linetype = "dashed") +
  geom_curve(data = data.frame(x1 = 6,
                                 x2 = 8,
                                 y1 = 0.2,
                                 y2 = 0.01,
                                 HCR = "Dyn"),
               aes(x = x1, xend = x2, y = y1, yend = y2),
               color = "black", size = 0.5, linetype = "dashed",
             curvature = 0.4, angle = 110) +
  theme_classic() +
  theme(#axis.text.x = element_blank(),
        legend.position = "none") +
  scale_x_continuous(labels = rep("",9), n.breaks = 9) + 
  labs(x = "Biomass", y = "Target F Rate")

grid.arrange(p1,p2)

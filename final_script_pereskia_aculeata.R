install.packages("readxl")
install.packages("openxlsx")
install.packages("ggplot")
install.packages("ggpmisc")
install.packages("lme4")
install.packages("lmerTest")
install.packages("MuMIn")
install.packages("RColorBrewer")

library(readxl)
library(openxlsx)
library(ggplot2)
library(ggpmisc)
library(lme4)
library(lmerTest)
library(MuMIn)
library(RColorBrewer)

getwd()
rm()

#Loading data for the morphological variables

rawdata_morph <- read.xlsx("raw_data.xlsx" ,2)
names(rawdata_morph)
rawdata_morph$block <- factor(rawdata_morph$block)

#total length of the longest branch (cm)

model_tl <- lmer(data=rawdata_morph,tl ~ oc_percent +(1|block))
summary(model_tl)
summary(residuals(model_tl))
print(model_tl)
plot(model_tl)
hist(residuals(model_tl))

coeff_tl <- fixef(model_tl)
r2_tl <- r.squaredGLMM(model_tl)
text_tl <- sprintf('y = %.2f + %.2fx, r² = %.2f', coeff_tl[1], coeff_tl[2], r2_tl[2])

ggplot(rawdata_morph,aes(oc_percent, tl, color=block)) +
  geom_point(size=4) +
  scale_color_manual(values = c("Block 1" = "#999999", "Block 2" ="#CC79A7","Block 3"="#0072B2","Block 4" = "#E69F00")) +
  geom_text(aes(x=min(oc_percent), y=max(tl), label=text_tl), hjust=-0.06, vjust=3, size=6, family="serif", fontface = 'italic', color = "gray12")+
  geom_smooth(method='lm', se=FALSE, color= "gray12") +
  theme_minimal() +
  theme(axis.line = element_line(linewidth = 0.5, colour = 'gray12', linetype=1),
        axis.title.x = element_text(hjust=0.52, vjust=-1, size=15, face='plain', family= "serif"),
        axis.title.y = element_text(hjust=0.5, vjust =3, size=15, face='plain', family= "serif"),
        axis.text = element_text(size = 12, face = 'plain', family = 'serif', colour = 'gray10'),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
  labs(x='Organic compost (%)', y='Total length (cm)', colour = "Block")+
  scale_x_continuous(breaks=c(0, 25, 50, 75))


#internodes lenght of the longest branch (cm)

model_il <- lmer(data=rawdata_morph,il ~ oc_percent +(1|block))
summary(model_il)
print(model_il)
plot(model_il)
hist(residuals(model_il))

coeff_il <- fixef(model_il)
r2_il <- r.squaredGLMM(model_il)
text_il <- sprintf('y = %.2f + %.2fx, r² = %.2f', coeff_il[1], coeff_il[2], r2_il[2])

ggplot(rawdata_morph,aes(oc_percent, il, color=block)) +
  geom_point(size=4) +
  scale_color_manual(values = c("Block 1" = "#999999", "Block 2" ="#CC79A7","Block 3"="#0072B2","Block 4" = "#E69F00")) +
  geom_text(aes(x=min(oc_percent), y=max(il), label=text_il), hjust=-0.02, vjust=6.7, size=6, family="serif", fontface = 'italic', color = "gray12")+
  geom_smooth(method='lm', se=FALSE, color= "gray12") +
  theme_minimal() +
  theme(axis.line = element_line(linewidth = 0.5, colour = 'gray12', linetype=1),
        axis.title.x = element_text(hjust=0.52, vjust=-1, size=15, face='plain', family= "serif"),
        axis.title.y = element_text(hjust=0.5, vjust =3, size=15, face='plain', family= "serif"),
        axis.text = element_text(size = 12, face = 'plain', family = 'serif', colour = 'gray10'),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
  labs(x='Organic compost (%)', y='Internode length (cm)', colour = 'Block')+
  scale_x_continuous(breaks=c(0, 25, 50, 75))

#diameter of the longest branch (cm) 

model_d <- lmer(data=rawdata_morph,d ~ oc_percent +(1|block))
summary(model_d)
print(model_d)
plot(model_d)
hist(residuals(model_d))

coeff_d <- fixef(model_d)
r2_d <- r.squaredGLMM(model_d)
text_d <- sprintf('y = %.2f + %.2fx, r² = %.2f', coeff_d[1], coeff_d[2], r2_d[2])

ggplot(rawdata_morph,aes(oc_percent, d, color=block)) +
  geom_point(size=4) +
  scale_color_manual(values = c("Block 1" = "#999999", "Block 2" ="#CC79A7","Block 3"="#0072B2","Block 4" = "#E69F00")) +
  geom_text(aes(x=min(oc_percent), y=max(d), label=text_d), hjust=-0.05, vjust=3.2, size=6, family="serif", fontface = 'italic', color = "gray12")+
  geom_smooth(method='lm', se=FALSE, color= "gray12") +
  theme_minimal() +
  theme(axis.line = element_line(linewidth = 0.5, colour = 'gray12', linetype=1),
        axis.title.x = element_text(hjust=0.52, vjust=-1, size=15, face='plain', family= "serif"),
        axis.title.y = element_text(hjust=0.5, vjust =3, size=15, face='plain', family= "serif"),
        axis.text = element_text(size = 12, face = 'plain', family = 'serif', colour = 'gray10'),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
  labs(x='Organic compost (%)', y='Diameter (cm)', colour = 'Block')+
  scale_x_continuous(breaks=c(0, 25, 50, 75))

#number of leaves on the longest branch (unit)

model_nl <- lmer(data=rawdata_morph,nl ~ oc_percent +(1|block))
summary(model_nl)
print(model_nl)
plot(model_nl)
hist(residuals(model_nl))

coeff_nl <- fixef(model_nl)
r2_nl <- r.squaredGLMM(model_nl)
text_nl <- sprintf('y = %.2f + %.2fx, r² = %.2f', coeff_nl[1], coeff_nl[2], r2_nl[2])

ggplot(rawdata_morph,aes(oc_percent, nl, color=block)) +
  geom_point(size=4) +
  scale_color_manual(values = c("Block 1" = "#999999", "Block 2" ="#CC79A7","Block 3"="#0072B2","Block 4" = "#E69F00")) +
  geom_text(aes(x=min(oc_percent), y=max(nl), label=text_nl), hjust=-0.06, vjust=2.9, size=6, family="serif", fontface = 'italic', color = "gray12")+
  geom_smooth(method='lm', se=FALSE, color= "gray12") +
  theme_minimal() +
  theme(axis.line = element_line(linewidth = 0.5, colour = 'gray12', linetype=1),
        axis.title.x = element_text(hjust=0.52, vjust=-1, size=15, face='plain', family= "serif"),
        axis.title.y = element_text(hjust=0.40, vjust =2.5, size=15, face='plain', family= "serif"),
        axis.text = element_text(size = 12, face = 'plain', family = 'serif', colour = 'gray10'),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
  labs(x='Organic compost (%)', y='Number of leaves (unit)', colour = "Block")+
  scale_x_continuous(breaks=c(0, 25, 50, 75))

#fresh mass of the above-ground part (g) 

model_fmag <- lmer(data=rawdata_morph,fmag ~ oc_percent +(1|block))
summary(model_fmag)
print(model_fmag)
plot(model_fmag)
hist(residuals(model_fmag))

coeff_fmag <- fixef(model_fmag)
r2_fmag <- r.squaredGLMM(model_fmag)
text_fmag <- sprintf('y = %.2f + %.2fx, r² = %.2f', coeff_fmag[1], coeff_fmag[2], r2_fmag[2])

ggplot(rawdata_morph,aes(oc_percent, fmag, color=block)) +
  geom_point(size=4) +
  scale_color_manual(values = c("Block 1" = "#999999", "Block 2" ="#CC79A7","Block 3"="#0072B2","Block 4" = "#E69F00")) +
  geom_text(aes(x=min(oc_percent), y=max(fmag), label=text_fmag), hjust=-0.08, vjust=6, size=6, family="serif", fontface = 'italic', color = "gray12")+
  geom_smooth(method='lm', se=FALSE, color= "gray12") +
  theme_minimal() +
  theme(axis.line = element_line(linewidth = 0.5, colour = 'gray12', linetype=1),
        axis.title.x = element_text(hjust=0.52, vjust=-1, size=15, face='plain', family= "serif"),
        axis.title.y = element_text(hjust=0.5, vjust =3, size=15, face='plain', family= "serif"),
        axis.text = element_text(size = 12, face = 'plain', family = 'serif', colour = 'gray10'),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
  labs(x='Organic compost (%)', y='Fresh mass of the above-ground part (g)', colour = 'Block')+
  scale_x_continuous(breaks=c(0, 25, 50, 75))

#Dry mass of the above-ground part (g) 

model_dmag <- lmer(data=rawdata_morph,dmag ~ oc_percent +(1|block))
summary(model_dmag)
print(model_dmag)
plot(model_dmag)
hist(residuals(model_dmag))

coeff_dmag <- fixef(model_dmag)
r2_dmag <- r.squaredGLMM(model_dmag)
text_dmag <- sprintf('y = %.2f + %.2fx, r² = %.2f', coeff_dmag[1], coeff_dmag[2], r2_dmag[2])

ggplot(rawdata_morph,aes(oc_percent, dmag, color=block)) +
  geom_point(size=4) +
  scale_color_manual(values = c("Block 1" = "#999999", "Block 2" ="#CC79A7","Block 3"="#0072B2","Block 4" = "#E69F00")) +
  geom_text(aes(x=min(oc_percent), y=max(dmag), label=text_dmag), hjust=-0.08, vjust=5.2, size=6, family="serif", fontface = 'italic', color = "gray12")+
  geom_smooth(method='lm', se=FALSE, color= "gray12") +
  theme_minimal() +
  theme(axis.line = element_line(linewidth = 0.5, colour = 'gray12', linetype=1),
        axis.title.x = element_text(hjust=0.52, vjust=-1, size=15, face='plain', family= "serif"),
        axis.title.y = element_text(hjust=0.5, vjust =3, size=15, face='plain', family= "serif"),
        axis.text = element_text(size = 12, face = 'plain', family = 'serif', colour = 'gray10'),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
  labs(x='Organic compost (%)', y='Dry mass of the above-ground part (g)', colour = 'Block')+
  scale_x_continuous(breaks=c(0, 25, 50, 75))

#Fresh mass of the roots (g) 

model_fmr <- lmer(data=rawdata_morph,fmr ~ oc_percent +(1|block))
summary(model_fmr)
print(model_fmr)
plot(model_fmr)
hist(residuals(model_fmr))

coeff_fmr <- fixef(model_fmr)
r2_fmr <- r.squaredGLMM(model_fmr)
text_fmr <- sprintf('y = %.2f + %.2fx, r² = %.2f', coeff_fmr[1], coeff_fmr[2], r2_fmr[2])

ggplot(rawdata_morph,aes(oc_percent, fmr, color=block)) +
  geom_point(size=4) +
  scale_color_manual(values = c("Block 1" = "#999999", "Block 2" ="#CC79A7","Block 3"="#0072B2","Block 4" = "#E69F00")) +
  geom_text(aes(x=min(oc_percent), y=max(fmr), label=text_fmr), hjust=-0.08, vjust=2.7, size=6, family="serif", fontface = 'italic', color = "gray12")+
  geom_smooth(method='lm', se=FALSE, color= "gray12") +
  theme_minimal() +
  theme(axis.line = element_line(linewidth = 0.5, colour = 'gray12', linetype=1),
        axis.title.x = element_text(hjust=0.52, vjust=-1, size=15, face='plain', family= "serif"),
        axis.title.y = element_text(hjust=0.5, vjust =3, size=15, face='plain', family= "serif"),
        axis.text = element_text(size = 12, face = 'plain', family = 'serif', colour = 'gray10'),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
  labs(x='Organic compost (%)', y='Fresh mass of the roots (g)', colour = 'Block')+
  scale_x_continuous(breaks=c(0, 25, 50, 75))

#Dry mass of the roots (g)

model_dmr <- lmer(data=rawdata_morph,dmr ~ oc_percent +(1|block))
summary(model_dmr)
print(model_dmr)
plot(model_dmr)
hist(residuals(model_dmr))

coeff_dmr <- fixef(model_dmr)
r2_dmr <- r.squaredGLMM(model_dmr)
text_dmr <- sprintf('y = %.2f + %.2fx, r² = %.2f', coeff_dmr[1], coeff_dmr[2], r2_dmr[2])

ggplot(rawdata_morph,aes(oc_percent, dmr, color=block)) +
  geom_point(size=4) +
  scale_color_manual(values = c("Block 1" = "#999999", "Block 2" ="#CC79A7","Block 3"="#0072B2","Block 4" = "#E69F00")) +
  geom_text(aes(x=min(oc_percent), y=max(dmr), label=text_dmr), hjust=-0.05, vjust=7, size=6, family="serif", fontface = 'italic', color = "gray12")+
  geom_smooth(method='lm', se=FALSE, color= "gray12") +
  theme_minimal() +
  theme(axis.line = element_line(linewidth = 0.5, colour = 'gray12', linetype=1),
        axis.title.x = element_text(hjust=0.52, vjust=-1, size=15, face='plain', family= "serif"),
        axis.title.y = element_text(hjust=0.5, vjust =3, size=15, face='plain', family= "serif"),
        axis.text = element_text(size = 12, face = 'plain', family = 'serif', colour = 'gray10'),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
  labs(x='Organic compost (%)', y='Dry mass of the roots (g)', colour = 'Block')+
  scale_x_continuous(breaks=c(0, 25, 50, 75))

#Loading data for the physiological variables 

rawdata_physio <- read.xlsx("raw_data.xlsx" ,3)
names(rawdata_physio)
rawdata_physio$block <- factor(rawdata_physio$block)

#Net photosyntesis rate (μmol/m²/s) 

model_pn <- lmer(data=rawdata_physio, pn ~ oc_percent +(1|block))
summary(model_pn)
print(model_pn)
plot(model_pn)
hist(residuals(model_pn))

coeff_pn <- fixef(model_pn)
r2_pn <- r.squaredGLMM(model_pn)
text_pn <- sprintf('y = %.2f + %.2fx, r² = %.2f', coeff_pn[1], coeff_pn[2], r2_pn[2])

ggplot(rawdata_physio,aes(oc_percent, pn, color=block)) +
  geom_point(size=4) +
  scale_color_manual(values = c("Block 1" = "#999999", "Block 2" ="#CC79A7","Block 3"="#0072B2","Block 4" = "#E69F00")) +
  geom_text(aes(x=min(oc_percent), y=max(pn), label=text_pn), hjust=-0.05, vjust=4.5, size=6, family="serif", fontface = 'italic', color = "gray12")+
  geom_smooth(method='lm', se=FALSE, color= "gray12") +
  theme_minimal() +
  theme(axis.line = element_line(linewidth = 0.5, colour = 'gray12', linetype=1),
        axis.title.x = element_text(hjust=0.52, vjust=-1, size=15, face='plain', family= "serif"),
        axis.title.y = element_text(hjust=0.5, vjust =3, size=15, face='plain', family= "serif"),
        axis.text = element_text(size = 12, face = 'plain', family = 'serif', colour = 'gray10'),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
  labs(x='Organic compost (%)', y='Net photosyntesis rate (µmol/m²/s)', colour = 'Block')+
  scale_x_continuous(breaks=c(0, 25, 50, 75))

#Maximum quantum efficiency of Photosystem II (Fv'/Fm')

model_fvfm <- lmer(data=rawdata_physio, fvfm ~ oc_percent +(1|block))
summary(model_fvfm)
print(model_fvfm)
plot(model_fvfm)
hist(residuals(model_fvfm))

coeff_fvfm <- fixef(model_fvfm)
r2_fvfm <- r.squaredGLMM(model_fvfm)
text_fvfm <- sprintf('y = %.2f + %.2fx, r² = %.2f', coeff_fvfm[1], coeff_fvfm[2], r2_fvfm[2])

ggplot(rawdata_physio,aes(oc_percent, fvfm, color=block)) +
  geom_point(size=4) +
  scale_color_manual(values = c("Block 1" = "#999999", "Block 2" ="#CC79A7","Block 3"="#0072B2","Block 4" = "#E69F00")) +
  geom_text(aes(x=min(oc_percent), y=max(fvfm), label=text_fvfm), hjust=-0.21, vjust=21, size=6, family="serif", fontface = 'italic', color = "gray12")+
  geom_smooth(method='lm', se=FALSE, color= "gray12") +
  theme_minimal() +
  theme(axis.line = element_line(linewidth = 0.5, colour = 'gray12', linetype=1),
        axis.title.x = element_text(hjust=0.52, vjust=-1, size=15, face='plain', family= "serif"),
        axis.title.y = element_text(hjust=0.5, vjust =3, size=15, face='plain', family= "serif"),
        axis.text = element_text(size = 12, face = 'plain', family = 'serif', colour = 'gray10'),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
  labs(x='Organic compost (%)', y='Maximum quantum efficiency of Photosystem II (Fv/Fm)', colour = 'Block')+
  scale_x_continuous(breaks=c(0, 25, 50, 75))

#Stomatal conductance (molH2O/m2/s)

model_gs <- lmer(data=rawdata_physio, gs ~ oc_percent +(1|block))
summary(model_gs)
print(model_gs)
plot(model_gs)
hist(residuals(model_gs))

coeff_gs <- fixef(model_gs)
r2_gs <- r.squaredGLMM(model_gs)
text_gs <- sprintf('y = %.2f + %.2fx, r² = %.2f', coeff_gs[1], coeff_gs[2], r2_gs[2])

ggplot(rawdata_physio,aes(oc_percent, gs, color=block)) +
  geom_point(size=4) +
  scale_color_manual(values = c("Block 1" = "#999999", "Block 2" ="#CC79A7","Block 3"="#0072B2","Block 4" = "#E69F00")) +
  geom_text(aes(x=min(oc_percent), y=max(gs), label=text_gs), hjust=-0.07, vjust=4.5, size=6, family="serif", fontface = 'italic', color = "gray12")+
  geom_smooth(method='lm', se=FALSE, color= "gray12") +
  theme_minimal() +
  theme(axis.line = element_line(linewidth = 0.5, colour = 'gray12', linetype=1),
        axis.title.x = element_text(hjust=0.52, vjust=-1, size=15, face='plain', family= "serif"),
        axis.title.y = element_text(hjust=0.5, vjust =3, size=15, face='plain', family= "serif"),
        axis.text = element_text(size = 12, face = 'plain', family = 'serif', colour = 'gray10'),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
  labs(x='Organic compost (%)', y='Stomatal conductance (molH2O/m²/s)', colour = 'Block')+
  scale_x_continuous(breaks=c(0, 25, 50, 75))

#Transpiration (mmol/m²/s)

model_e <- lmer(data=rawdata_physio, e ~ oc_percent +(1|block))
summary(model_e)
print(model_e)
plot(model_e)
hist(residuals(model_e))

coeff_e <- fixef(model_e)
r2_e <- r.squaredGLMM(model_e)
text_e <- sprintf('y = %.2f + %.2fx, r² = %.2f', coeff_e[1], coeff_e[2], r2_e[2])

ggplot(rawdata_physio,aes(oc_percent, e, color=block)) +
  geom_point(size=4) +
  scale_color_manual(values = c("Block 1" = "#999999", "Block 2" ="#CC79A7","Block 3"="#0072B2","Block 4" = "#E69F00")) +
  geom_text(aes(x=min(oc_percent), y=max(e), label=text_e), hjust=-0.07, vjust=8.5, size=6, family="serif", fontface = 'italic', color = "gray12")+
  geom_smooth(method='lm', se=FALSE, color= "gray12") +
  theme_minimal() +
  theme(axis.line = element_line(linewidth = 0.5, colour = 'gray12', linetype=1),
        axis.title.x = element_text(hjust=0.52, vjust=-1, size=15, face='plain', family= "serif"),
        axis.title.y = element_text(hjust=0.5, vjust =3, size=15, face='plain', family= "serif"),
        axis.text = element_text(size = 12, face = 'plain', family = 'serif', colour = 'gray10'),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
  labs(x='Organic compost (%)', y='Transpiration (mmol/m²/s)', colour = 'Block')+
  scale_x_continuous(breaks=c(0, 25, 50, 75)) 





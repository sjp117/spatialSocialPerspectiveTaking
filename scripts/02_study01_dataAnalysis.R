# import packages ----
library(dplyr)
library(tidyr)
library(ggpubr)
library(dotwhisker)
library(cowplot)
library(corrplot)
library(lme4)
library(datawizard)
library(effectsize)

# set proper contrast method ----
options(contrasts = c("contr.helmert", "contr.poly")) # SPSS contrast method
# options(contrasts = c("contr.treatment", "contr.poly"))
# options(contrasts = c("contr.sum", "contr.poly"))
options("contrasts")

# import data ----

data1 <- read.csv("data/study01_cleanData.csv")

# set variables as factor

data1$id <- as.factor(data1$id)

data1$gender <- as.factor(data1$gender)

data1$ptFirst <- as.factor(data1$ptFirst)

# Other vs Self ----

## PT * ----

wilcox.test(data1$PTOprop, data1$PTSprop, paired = T)
# V = 3960, p-value < 2.2e-16 ***

### Figure B.4 ----

ptviolin <- data1 %>%
    pivot_longer(cols = c(PTOprop, PTSprop), names_to = "Perspective", values_to = "Score") %>%
    mutate(Perspective = factor(Perspective, labels = c("Other", "Self"))) %>%
    ggplot(aes(x = Perspective, y = Score, fill = Perspective)) +
    geom_violin() +
    geom_jitter(width = 0.1, alpha = 0.3, size = 3) +
    geom_hline(yintercept = 0.5, linetype = 'dashed', size = 1) +
    geom_hline(yintercept = 0.33, linetype = 'dashed', size = 1) +
    ylab(label = "Score Proportion") +
    scale_y_continuous(n.breaks = 10) +
    theme_pubr() +
    theme(text=element_text(size = 20), legend.position="none")

ptviolin

# ggsave(filename = "plots/study1PTviolin.png", plot = ptviolin, units = "px", dpi = 100,
#        width = 900, height = 800)

## False Belief *----

wilcox.test(data1$FBOtotal, data1$FBStotal, paired = T, alternative = "two.sided")
# V = 2750.5, p-value = 4.341e-05 ***

### Figure B.5 ----

fbviolin <- data1 %>%
    pivot_longer(cols = c(FBOprop, FBSprop), names_to = "Perspective", values_to = "Score") %>%
    mutate(Perspective = factor(Perspective, labels = c("Other", "Self"))) %>%
    ggplot(aes(x = Perspective, y = Score, fill = Perspective)) +
    geom_violin() +
    geom_jitter(width = 0.1, height = 0.02, alpha = 0.3, size = 3) +
    geom_hline(yintercept = 0.5, linetype = 'dashed', size = 1) +
    ylab(label = "Score Proportion") +
    scale_y_continuous(n.breaks = 10) +
    theme_pubr() +
    theme(text=element_text(size = 20), legend.position="none")

fbviolin

# mistake in original plot, perspective condition was swapped.

# ggsave(filename = "plots/study1FBviolin.png", plot = fbviolin, units = "px", dpi = 100,
#        width = 900, height = 800)

## Individual Differences ----

indDat <- data1 %>%
    select('Openness' = bigFiveF_O, 'Conscientiousness' = bigFiveF_C, 
           'Extraversion' = bigFiveF_E, 'Agreeableness' = bigFiveF_A,
           'Neuroticism' = bigFiveF_N, 'Anxiety Symptoms' = ASQ_Total,
           'Spatial Anxiety' = SAQ_Total, Vividness = VVIQtotal)

### Figure B.6 ----
qqplot <- indDat %>%
    mutate_all(scale) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Score") %>%
    mutate(Variable = factor(Variable, levels = colnames(indDat))) %>%
    ggplot(aes(sample = Score, color = Variable)) +
    stat_qq() +
    stat_qq_line(color = "black") +
    facet_wrap(~Variable, scales = "free") +
    theme_pubr() +
    scale_color_brewer(palette="Dark2") +
    ylab(label = "Standardized Sample Quantiles") +
    xlab(label = "Theoretical Quantiles") +
    theme(text=element_text(size = 15), legend.position="none")

qqplot

# ggsave(filename = "plots/study1qqplot.png", plot = qqplot, units = "px", dpi = 100,
#        width = 1000, height = 900)

# Task Order ----

## PT ----

### PTO, PT first vs FB first ----

data1 %>%
    select(ptFirst, PTOtotal) %>%
    wilcox_test(PTOtotal ~ ptFirst, detailed = T, paired = F)

# no difference

### PTS, PT first vs FB first ----

data1 %>%
    select(ptFirst, PTStotal) %>%
    wilcox_test(PTStotal ~ ptFirst, detailed = T, paired = F)

# no difference

## False Belief ----

### FBO, PT first vs FB first ----

data1 %>%
    select(ptFirst, FBOtotal) %>%
    wilcox_test(FBOtotal ~ ptFirst, detailed = T, paired = F)

# no difference

### FBS, PT first vs FB first *----

data1 %>%
    select(ptFirst, FBStotal) %>%
    wilcox_test(FBStotal ~ ptFirst, detailed = T, paired = F)

# There is a difference (did not replicate...)

# Correlation ----

## PTO vs FBO *----

cor.test(data1$PTOtotal, data1$FBOtotal)

# 0.2053758, t = 3.3047, df = 248, p-value = 0.001091 ***

### Figure B.7 ----
corrdf <- data1 %>%
    select("Social (Other)" = FBOtotal, "Social (Self)" = FBStotal,
           "Spatial (Other)" = PTOtotal, "Spatial (Self)" = PTStotal)

corMat <- cor(corrdf, method = "pearson")
corRes <- cor.mtest(corrdf)

# # start export plot
# png(filename = "plots/study1CorMat.png", width = 1200, height = 900, res = 100)

corrplot(corMat,
         method = "color",
         type = "upper",
         order = "FPC",
         diag = F,
         p.mat = corRes$p,
         sig.level = 0.012,
         insig = 'label_sig',
         tl.srt= 0,
         tl.offset= 0.8,
         tl.cex = 1.45,
         tl.col = "black")

# # end export plot
# dev.off()

### Figure B.8 ----

scatterplt <- data1 %>%
    ggplot(aes(x = PTOtotal, y = FBOtotal)) +
    geom_jitter(width = .1, height = .1, size = 3, alpha = 0.2) +
    geom_smooth(method = "lm", color = "black") +
    stat_cor(method = "pearson", label.x = 12, label.y = 1.0, size = 6,
             alternative = "greater", cor.coef.name = "r") +
    stat_cor(method = "kendall", label.x = 12, label.y = 0.5, size = 6,
             alternative = "greater", cor.coef.name = "tau") +
    expand_limits(y = c(0, 6)) +
    scale_y_continuous(n.breaks = 6) +
    scale_x_continuous(n.breaks = 15) +
    xlab("Spatial (Other) Perspective Score") +
    ylab("Social (Other) Perspective Score") +
    theme_pubr() +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 15))

scatterplt

# ggsave(filename = "plots/study1ScatterPlot.png", scatterplt, dpi = 100,
#        units = "px", width = 800, height = 700)

# Regression ----

data1 <- data1 %>%
    mutate(
        bigFiveF_Os = standardize(bigFiveF_O),
        bigFiveF_Cs = standardize(bigFiveF_C),
        bigFiveF_Es = standardize(bigFiveF_E),
        bigFiveF_As = standardize(bigFiveF_A),
        bigFiveF_Ns = standardize(bigFiveF_N),
        ASQ_Totals = standardize(ASQ_Total),
        SAQ_Totals = standardize(SAQ_Total),
        VVIQtotals = standardize(VVIQtotal)
    )

## PT ----

ptlong <- data1 %>%
    select(id, PTOprop, PTSprop, bigFiveF_O, bigFiveF_C, bigFiveF_E, bigFiveF_A,
           bigFiveF_N, ASQ_Total, SAQ_Total, VVIQtotal) %>%
    pivot_longer(cols = c(PTOprop, PTSprop), names_to = "persp", values_to = "score") %>%
    mutate(persp = factor(persp))

ptmod <- ptlong %>%
    lm(formula = score ~ persp * 
           (ASQ_Total + SAQ_Total + VVIQtotal + bigFiveF_O + bigFiveF_C + bigFiveF_E +
                bigFiveF_A + bigFiveF_N))

summary(ptmod)

effectsize::eta_squared(ptmod)

### PT dot-whiskers plot ----

ptoMod <- data1 %>%
    lm(formula = PTOprop ~ ASQ_Totals + SAQ_Totals + VVIQtotals + bigFiveF_Os +
           bigFiveF_Cs + bigFiveF_Es + bigFiveF_As + bigFiveF_Ns)

ptsMod <- data1 %>%
    lm(formula = PTSprop ~ ASQ_Totals + SAQ_Totals + VVIQtotals + bigFiveF_Os +
           bigFiveF_Cs + bigFiveF_Es + bigFiveF_As + bigFiveF_Ns)

ptplot <- dwplot(list(ptoMod, ptsMod),
                 vline = geom_vline(xintercept = 0, color = "grey", linetype = 2),
                 vars_order = c("ASQ_Totals", "SAQ_Totals", "VVIQtotals", "bigFiveF_Os",
                                "bigFiveF_Cs", "bigFiveF_Es",
                                "bigFiveF_As", "bigFiveF_Ns"),
                 whisker_args = list(size = 1.5),
                 dot_args = list(size = 3.5),
                 show_intercept = T) %>%
    relabel_predictors(c(
        ASQ_Totals = "Anxiety\nSymptoms",
        SAQ_Totals = "Spatial\nAnxiety",
        VVIQtotals = "Vividness of\nVisualization",
        bigFiveF_Os = "Openness",
        bigFiveF_Cs = "Conscientiousness",
        bigFiveF_Es = "Extraversion",
        bigFiveF_As = "Agreeableness",
        bigFiveF_Ns = "Negative\nEmotionality"
    )) +
    #scale_x_continuous(n.breaks = 6) +
    ggtitle("a) Spatial Perspective") +
    theme_pubr() +
    scale_colour_grey(
        name = "Perspective",
        labels = c("Self", "Other")
    ) +
    theme(text = element_text(size = 15)) 

ptplot

## False Belief ----

fblong <- data1 %>%
    select(id, FBOprop, FBSprop, bigFiveF_O, bigFiveF_C, bigFiveF_E, bigFiveF_A,
           bigFiveF_N, ASQ_Total, SAQ_Total, VVIQtotal) %>%
    pivot_longer(cols = c(FBOprop, FBSprop), names_to = "persp", values_to = "score") %>%
    mutate(persp = factor(persp))

fbmod <- fblong %>%
    lm(formula = score ~ persp * 
           (ASQ_Total + SAQ_Total + VVIQtotal + bigFiveF_O + bigFiveF_C + bigFiveF_E +
                bigFiveF_A + bigFiveF_N))

summary(fbmod)

effectsize::eta_squared(fbmod)

# unlike my original analysis, this model is now statistically significant.

### FB dot-whiskers plot ----

fboMod <- data1 %>%
    lm(formula = FBOprop ~ ASQ_Totals + SAQ_Totals + VVIQtotals + bigFiveF_Os +
           bigFiveF_Cs + bigFiveF_Es + bigFiveF_As + bigFiveF_Ns)

fbsMod <- data1 %>%
    lm(formula = FBSprop ~ ASQ_Totals + SAQ_Totals + VVIQtotals + bigFiveF_Os +
           bigFiveF_Cs + bigFiveF_Es + bigFiveF_As + bigFiveF_Ns)

fbplot <- dwplot(list(fboMod, fbsMod),
                 vline = geom_vline(xintercept = 0, color = "grey", linetype = 2),
                 vars_order = c("ASQ_Totals", "SAQ_Totals", "VVIQtotals", "bigFiveF_Os",
                                "bigFiveF_Cs", "bigFiveF_Es",
                                "bigFiveF_As", "bigFiveF_Ns"),
                 whisker_args = list(size = 1.5),
                 dot_args = list(size = 3.5),
                 show_intercept = T) %>%
    relabel_predictors(c(
        ASQ_Totals = "Anxiety\nSymptoms",
        SAQ_Totals = "Spatial\nAnxiety",
        VVIQtotals = "Vividness of\nVisualization",
        bigFiveF_Os = "Openness",
        bigFiveF_Cs = "Conscientiousness",
        bigFiveF_Es = "Extraversion",
        bigFiveF_As = "Agreeableness",
        bigFiveF_Ns = "Negative\nEmotionality"
    )) +
    #scale_x_continuous(n.breaks = 6) +
    ggtitle("b) Social Perspective") +
    theme_pubr() +
    scale_colour_grey(
        name = "Perspective",
        labels = c("Self", "Other")
    ) +
    theme(text = element_text(size = 15),
          axis.text.y = element_blank()) 

fbplot

## Figure B.9 ----

# combine plots 

regplot <- plot_grid(ptplot, fbplot, labels = NA,
                     rel_widths = c(1.4,1))

regplot

# ggsave(filename = "plots/study1RegDotWhiskersPlot.png", regplot, dpi = 100,
#        units = "px", width = 1000, height = 800)

# models do not replicate, but follow similar trend.
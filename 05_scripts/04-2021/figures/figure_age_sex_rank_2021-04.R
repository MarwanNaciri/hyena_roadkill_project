#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                                                                              #
#                              Figure age sex rank                             #        
#                                                                              #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#


library(tidyverse)
library(ggplot2)
# library(ggthemes)
library(EMT)
library(RVAideMemoire)
library(cowplot)
Sys.setenv(LANG = "en")


# A. EFFECT OF AGE =============================================================

# Import data --------------*
Clan_size_year_clan <- read_csv("06_processed_data/hyenas/Clan_size_year_clan.csv", 
                                col_types = cols(X1 = col_skip()))
hy_carcasses <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)

# Calculate the average age distribution ----------------------

Clan_size_year_clan <- na.omit(Clan_size_year_clan)

Clan_size_year_clan_I <- Clan_size_year_clan %>%
  filter(clan == "Isiaka") %>%
  group_by(age_class) %>%
  summarise(mean_nb = mean(nb))

Clan_size_year_clan_M <- Clan_size_year_clan %>%
  filter(clan == "Mamba") %>%
  group_by(age_class) %>%
  summarise(mean_nb = mean(nb, na.rm = TRUE))

Clan_size_year_clan_P <- Clan_size_year_clan %>%
  filter(clan == "Pool") %>%
  group_by(age_class) %>%
  summarise(mean_nb = mean(nb, na.rm = TRUE),
            sum_nb = sum(mean_nb))

{total <- 0
  for (i in 1:nrow(Clan_size_year_clan_I)) {
    total <- total + Clan_size_year_clan_I$mean_nb[i]
  }
  proportion_I <- c()
  for (j in 1:nrow(Clan_size_year_clan_I)) {
    proportion_I <- rbind(proportion_I, Clan_size_year_clan_I$mean_nb[j]/total)
  }
  
  
  total <- 0
  for (i in 1:nrow(Clan_size_year_clan_M)) {
    total <- total + Clan_size_year_clan_M$mean_nb[i]
  }
  proportion_M <- c()
  for (j in 1:nrow(Clan_size_year_clan_M)) {
    proportion_M <- rbind(proportion_M, Clan_size_year_clan_M$mean_nb[j]/total)
  }
  
  
  total <- 0
  for (i in 1:nrow(Clan_size_year_clan_P)) {
    total <- total + Clan_size_year_clan_P$mean_nb[i]
  }
  proportion_P <- c()
  for (j in 1:nrow(Clan_size_year_clan_P)) {
    proportion_P <- rbind(proportion_P, Clan_size_year_clan_P$mean_nb[j]/total)
  }
  
  
  probabilities <- c()
  for (i in 1:length(proportion_I)) {
    probabilities <- rbind(probabilities, mean(c(proportion_I[i], proportion_M[i], proportion_P[i])))
  }
  }
# sum(probabilities) == 1

probabilities <- t(as.data.frame(probabilities))
colnames(probabilities) <- c("Adult females", 
                             "Adult immigrant males",
                             "Adult natal males", 
                             "Cubs",
                             "Subadults females",
                             "Subadults males")


# Fuse natal and immigrant males, and subadult males and females
proba1 <- cbind(probabilities[1,1],                              # Adult females
                probabilities[1,2] + probabilities[1,3], # Adult males
                probabilities[1,5] + probabilities[1,6], # Subadults
                probabilities[1,4])                      # Cubs

colnames(proba1) <- c("Adult females", 
                      "Adult males",
                      "Subadults", 
                      "Cubs")

# Fuse all adults together, and subadult males and females
proba2 <- cbind(probabilities[1,1] + probabilities[1,2] + probabilities[1,3], # Adults 
                probabilities[1,5] + probabilities[1,6], # Subadults
                probabilities[1,4])
colnames(proba2) <- c("Adults",
                      "Subadults", 
                      "Cubs")

# ~~~ a. Count individuals of each age class ------------

{counts <- hy_carcasses %>%
  count(age,sex)

nb.ad.F <- as.numeric(counts[1,3])
nb.ad.M <- as.numeric(counts[2,3])
nb.ad <- as.numeric(counts[1,3] + counts[2,3] + counts[3,3])
nb.sub <- as.numeric(counts[7,3] + counts[8,3] + counts[9,3])
nb.cub <- as.numeric(counts[4,3] + counts[5,3] + counts[6,3])
nb.unkn <- as.numeric(counts[10,3] + counts[11,3])


obs <- c(nb.ad.F, nb.ad.M, nb.sub, nb.cub) #adult males and adult females in separate categories
obs1 <- c(nb.ad, nb.sub, nb.cub) 
obs2 <- c(nb.ad + nb.unkn, nb.sub, nb.cub) # unknown age considered as adults.
}
# ~~~ b. Plot ----------------------------------

# Adults and subadults VS cubs (without unknowns considered adults)
# df.age <- data.frame(age = c("adults and subadults", "adults and subadults", "cubs", "cubs"),
#                      type = c("observed", "expected", "observed", "expected"),
#                      n =  c(obs1[1]+obs1[2], (proba2[1,1]+proba2[1,2])*(nb.ad + nb.sub + nb.cub), 
#                             obs1[3], proba2[1,3]*(nb.ad + nb.sub + nb.cub)))
# 
# ggplot(df.age, aes(x = age, y = n, fill = type)) +
#   geom_bar(stat = "identity",
#            position = position_dodge(),
#            color = "black",
#            size = 0.5) +
#   scale_fill_grey(start=0.8, end=0.2) +
#   theme_classic() +
#   theme(legend.title = element_blank()) +
#   scale_x_discrete(limits = c("adults and subadults", "cubs"), 
#                    labels = c("adults \n& subadults", "cubs")) +
#   ylab("number of carcasses") +
#   xlab("")
# 
# ggsave("09-02 age expected VS observed.svg", path = "07_intermediate_results/2021-04/plots/Age_Sex",
#        width = unit(3,"cm"), height = unit(2.5,"cm"))


# Adults and subadults VS cubs (with unknowns considered adults)
df.age <- data.frame(age = c("adults and subadults", "adults and subadults", "cubs", "cubs"),
                     type = c("observed", "expected", "observed", "expected"),
                     n =  c(obs2[1] + obs2[2], 
                            (proba2[1,1]+proba2[1,2])*sum(counts$n), # Expected 
                            obs2[3], 
                            proba2[1,3]*sum(counts$n)))              # Expected

(plot.age <- ggplot(df.age, aes(x = age, y = n, fill = type)) +
    geom_bar(stat = "identity",
             position = position_dodge(),
             color = "black",
             size = 0.5) +
    # scale_fill_grey(start = 0.8, end = 0.2) +
    scale_fill_manual(values = c("#999999", "#E69F00")) +
    theme_classic() +
    theme(legend.title = element_blank()) +
    scale_x_discrete(limits = c("adults and subadults", "cubs"), 
                     labels = c("adults \n& subadults", "cubs")) +
    ylab("number of carcasses") +
    xlab("")
)

ggsave("07_intermediate_results/2021-04/plots/4.2 age expected VS observed with unknowns.png", 
       width = unit(3,"cm"), height = unit(2.5,"cm"))





#___________________________________________________________________________####
#B. EFFECT OF SEX ==============================================================

# Import data --------------*
Clan_size_year_clan <- read_csv("06_processed_data/hyenas/Clan_size_year_clan.csv", 
                                col_types = cols(X1 = col_skip()))
hy_carcasses <- read_delim("06_processed_data/carcasses/12_hy.carcasses.certainty.formatted.spatial_updated_10-2021.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)

# Calculate the average adult and subadult sex ratio -------------

Clan_size_year_clan <- na.omit(Clan_size_year_clan)

Clan_size_year_clan_I <- Clan_size_year_clan %>%
  filter(clan == "Isiaka") %>%
  group_by(age_class) %>%
  summarise(mean_nb = mean(nb))

Clan_size_year_clan_M <- Clan_size_year_clan %>%
  filter(clan == "Mamba") %>%
  group_by(age_class) %>%
  summarise(mean_nb = mean(nb, na.rm = TRUE))

Clan_size_year_clan_P <- Clan_size_year_clan %>%
  filter(clan == "Pool") %>%
  group_by(age_class) %>%
  summarise(mean_nb = mean(nb, na.rm = TRUE),
            sum_nb = sum(mean_nb))

{total <- 0
for (i in 1:nrow(Clan_size_year_clan_I)) {
  total <- total + Clan_size_year_clan_I$mean_nb[i]
}
proportion_I <- c()
for (j in 1:nrow(Clan_size_year_clan_I)) {
  proportion_I <- rbind(proportion_I, Clan_size_year_clan_I$mean_nb[j]/total)
}


total <- 0
for (i in 1:nrow(Clan_size_year_clan_M)) {
  total <- total + Clan_size_year_clan_M$mean_nb[i]
}
proportion_M <- c()
for (j in 1:nrow(Clan_size_year_clan_M)) {
  proportion_M <- rbind(proportion_M, Clan_size_year_clan_M$mean_nb[j]/total)
}


total <- 0
for (i in 1:nrow(Clan_size_year_clan_P)) {
  total <- total + Clan_size_year_clan_P$mean_nb[i]
}
proportion_P <- c()
for (j in 1:nrow(Clan_size_year_clan_P)) {
  proportion_P <- rbind(proportion_P, Clan_size_year_clan_P$mean_nb[j]/total)
}


probabilities <- c()
for (i in 1:length(proportion_I)) {
  probabilities <- rbind(probabilities, mean(c(proportion_I[i], proportion_M[i], proportion_P[i])))
}
}

probabilities <- t(as.data.frame(probabilities))
colnames(probabilities) <- c("Adult females", 
                             "Adult immigrant males",
                             "Adult natal males", 
                             "Cubs",
                             "Subadults females",
                             "Subadults males")


ad.sex.ratio <- probabilities[1,1]/(probabilities[1,1] + probabilities[1,2] + probabilities[1,3])

subad.sex.ratio <- probabilities[1,5]/(probabilities[1,5] + probabilities[1,6])


# ~~~ a. Count individuals of each sex ------------

# ----------- Adults --
ad.counts <- hy_carcasses %>%
  filter(age == "adult",
         sex %in% c("F", "M")) %>%
  count(sex)
nb.ad.F <- as.numeric(ad.counts[1,2])
N <- nb.ad.F + as.numeric(ad.counts[2,2])

# Adults With individuals of unknwon age considered adults
ad.counts.bis <- hy_carcasses %>%
  filter(age %in% c("adult", "unknown"),
         sex %in% c("F", "M")) %>%
  count(sex)
nb.ad.F.bis <- as.numeric(ad.counts.bis[1,2])
N.bis <- nb.ad.F + as.numeric(ad.counts.bis[2,2])


# ---------- Subadults --
subad.counts <- hy_carcasses %>%
  filter(age == "subadult",
         sex %in% c("F", "M")) %>%
  count(sex)
nb.subad.F <- as.numeric(subad.counts[1,2])
N.subad <- nb.subad.F + as.numeric(subad.counts[2,2])


# ~~~ b. Plot -------------

#Adults sex ratio (without unknowns)
# df.sex.ad <- data.frame(sex = c("females", "females", "males", "males"),
#                         type = c("observed", "expected", "observed", "expected"),
#                         n = c(nb.ad.F, N*ad.sex.ratio, N - nb.ad.F, N*(1-ad.sex.ratio)))
# 
# ggplot(df.sex.ad, aes(x = sex, y = n, fill = type)) +
#   geom_bar(stat="identity",
#            position=position_dodge(),
#            color = "black",
#            size = 0.5) +
#   scale_fill_grey(start=0.8, end=0.2) +
#   theme_classic() +
#   theme(legend.title = element_blank()) +
#   scale_x_discrete(limits=c("females", "males")) +
#   ylab("number of carcasses") +
#   xlab("")
# 
# ggsave("09-02 ad sex expected VS observed.svg", path = "07_intermediate_results/2021-04/plots/Age_Sex",
#        width = unit(3,"cm"), height = unit(2.5,"cm"))


#Adults sex ratio (with unknowns)
df.sex.ad <- data.frame(sex = c("females", "females", "males", "males"),
                        type = c("observed", "expected", "observed", "expected"),
                        n = c(nb.ad.F.bis, N.bis*ad.sex.ratio, N.bis - nb.ad.F.bis, N.bis*(1-ad.sex.ratio)),
                        age = rep("Adults", times = 4))
# Subadults sex ratio 
df.sex.subad <- data.frame(sex = c("females", "females", "males", "males"),
                           type = c("observed", "expected", "observed", "expected"),
                           n = c(nb.subad.F, N.subad*subad.sex.ratio, N.subad - nb.subad.F, N.subad*(1-subad.sex.ratio)),
                           age = rep("Subadults", times = 4))

# Merge ad and subad
df.sex.ad.subad <- rbind(df.sex.ad, df.sex.subad)

(plot.sex.ad <- ggplot(df.sex.ad, aes(x = sex, y = n, fill = type)) +
    geom_bar(stat = "identity",
             position = position_dodge(),
             color = "black",
             size = 0.5) +
    # scale_fill_grey(start = 0.8, end = 0.2) +
    scale_fill_manual(values = c("#999999", "#E69F00")) +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.position = 'none') +
    scale_x_discrete(limits = c("females", "males")) +
    labs(x = "", 
         y = "number of carcasses")
)
ggsave("07_intermediate_results/2021-04/plots/4.2 ad sex expected VS observed with unknowns.svg", 
       width = unit(3,"cm"), height = unit(2.5,"cm"))


(plot.sex.subad <- ggplot(df.sex.subad, aes(x = sex, y = n, fill = type)) +
    geom_bar(stat = "identity",
             position = position_dodge(),
             color = "black",
             size = 0.5) +
    # scale_fill_grey(start = 0.8, end = 0.2) +
    scale_fill_manual(values = c("#999999", "#E69F00")) +
    theme_classic() +
    theme(legend.title = element_blank()) +
    scale_x_discrete(limits=c("females", "males")) +
    labs(x = "", 
         y = "number of carcasses")
)

ggsave("07_intermediate_results/2021-04/plots/4.3 subad sex expected VS observed with unknowns.svg", 
       width = unit(3,"cm"), height = unit(2.5,"cm"))



# Facetted plot with adults and subadults
(plot.sex.ad.subad <- ggplot(df.sex.ad.subad, aes(x = sex, y = n, fill = type)) +
    geom_bar(stat = "identity",
             position = position_dodge(),
             color = "black",
             size = 0.5) +
    # scale_fill_grey(start = 0.8, end = 0.2) +
    scale_fill_manual(values = c("#999999", "#E69F00")) +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.position = 'none') +
    scale_x_discrete(limits = c("females", "males")) +
    labs(x = "",
         y = "number of carcasses") +
    facet_wrap( ~age)
  
)

ggsave("07_intermediate_results/2021-04/plots/4.2 ad & subad sex expected VS observed with unknowns.png", 
       width = 5, height = 3)





#___________________________________________________________________________####
# C. EFFECT OF RANK============================================================

# All the carcasses of females from the three studied clans have collision 
# certainty scores >= 0.75.

hy_carcasses_clan_members <- read_delim("06_processed_data/carcasses/3_hy.clan.members.csv", 
                                        ";", escape_double = FALSE, trim_ws = TRUE)

# Remove the males and the female cub
hy_carcasses_clan_members <- hy_carcasses_clan_members %>%
  filter(sex == "F",
         age_category %in% c("adult", "subadult")) %>%
  arrange(standardized_rank)


# hy_carcasses_clan_members_ad <- hy_carcasses_clan_members %>%
#   filter(age_category == "adult")


# (plot.rank <- ggplot(hy_carcasses_clan_members, 
#                      aes(x = ID_clan, y = standardized_rank)) +
#    geom_point(size = 2.5) +
#    geom_hline(yintercept=0, size = 0.5, color = "black") +
#    geom_hline(yintercept=0.4, size = 0.5, linetype = "dotted", color = "red") +
#    theme_classic() +
#    theme(axis.text.x = element_text(size=10, angle = 60, hjust = 1),
#          legend.text = element_text(size = 11)) +
#    scale_x_discrete(limits = c("I744", "P619", "I383", "P159", "I484", "I705", "M661", "P518"),
#                     labels = c("Hy_074", "Hy_069", "Hy_055", "Hy_050", "Hy_089", "Hy_088", "Hy_059", "Hy_083")) +  
#    ylim(-1, 1) +
#    labs(x = "ID", 
#         y = "standardized rank")
# )
# 
# ggsave("07_intermediate_results/2021-04/plots/4.4 rank.png", 
#        width = unit(3.5,"cm"), height = unit(3.5,"cm"))
# 
# # With dot observed median, line expected median
# (plot.rank.dotplot <- ggplot(hy_carcasses_clan_members, 
#                              aes(x = collision_certainty_score_NEW, y = standardized_rank)) +
#     geom_dotplot(binaxis = 'y', stackdir = 'center') +
#     annotate("point", x = 1, y = 0.4, 
#              colour = "red", size = 3) +
#     # geom_hline(yintercept = 0.4, size = 0.5, color = "red") +
#     geom_hline(yintercept = 0, size = 0.5, color = "black") +
#     theme_classic() +
#     theme(axis.text.x = element_blank(),
#           axis.ticks.x = element_blank()) +
#     ylim(-1, 1) +
#     labs(y = "standardized rank",
#          x = ""))
# 
# ggsave("07_intermediate_results/2021-04/plots/4.4 rank dotplot.png", 
#        width = unit(1.5,"cm"), height = unit(3,"cm"))


# With dot observed median, dot expected median
median.df <- data.frame(collision_certainty_score_NEW = c(0.98, 0.98),
                        type = c("expected", "observed"),
                        value = c(0, 0.4))

(plot.rank.dotplot <- ggplot(hy_carcasses_clan_members, 
                             aes(x = collision_certainty_score_NEW, y = standardized_rank)) +
    geom_dotplot(binaxis = 'y', stackdir = 'center', fill = "black",
                 stackratio = 2, dotsize = 1.4) +

    geom_point(data = median.df, 
               aes(x = collision_certainty_score_NEW, y = value, fill = type),
               size = 2.75, shape = 24, color = "black") +
    scale_fill_manual(values = c("#999999", "#E69F00"),
                      labels = c("expected \nmedian", "observed \nmedian")) +
    # annotate("point", x = 1, y = 0.4,
    #          colour = "black", size = 3) +
    # annotate("point", x = 1, y = 0, 
    #          colour = "black", size = 3) +
    # annotate("point", x = 1, y = 0.4, 
    #          colour = "#E69F00", size = 2.4,) +
    # annotate("point", x = 1, y = 0, 
    #          colour = "#999999", size = 2.4) +
    # geom_hline(yintercept = 0.4, size = 0.5, color = "red") +
    theme_classic() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = c(0.5, 0.2),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "white",
                                           size = 0.5, # linetype = "solid", 
                                           colour = "white")) +
          # legend.position = "none") +
    ylim(-1, 1) +
    labs(y = "standardized rank",
         x = "") 
)

ggsave("07_intermediate_results/2021-04/plots/4.4 rank dotplot.3.png", 
       width = unit(1.75,"cm"), height = unit(3,"cm"))


# With line observed median, line expected median
median.df <- data.frame(collision_certainty_score_NEW = c(0.98, 0.98),
                        type = c("expected", "observed"),
                        value = c(0, 0.4))

(plot.rank.dotplot <- ggplot(hy_carcasses_clan_members, 
                             aes(x = collision_certainty_score_NEW, y = standardized_rank)) +
    geom_dotplot(binaxis = 'y', stackdir = 'center', fill = "black",
                 stackratio = 2, dotsize = 1.4) +
    geom_hline(data = median.df, 
               aes(yintercept = value, color = type)) +
    
    # geom_hline(yintercept = median.df$value) +
               # size = 2.75, shape = 24, color = "black") +
    scale_color_manual(values = c("#999999", "#E69F00"),
                      labels = c("expected \nmedian", "observed \nmedian")) +
    # annotate("point", x = 1, y = 0.4,
    #          colour = "black", size = 3) +
    # annotate("point", x = 1, y = 0, 
    #          colour = "black", size = 3) +
    # annotate("point", x = 1, y = 0.4, 
    #          colour = "#E69F00", size = 2.4,) +
    # annotate("point", x = 1, y = 0, 
    #          colour = "#999999", size = 2.4) +
    # geom_hline(yintercept = 0.4, size = 0.5, color = "red") +
    theme_classic() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = c(0.5, 0.2),
          legend.title = element_blank(),
          # legend.background = element_rect(fill = "white",
          #                                  size = 0.5, # linetype = "solid", 
          #                                  colour = "white")
          legend.background = element_blank()) +
    # legend.position = "none") +
    ylim(-1, 1) +
    labs(y = "social status",
         x = "") 
)

ggsave("07_intermediate_results/2021-04/plots/4.4 rank dotplot.4.png", 
       width = unit(1.75,"cm"), height = unit(3,"cm"))


#___________________________________________________________________________####
# D. Construct the figure ======================================================

(
  figure.4 <- ggdraw() +
    draw_plot(plot.age, x = 0.02,  y = 0, width = 0.40, height = 1) +
    draw_plot(plot.sex.ad, x = 0.42, y = 0.045, width = 0.27, height = 0.955) +
    
    draw_plot(plot.rank.dotplot, x = 0.71, y = 0.095, width = 0.21, height = 0.905) + 
    
    draw_plot_label(label = c("(a)", "(b)", "(c)"),
                    x = c(0, 0.40, 0.69), y = c(1, 1, 1), size = 14) 
  
)

# save_plot("07_intermediate_results/2021-04/plots/figure 4 (rank dotplot).png", 
#           plot = figure.4,
#           ncol = 2,
#           nrow = 2,
#           base_height = 1.5,
#           base_asp =  3) #1.618)

save_plot("11_manuscript/V3 Figures/figure 4 raw.svg", 
          plot = figure.4,
          ncol = 2,
          nrow = 2,
          base_height = 1.35,
          base_asp =  2.7) #1.618)



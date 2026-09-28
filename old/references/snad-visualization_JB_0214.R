## Goal: SNAD r01 project visualization
## OG Author: Tianyao Qu
## New Author: Jess Brantez
## Date created: Feb 14, 2025
## Updated: Feb 24, 2025

# Load required libraries
library(ggplot2)
library(reshape2)
library(dplyr)
library(readr)
library(magrittr)
library(tidyr)
library(ppcor)
library(haven)
library(lmerTest)
library(MuMIn)

##### 1. Coef Heatmap Full #####

# Load the dataset
data <- read_csv("snad_coef_R_0218_v3.csv")
#View(data)

# Ensure first column (Independent Variables) has unique names
colnames(data)[1] <- "Independent_Variable"

# Reshape data into long format for ggplot
coef_long <- data %>%
  pivot_longer(cols = -Independent_Variable, names_to = "Outcome", values_to = "Coefficient")

# Extract significance markers and convert coefficients to numeric
coef_long <- coef_long %>%
  mutate(Significance = case_when(
    grepl("\\*\\*\\*", Coefficient) ~ "***",
    grepl("\\*\\*", Coefficient) ~ "**",
    grepl("\\*", Coefficient) ~ "*",
    grepl("\\+", Coefficient) ~ "+",
    TRUE ~ ""
  ),
  Coefficient = as.numeric(gsub("[^0-9.-]", "", Coefficient))  # Remove significance markers for plotting
  )

#reorder variables
coef_long <- coef_long %>%
  mutate(
    Outcome = factor(Outcome, levels = c("std_moca_raw", 
                                         "std_execfxn", 
                                         "std_epmem",
                                         "std_CRAFTDRESD",
                                         "MCIdementia_rev")),
    Independent_Variable = factor(Independent_Variable, 
                                  levels = c("1.married",
                                             "1.parenthood", 
                                             "1.working",
                                             "1.assisting_adult",
                                             "1.livealone",
                                             "1.grphouse",
                                             "1.anyvolunteer",
                                             "std_church_day",
                                             "std_famfriends_day",
                                             "std_smcount",
                                             "widowed",
                                             "1.anybrevement",
                                             "std_mhassles",
                                             "std_phassles",
                                             "std_uclaloneliness",
                                             "std_famsupp",
                                             "std_frndsupp",
                                             "std_marriage",
                                             "std_family",
                                             "std_friends",
                                             "std_communityscale",
                                             "std_pclose",
                                             "std_msupport3",
                                             "std_mstrength",
                                             "std_mprox",
                                             "std_pprox",
                                             "std_mfreq",
                                             "std_ploan",
                                             "std_padvice",
                                             "std_pchores",
                                             "std_diff_race",
                                             "std_diff_college",
                                             "std_sdage",
                                             "std_diverse",
                                             "std_pkin",
                                             "std_numphonecontact",
                                             "std_netsize",
                                             "std_b1density",
                                             "std_occcomplex_people",
                                             "std_efctsize",
                                             "1.sole",
                                             "std_weakest",
                                             "std_sdstrength",
                                             "std_sdfreq",
                                             "std_bridging"))
  ) %>%
  arrange(Independent_Variable, Outcome)

#set up 
threshold <- quantile(abs(coef_long$Coefficient), .98)
ylabels <- c("widowed" = "widowed", 
             "std_weakest" = "min tie strength",
             "std_uclaloneliness" = "loneliness",
             "std_smcount" = "# social media sites",
             "std_sdstrength" = "sd tie strength",
             "std_sdfreq" = "sd contact freq",
             "std_pprox" = "prop ? proximity",
             "std_ploan" = "prop loan money",
             "std_pkin" = "prop kin",
             "std_phassles" = "prop hasslers",
             "std_pclose" = "prop very close",
             "std_pchores" = "prop help w chores",
             "std_padvice" = "prop give advice",
             "std_numphonecontact" = "# phone contacts",
             "std_netsize" = "# alters",
             "std_msupport3" = "mean # suppt functions",
             "std_mstrength" = "mean tie strength",
             "std_mprox" = "mean alter proximity",
             "std_mhassles" = "mean # hasslers",
             "std_mfreq" = "mean freq contact",
             "std_marriage" = "marriage rel. quality",
             "std_frndsupp" = "perceived suppt-friends",
             "std_friends" = "friends rel. quality",
             "std_famsupp" = "perceived suppt-family",
             "std_family" = "family rel. quality",
             "std_famfriends_day" = "# days visit family/friends",
             "std_efctsize" = "effective size",
             "std_diverse" = "network diversity",
             "std_diff_race" = "race heterogeneity",
             "std_diff_college" = "edu heterogeneity",
             "std_communityscale" = "community integration",
             "std_church_day" = "# days attend church",
             "std_bridging" = "bridging",
             "std_b1density" = "net density",
             "std_occcomplex_people" = "occ complexity w people",
             "1.working" = "currently working",
             "1.sole" = "sole bridge status",
             "1.parenthood" = "parent", 
             "1.married" = "married",
             "1.livealone" = "live alone",
             "1.grphouse" = "live in group housing",
             "1.assisting_adult" = "assisting adult",
             "1.anyvolunteer" = "volunteer",
             "1.anybrevement" = "fam or friend died past yr")
xlabels <- c("std_moca_raw" = "MoCA",
             "std_execfxn" = "executive function",
             "std_epmem" = "episodic memory",
             "std_CRAFTDRESD" = "Craft delayed recall",
             "MCIdementia_rev" = "no MCI or dementia")

#group_labels <- data.frame(
#  Independent_Variable = c("1.grphouse", "std_occcomplex_people", "std_mstrength", "std_b1density"),  # Position of group labels
#  Group = c("Participation", "Stressors", "Bonding", "Bridging")         # Label names
#)

#grayscale plot absolute value
plot <- ggplot(coef_long, aes(x = Outcome, y = Independent_Variable, fill = abs(Coefficient))) +
  geom_tile() +
  geom_text(aes(
    label = Significance), 
    color = ifelse(abs(coef_long$Coefficient) > threshold, "white", "black"),
    size = 4, 
    vjust = ifelse(coef_long$Significance == "+", 0.5, 0.8)) +
  scale_fill_gradient2(low = "white", high = "grey10") +
  labs(title = "Heatmap of Coefficients with Significance Levels",
       x = "Outcome Variables",
       y = "Independent Variables") +
  scale_x_discrete(labels = xlabels, expand = expansion(mult = c(0.05, 0.3))) +
  scale_y_discrete(labels = ylabels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 10, hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA),  # Set background to white
        panel.background = element_rect(fill = "white", color = NA)   # Ensure panel is white
  )
plot
plot + geom_hline(yintercept = c(10.5, 15.5, 30.5), 
             linetype = "solid", color = "black", size = 0.2)
ggsave("Heatmaps/heatmap_coefficients_JB0218.png")


#####################################
##### 2. Coef Heatmap without MCIdem #####

coef_long2 <- coef_long %>% filter(Outcome != "MCIdementia_rev")
xlabels2 <- c("std_moca_raw" = "MoCA",
             "std_execfxn" = "executive function",
             "std_epmem" = "episodic memory",
             "std_CRAFTDRESD" = "Craft delayed recall")

plot <- ggplot(coef_long2, aes(x = Outcome, y = Independent_Variable, fill = abs(Coefficient))) +
  geom_tile() +
  geom_text(aes(
    label = Significance), 
    color = ifelse(abs(coef_long2$Coefficient) > threshold, "white", "black"),
    size = 4, 
    vjust = ifelse(coef_long2$Significance == "+", 0.5, 0.8)) +
  scale_fill_gradient2(low = "white", high = "grey10") +
  labs(title = "Heatmap of Coefficients with Significance Levels",
       x = "Outcome Variables",
       y = "Independent Variables") +
  scale_x_discrete(labels = xlabels2, expand = expansion(mult = c(0.05, 0.3))) +
  scale_y_discrete(labels = ylabels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 10, hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA),  # Set background to white
        panel.background = element_rect(fill = "white", color = NA)   # Ensure panel is white
  )
plot
plot + geom_hline(yintercept = c(10.5, 15.5, 30.5), 
                  linetype = "solid", color = "black", size = 0.2)

ggsave("Heatmaps/heatmap_noMCI-coefficients_JB0218.png")


# to add group labels 
  #geom_text(data = group_labels, aes(x = max(as.numeric(coef_long$Outcome)) + 1.5, 
  #                                  y = Independent_Variable, 
  #                                   label = Group),
  #          inherit.aes = FALSE,
  #          hjust = 0, size = 5) +
  #  coord_cartesian(clip = "off") +
  #annotate("text", x = .5, y = 5.75, label = "Group 1", hjust = 0, size = 5, fontface = "bold") +  
  #annotate("text", x = 0.5, y = 15, label = "Group 2", hjust = 0, size = 5, fontface = "bold") +  
  #annotate("text", x = 0.5, y = 26, label = "Group 3", hjust = 0, size = 5, fontface = "bold") 

#####################################
##### 3. Coef Heatmap without binary #####
coef_long2 <- coef_long %>% filter(Outcome != "MCIdementia_rev")
coef_long2 <- coef_long2 %>% 
  filter(!Independent_Variable %in% c("1.married", 
                                      "1.parenthood", 
                                      "1.working", 
                                      "1.sole",
                                      "widowed",
                                      "1.livealone",
                                      "1.grphouse",
                                      "1.assisting_adult",
                                      "1.anyvolunteer",
                                      "1.anybrevement"))
xlabels2 <- c("std_moca_raw" = "MoCA",
              "std_execfxn" = "executive function",
              "std_epmem" = "episodic memory",
              "std_CRAFTDRESD" = "Craft delayed recall")
ylabels2 <- c("std_weakest" = "min tie strength",
             "std_uclaloneliness" = "loneliness",
             "std_smcount" = "# social media sites",
             "std_sdstrength" = "sd tie strength",
             "std_sdfreq" = "sd contact freq",
             "std_pprox" = "prop ? proximity",
             "std_ploan" = "prop loan money",
             "std_pkin" = "prop kin",
             "std_phassles" = "prop hasslers",
             "std_pclose" = "prop very close",
             "std_pchores" = "prop help w chores",
             "std_padvice" = "prop give advice",
             "std_numphonecontact" = "# phone contacts",
             "std_netsize" = "# alters",
             "std_msupport3" = "mean # suppt functions",
             "std_mstrength" = "mean tie strength",
             "std_mprox" = "mean alter proximity",
             "std_mhassles" = "mean # hasslers",
             "std_mfreq" = "mean freq contact",
             "std_marriage" = "marriage rel. quality",
             "std_frndsupp" = "perceived suppt-friends",
             "std_friends" = "friends rel. quality",
             "std_famsupp" = "perceived suppt-family",
             "std_family" = "family rel. quality",
             "std_famfriends_day" = "# days visit family/friends",
             "std_efctsize" = "effective size",
             "std_diverse" = "network diversity",
             "std_diff_race" = "race heterogeneity",
             "std_diff_college" = "edu heterogeneity",
             "std_communityscale" = "community integration",
             "std_church_day" = "# days attend church",
             "std_bridging" = "bridging",
             "std_b1density" = "net density",
             "std_occcomplex_people" = "occ complexity w people",
             "std_sdage" = "sd age")
threshold <- quantile(abs(coef_long$Coefficient), .50)

plot <- ggplot(coef_long2, aes(x = Outcome, y = Independent_Variable, fill = abs(Coefficient))) +
  geom_tile() +
  geom_text(aes(
    label = Significance), 
    color = ifelse(abs(coef_long2$Coefficient) > threshold, "white", "black"),
    size = 4, 
    vjust = ifelse(coef_long2$Significance == "+", 0.5, 0.8)) +
  scale_fill_gradient2(low = "white", high = "grey10") +
  labs(title = "Heatmap of Coefficients with Significance Levels",
       x = "Outcome Variables",
       y = "Independent Variables") +
  scale_x_discrete(labels = xlabels2, expand = expansion(mult = c(0.05, 0.3))) +
  scale_y_discrete(labels = ylabels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 10, hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA),  # Set background to white
        panel.background = element_rect(fill = "white", color = NA)   # Ensure panel is white
  )
plot
plot + geom_hline(yintercept = c(3.5, 6.5, 21.5), 
                  linetype = "solid", color = "black", size = 0.2)

ggsave("Heatmaps/heatmap_nobinarycoefficients_JB0218.png")

#####################################
##### 4. Correlation Heatmap #####

df <- read_dta("forvisuals_0130_JB.dta")

df <- df[!(df$time %in% c(4, 5)), ]

# Define control variables
control_vars <- c("std_agesnad", "female", "white", "college", "std_gds15", "std_faq")  

# Define independent variables
varlist <- c("married", "parenthood", "working", "assisting_adult", "livealone", 
             "grphouse", "anyvolunteer", "std_church_day", "std_famfriends_day", 
             "std_smcount", "widowed",
             "anybrevement", "std_occcomplex_people", "std_mhassles", "std_phassles", 
             "std_uclaloneliness", "std_famsupp", "std_frndsupp", "std_marriage", 
             "std_family", "std_friends", "std_communityscale", "std_pclose", 
             "std_msupport3", "std_mstrength", "std_mprox", "std_mfreq", "std_pprox", 
             "std_ploan", "std_padvice", "std_pchores", "std_diff_race", "std_diff_college", 
             "std_sdage", "std_diverse", "std_pkin", "std_numphonecontact", "std_netsize", 
             "std_b1density", "std_efctsize", "sole", "std_weakest", "std_sdstrength", 
             "std_sdfreq", "std_bridging")

# Define multiple outcome variables
outcome_vars <- c("std_moca_raw", "std_execfxn", "std_epmem", "std_CRAFTDRESD", "MCIdementia_rev")  # Replace with actual outcome variable names

# Initialize results storage
results <- data.frame(varname = character(),
                      outcome_var = character(),
                      partial_corr = numeric(),
                      p_value = numeric(),
                      stars = character(),
                      stringsAsFactors = FALSE)

# Loop over each outcome variable
for (outcome in outcome_vars) {
  
  # Loop through each independent variable
  for (var in varlist) {
    
    # Ensure the variable exists in the dataset
    if (!(var %in% colnames(df)) | !(outcome %in% colnames(df))) next 
    
    # Subset to complete cases
    complete_data <- df[, c(var, outcome, control_vars)]
    complete_data <- na.omit(complete_data)
    
    # Skip if too few observations
    if (nrow(complete_data) < 5) next  
    
    # Compute partial correlation
    pcor_result <- pcor.test(complete_data[[var]], complete_data[[outcome]], complete_data[control_vars])
    
    # Assign significance stars
    stars <- ifelse(pcor_result$p.value <= 0.001, "***",
                    ifelse(pcor_result$p.value <= 0.01, "**",
                           ifelse(pcor_result$p.value <= 0.05, "*",
                                  ifelse(pcor_result$p.value <= 0.10, "+", ""))))
    
    # Store result
    results <- rbind(results, data.frame(varname = var,
                                         outcome_var = outcome,
                                         partial_corr = pcor_result$estimate,
                                         p_value = pcor_result$p.value,
                                         stars = stars,
                                         stringsAsFactors = FALSE))
  }
}
head(results)
results <- results[, -4]
head(results)

results$varname <- factor(results$varname, levels = c(
  "married", "parenthood", "working", "assisting_adult", "livealone",
  "grphouse", "anyvolunteer", "std_church_day", "std_famfriends_day",
  "std_smcount", "widowed",
  "anybrevement", "std_mhassles", "std_phassles",
  "std_uclaloneliness", "std_famsupp", "std_frndsupp", "std_marriage",
  "std_family", "std_friends", "std_communityscale", "std_pclose",
  "std_msupport3", "std_mstrength", "std_mprox", "std_pprox",
  "std_mfreq", "std_ploan", "std_padvice", "std_pchores", "std_diff_race",
  "std_diff_college", "std_sdage", "std_diverse", "std_pkin",
  "std_numphonecontact", "std_netsize", "std_b1density", "std_efctsize",
  "std_occcomplex_people", "sole", "std_weakest", "std_sdstrength", "std_sdfreq", "std_bridging"
))
levels(results$varname)

results$outcome_var <- factor(results$outcome_var, levels = c(
  "std_moca_raw", "std_execfxn", "std_epmem", "std_CRAFTDRESD", "MCIdementia_rev"
))
levels(results$outcome_var)

ylabels <- c("widowed" = "widowed", 
             "std_weakest" = "min tie strength",
             "std_uclaloneliness" = "loneliness",
             "std_smcount" = "# social media sites",
             "std_sdstrength" = "sd tie strength",
             "std_sdfreq" = "sd contact freq",
             "std_pprox" = "prop ? proximity",
             "std_ploan" = "prop loan money",
             "std_pkin" = "prop kin",
             "std_phassles" = "prop hasslers",
             "std_pclose" = "prop very close",
             "std_pchores" = "prop help w chores",
             "std_padvice" = "prop give advice",
             "std_numphonecontact" = "# phone contacts",
             "std_netsize" = "# alters",
             "std_msupport3" = "mean # suppt functions",
             "std_mstrength" = "mean tie strength",
             "std_mprox" = "mean alter proximity",
             "std_mhassles" = "mean # hasslers",
             "std_mfreq" = "mean freq contact",
             "std_marriage" = "marriage rel. quality",
             "std_frndsupp" = "perceived suppt-friends",
             "std_friends" = "friends rel. quality",
             "std_famsupp" = "perceived suppt-family",
             "std_family" = "family rel. quality",
             "std_famfriends_day" = "# days visit family/friends",
             "std_efctsize" = "effective size",
             "std_diverse" = "network diversity",
             "std_diff_race" = "race heterogeneity",
             "std_diff_college" = "edu heterogeneity",
             "std_communityscale" = "community integration",
             "std_church_day" = "# days attend church",
             "std_bridging" = "bridging",
             "std_b1density" = "net density",
             "std_occcomplex_people" = "occ complexity w people",
             "1.working" = "currently working",
             "1.sole" = "sole bridge status",
             "1.parenthood" = "parent", 
             "1.married" = "married",
             "1.livealone" = "live alone",
             "1.grphouse" = "live in group housing",
             "1.assisting_adult" = "assisting adult",
             "1.anyvolunteer" = "volunteer",
             "1.anybrevement" = "fam or friend died past yr",
             "std_sdage" = "sd age")
xlabels <- c("std_moca_raw" = "MoCA",
             "std_execfxn" = "executive function",
             "std_epmem" = "episodic memory",
             "std_CRAFTDRESD" = "Craft delayed recall",
             "MCIdementia_rev" = "no MCI or dementia")

threshold <- quantile(abs(results$partial_corr), .60)

plot <- ggplot(results, aes(x = outcome_var, y = varname, fill = abs(partial_corr))) +
  geom_tile() +
  geom_text(aes(
    label = stars), 
    color = ifelse(abs(results$partial_corr) > threshold, "white", "black"),
    size = 4, 
    vjust = ifelse(results$stars == "+", 0.5, 0.8)) +
  scale_fill_gradient2(low = "white", high = "grey10") +
  labs(title = "Heatmap of Partial Correlations with Significance Levels",
       x = "Outcome Variables",
       y = "Independent Variables") +
  scale_x_discrete(labels = xlabels, expand = expansion(mult = c(0.05, 0.3))) +
  scale_y_discrete(labels = ylabels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 10, hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
  )
plot
plot + geom_hline(yintercept = c(10.5, 15.5, 30.5), 
                  linetype = "solid", color = "black", size = 0.2)

ggsave("Heatmaps/heatmap_partial-correlations_JB0221.png")

#####################################
##### 5. BIC Heatmap #####
 
df <- read_dta("forvisuals_0130_JB.dta")

df <- df[!(df$time %in% c(4, 5)), ]

# Define control variables
control_vars <- c("std_agesnad", "female", "white", "college", "std_gds15", "std_faq")  

# Define independent variables
varlist <- c("married", "parenthood", "working", "assisting_adult", "livealone", 
             "grphouse", "anyvolunteer", "std_church_day", "std_famfriends_day", 
             "std_smcount", "widowed",
             "anybrevement", "std_occcomplex_people", "std_mhassles", "std_phassles", 
             "std_uclaloneliness", "std_famsupp", "std_frndsupp", "std_marriage", 
             "std_family", "std_friends", "std_communityscale", "std_pclose", 
             "std_msupport3", "std_mstrength", "std_mprox", "std_mfreq", "std_pprox", 
             "std_ploan", "std_padvice", "std_pchores", "std_diff_race", "std_diff_college", 
             "std_sdage", "std_diverse", "std_pkin", "std_numphonecontact", "std_netsize", 
             "std_b1density", "std_efctsize", "sole", "std_weakest", "std_sdstrength", 
             "std_sdfreq", "std_bridging")

#got rid of "MCIdementia_rev" for now bc difficult to get p-values
outcome_vars <- c("std_moca_raw", "std_execfxn", "std_epmem", "std_CRAFTDRESD")


results <- data.frame(varname = character(),
                      outcome_var = character(),
                      BIC = numeric(),
                      stars = character(),
                      stringsAsFactors = FALSE)

# Loop over each outcome variable
for (outcome in outcome_vars) {
  
  # Loop through each independent variable
  for (var in varlist) {
    
    # Ensure the variable exists in the dataset
    if (!(var %in% colnames(df)) | !(outcome %in% colnames(df))) next 
    
    # Subset to complete cases
    complete_data <- df[, c(var, outcome, control_vars, "SUBID")]
    complete_data <- na.omit(complete_data)
    
    # Skip if too few observations
    if (nrow(complete_data) < 5) next  
    
    # Compute lm
    #if (outcome == "MCIdementia_rev") {
     # lm_result <- glmer(outcome ~ var + control_vars + (1 | SUBID), 
      #                   data = complete_data, 
       #                family = binomial(link = "logit"))
    #} else {
    formula <- as.formula(paste(outcome, 
                                "~", 
                                var, 
                                "+", 
                                paste(control_vars, collapse = " + "), 
                                "+ (1 | SUBID)"))
    lm_result <- lmerTest::lmer(formula, data = complete_data)
    
    # Compute BIC
    BIC <- BIC(lm_result)
    
    #Pull p-values
    p <- coef(summary(lm_result))[var, "Pr(>|t|)"]
    
    # Assign significance stars
    stars <- ifelse(p <= 0.001, "***",
                    ifelse(p <= 0.01, "**",
                           ifelse(p <= 0.05, "*",
                                  ifelse(p <= 0.10, "+", ""))))
    
    # Store result
    results <- rbind(results, data.frame(varname = var,
                                         outcome_var = outcome,
                                         BIC = BIC,
                                         stars = stars,
                                         stringsAsFactors = FALSE))
  }
}

head(results)

results$varname <- factor(results$varname, levels = c(
  "married", "parenthood", "working", "assisting_adult", "livealone",
  "grphouse", "anyvolunteer", "std_church_day", "std_famfriends_day",
  "std_smcount", "widowed",
  "anybrevement", "std_mhassles", "std_phassles",
  "std_uclaloneliness", "std_famsupp", "std_frndsupp", "std_marriage",
  "std_family", "std_friends", "std_communityscale", "std_pclose",
  "std_msupport3", "std_mstrength", "std_mprox", "std_pprox",
  "std_mfreq", "std_ploan", "std_padvice", "std_pchores", "std_diff_race",
  "std_diff_college", "std_sdage", "std_diverse", "std_pkin",
  "std_numphonecontact", "std_netsize", "std_b1density", "std_efctsize",
  "std_occcomplex_people", "sole", "std_weakest", "std_sdstrength", "std_sdfreq", "std_bridging"
))
levels(results$varname)

results$outcome_var <- factor(results$outcome_var, levels = c(
  "std_moca_raw", "std_execfxn", "std_epmem", "std_CRAFTDRESD"
))
levels(results$outcome_var)

threshold <- quantile(results$BIC, .80)

plot <- ggplot(results, aes(x = outcome_var, y = varname, fill = BIC)) +
  geom_tile() +
  geom_text(aes(
    label = stars), 
    color = ifelse(results$BIC < threshold, "white", "black"),
    size = 4, 
    vjust = ifelse(results$stars == "+", 0.5, 0.8)) +
  scale_fill_gradient(low = "grey10", high = "white") +
  labs(title = "Heatmap of BIC with Significance Levels",
       x = "Outcome Variables",
       y = "Independent Variables") +
  scale_x_discrete(labels = xlabels, expand = expansion(mult = c(0.05, 0.3))) +
  scale_y_discrete(labels = ylabels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 10, hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
  )
plot
plot + geom_hline(yintercept = c(10.5, 15.5, 30.5), 
                  linetype = "solid", color = "black", size = 0.2)

ggsave("Heatmaps/heatmap_BIC_JB0224.png")

### fix column to column variation
#method 1: sample size
#complete_data <- df[, c(varlist, outcome_vars, control_vars, "SUBID")]
#complete_data <- na.omit(complete_data)
#absolutely not. made the differences between the columns even more stark

#####################################
##### 6. scaled BIC Heatmap #####

#method 2: standardize within column
results_scaled <- results %>%
  group_by(outcome_var) %>%
  mutate(scaled_BIC = (BIC - min(BIC)) / (max(BIC) - min(BIC))) %>%
  ungroup()
threshold <- quantile(results_scaled$scaled_BIC, 0.2)
plot <- ggplot(results_scaled, aes(x = outcome_var, y = varname, fill = scaled_BIC)) +
  geom_tile() +
  geom_text(aes(
    label = stars), 
    color = ifelse(results_scaled$scaled_BIC < threshold, "white", "black"),
    size = 4, 
    vjust = ifelse(results_scaled$stars == "+", 0.5, 0.8)) +
  scale_fill_gradient(low = "grey10", high = "white") +
  labs(title = "Heatmap of Scaled BIC with Significance Levels",
       x = "Outcome Variables",
       y = "Independent Variables") +
  scale_x_discrete(labels = xlabels, expand = expansion(mult = c(0.05, 0.3))) +
  scale_y_discrete(labels = ylabels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 10, hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
  )
plot
plot + geom_hline(yintercept = c(10.5, 15.5, 30.5), 
                  linetype = "solid", color = "black", size = 0.2)
ggsave("Heatmaps/heatmap_scaledBIC_JB0224.png")


#####################################
##### 7. R-squared Heatmap listwise deletion #####
df <- read_dta("forvisuals_0130_JB.dta")
df <- df[!(df$time %in% c(4, 5)), ]
complete_data <- df[, c(varlist, outcome_vars, control_vars, "SUBID")]
complete_data <- na.omit(complete_data)
#lowers N to 129

# Define control variables
control_vars <- c("std_agesnad", "female", "white", "college", "std_gds15", "std_faq")  

# Define independent variables
varlist <- c("married", "parenthood", "working", "assisting_adult", "livealone", 
             "grphouse", "anyvolunteer", "std_church_day", "std_famfriends_day", 
             "std_smcount", "widowed",
             "anybrevement", "std_occcomplex_people", "std_mhassles", "std_phassles", 
             "std_uclaloneliness", "std_famsupp", "std_frndsupp", "std_marriage", 
             "std_family", "std_friends", "std_communityscale", "std_pclose", 
             "std_msupport3", "std_mstrength", "std_mprox", "std_mfreq", "std_pprox", 
             "std_ploan", "std_padvice", "std_pchores", "std_diff_race", "std_diff_college", 
             "std_sdage", "std_diverse", "std_pkin", "std_numphonecontact", "std_netsize", 
             "std_b1density", "std_efctsize", "sole", "std_weakest", "std_sdstrength", 
             "std_sdfreq", "std_bridging")

#got rid of "MCIdementia_rev" for now bc difficult to get p-values
outcome_vars <- c("std_moca_raw", "std_execfxn", "std_epmem", "std_CRAFTDRESD")


results <- data.frame(varname = character(),
                      outcome_var = character(),
                      Rsq_diff = numeric(),
                      stars = character(),
                      n_partial = numeric(),
                      n_full = numeric(),
                      stringsAsFactors = FALSE)

# Loop over each outcome variable
for (outcome in outcome_vars) {
  
  formula_partial <- as.formula(paste(outcome, 
                              "~", 
                              paste(control_vars, collapse = " + "), 
                              "+ (1 | SUBID)"))
  lm_partial <- lmer(formula_partial, data = complete_data)
  
  Rsq_partial <- r.squaredGLMM(lm_partial)[,1] #of fixed effects only
  
  n_partial <- nobs(lm_partial)
  
  # Loop through each independent variable
  for (var in varlist) {
    
    # Ensure the variable exists in the dataset
    if (!(var %in% colnames(df)) | !(outcome %in% colnames(df))) next 

    formula <- as.formula(paste(outcome, 
                                "~", 
                                var, 
                                "+", 
                                paste(control_vars, collapse = " + "), 
                                "+ (1 | SUBID)"))
    lm_result <-lmerTest::lmer(formula, data = complete_data)
    
    n_full <- nobs(lm_result)
    
    # Compute R-squared
    Rsq_full <- r.squaredGLMM(lm_result)[,1]
    
    #Difference
    Rsq_diff <- Rsq_full - Rsq_partial
    
    #Pull p-values
    p <- coef(summary(lm_result))[var, "Pr(>|t|)"]
    
    # Assign significance stars
    stars <- ifelse(p <= 0.001, "***",
                    ifelse(p <= 0.01, "**",
                           ifelse(p <= 0.05, "*",
                                  ifelse(p <= 0.10, "+", ""))))
    
    # Store result
    results <- rbind(results, data.frame(varname = var,
                                         outcome_var = outcome,
                                         Rsq_diff = Rsq_diff,
                                         stars = stars,
                                         n_partial = n_partial,
                                         n_full = n_full,
                                         stringsAsFactors = FALSE))
  }
}

head(results)

results$varname <- factor(results$varname, levels = c(
  "married", "parenthood", "working", "assisting_adult", "livealone",
  "grphouse", "anyvolunteer", "std_church_day", "std_famfriends_day",
  "std_smcount", "widowed",
  "anybrevement", "std_mhassles", "std_phassles",
  "std_uclaloneliness", "std_famsupp", "std_frndsupp", "std_marriage",
  "std_family", "std_friends", "std_communityscale", "std_pclose",
  "std_msupport3", "std_mstrength", "std_mprox", "std_pprox",
  "std_mfreq", "std_ploan", "std_padvice", "std_pchores", "std_diff_race",
  "std_diff_college", "std_sdage", "std_diverse", "std_pkin",
  "std_numphonecontact", "std_netsize", "std_b1density", "std_efctsize",
  "std_occcomplex_people", "sole", "std_weakest", "std_sdstrength", "std_sdfreq", "std_bridging"
))

levels(results$varname)

results$outcome_var <- factor(results$outcome_var, levels = c(
  "std_moca_raw", "std_execfxn", "std_epmem", "std_CRAFTDRESD"
))
levels(results$outcome_var)

ylabels <- c("widowed" = "widowed", 
             "std_weakest" = "min tie strength",
             "std_uclaloneliness" = "loneliness",
             "std_smcount" = "# social media sites",
             "std_sdstrength" = "sd tie strength",
             "std_sdfreq" = "sd contact freq",
             "std_pprox" = "prop ? proximity",
             "std_ploan" = "prop loan money",
             "std_pkin" = "prop kin",
             "std_phassles" = "prop hasslers",
             "std_pclose" = "prop very close",
             "std_pchores" = "prop help w chores",
             "std_padvice" = "prop give advice",
             "std_numphonecontact" = "# phone contacts",
             "std_netsize" = "# alters",
             "std_msupport3" = "mean # suppt functions",
             "std_mstrength" = "mean tie strength",
             "std_mprox" = "mean alter proximity",
             "std_mhassles" = "mean # hasslers",
             "std_mfreq" = "mean freq contact",
             "std_marriage" = "marriage rel. quality",
             "std_frndsupp" = "perceived suppt-friends",
             "std_friends" = "friends rel. quality",
             "std_famsupp" = "perceived suppt-family",
             "std_family" = "family rel. quality",
             "std_famfriends_day" = "# days visit family/friends",
             "std_efctsize" = "effective size",
             "std_diverse" = "network diversity",
             "std_diff_race" = "race heterogeneity",
             "std_diff_college" = "edu heterogeneity",
             "std_communityscale" = "community integration",
             "std_church_day" = "# days attend church",
             "std_bridging" = "bridging",
             "std_b1density" = "net density",
             "std_occcomplex_people" = "occ complexity w people",
             "1.working" = "currently working",
             "1.sole" = "sole bridge status",
             "1.parenthood" = "parent", 
             "1.married" = "married",
             "1.livealone" = "live alone",
             "1.grphouse" = "live in group housing",
             "1.assisting_adult" = "assisting adult",
             "1.anyvolunteer" = "volunteer",
             "1.anybrevement" = "fam or friend died past yr",
             "std_sdage" = "sd age")
xlabels <- c("std_moca_raw" = "MoCA",
             "std_execfxn" = "executive function",
             "std_epmem" = "episodic memory",
             "std_CRAFTDRESD" = "Craft delayed recall")

threshold <- quantile(results$Rsq_diff, .80)

plot <- ggplot(results, aes(x = outcome_var, y = varname, fill = Rsq_diff)) +
  geom_tile() +
  geom_text(aes(
    label = stars), 
    color = ifelse(results$Rsq_diff > threshold, "white", "black"),
    size = 4, 
    vjust = ifelse(results$stars == "+", 0.5, 0.8)) +
  scale_fill_gradient(low = "grey10", high = "white") +
  labs(title = "Heatmap of Rsq Diff with Significance Levels",
       x = "Outcome Variables",
       y = "Independent Variables") +
  scale_x_discrete(labels = xlabels, expand = expansion(mult = c(0.05, 0.3))) +
  scale_y_discrete(labels = ylabels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 10, hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
  )
plot
plot + geom_hline(yintercept = c(10.5, 15.5, 30.5), 
                  linetype = "solid", color = "black", size = 0.2)

ggsave("Heatmaps/heatmap_Rsq_diff_JB0224.png")

#####################################
##### 8. scaled Rsq diff Heatmap #####

#fix column to column variation
#standardize within column
results_scaled <- results %>%
  group_by(outcome_var) %>%
  mutate(scaled_Rsq_diff = (Rsq_diff - min(Rsq_diff)) / (max(Rsq_diff) - min(Rsq_diff))) %>%
  ungroup()

threshold <- quantile(results_scaled$scaled_Rsq_diff, .40)

plot <- ggplot(results_scaled, aes(x = outcome_var, y = varname, fill = scaled_Rsq_diff)) +
  geom_tile() +
  geom_text(aes(
    label = stars), 
    color = ifelse(results_scaled$scaled_Rsq_diff > threshold, "white", "black"),
    size = 4, 
    vjust = ifelse(results_scaled$stars == "+", 0.5, 0.8)) +
  scale_fill_gradient(low = "white", high = "grey10") +
  labs(title = "Heatmap of Rsq Diff with Significance Levels",
       x = "Outcome Variables",
       y = "Independent Variables") +
  scale_x_discrete(labels = xlabels, expand = expansion(mult = c(0.05, 0.3))) +
  scale_y_discrete(labels = ylabels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 10, hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
  )
plot
plot + geom_hline(yintercept = c(10.5, 15.5, 30.5), 
                  linetype = "solid", color = "black", size = 0.2)

ggsave("Heatmaps/heatmap_scaledRsq_diff_JB0224.png")

#####################################
##### 9. R-squared Heatmap no listwise delete#####
df <- read_dta("forvisuals_0130_JB.dta")
df <- df[!(df$time %in% c(4, 5)), ]

# Define control variables
control_vars <- c("std_agesnad", "female", "white", "college", "std_gds15", "std_faq")  

# Define independent variables
varlist <- c("married", "parenthood", "working", "assisting_adult", "livealone", 
             "grphouse", "anyvolunteer", "std_church_day", "std_famfriends_day", 
             "std_smcount", "widowed",
             "anybrevement", "std_occcomplex_people", "std_mhassles", "std_phassles", 
             "std_uclaloneliness", "std_famsupp", "std_frndsupp", "std_marriage", 
             "std_family", "std_friends", "std_communityscale", "std_pclose", 
             "std_msupport3", "std_mstrength", "std_mprox", "std_mfreq", "std_pprox", 
             "std_ploan", "std_padvice", "std_pchores", "std_diff_race", "std_diff_college", 
             "std_sdage", "std_diverse", "std_pkin", "std_numphonecontact", "std_netsize", 
             "std_b1density", "std_efctsize", "sole", "std_weakest", "std_sdstrength", 
             "std_sdfreq", "std_bridging")

#got rid of "MCIdementia_rev" for now bc difficult to get p-values
outcome_vars <- c("std_moca_raw", "std_execfxn", "std_epmem", "std_CRAFTDRESD")


results <- data.frame(varname = character(),
                      outcome_var = character(),
                      Rsq_diff = numeric(),
                      stars = character(),
                      n_partial = numeric(),
                      n_full = numeric(),
                      stringsAsFactors = FALSE)

# Loop over each outcome variable
for (outcome in outcome_vars) {
  
  # Loop through each independent variable
  for (var in varlist) {
    
    # Ensure the variable exists in the dataset
    if (!(var %in% colnames(df)) | !(outcome %in% colnames(df))) next 
    
    # Subset to complete cases
    complete_data <- df[, c(var, outcome, control_vars, "SUBID")]
    complete_data <- na.omit(complete_data)
    
    # Compute lm without key var
    formula_partial <- as.formula(paste(outcome, 
                                        "~", 
                                        paste(control_vars, collapse = " + "), 
                                        "+ (1 | SUBID)"))
    lm_partial <- lmer(formula_partial, data = complete_data)
    
    Rsq_partial <- r.squaredGLMM(lm_partial)[,1] #of fixed effects only
    
    n_partial <- nobs(lm_partial)
    
    #Compute lm with key var
    formula_full <- as.formula(paste(outcome, 
                                "~", 
                                var, 
                                "+", 
                                paste(control_vars, collapse = " + "), 
                                "+ (1 | SUBID)"))
    lm_result <- lmerTest::lmer(formula_full, data = complete_data)
    
    n_full <- nobs(lm_result)
    
    # Compute R-squared
    Rsq_full <- r.squaredGLMM(lm_result)[,1]
    
    #Difference
    Rsq_diff <- Rsq_full - Rsq_partial
    
    #Pull p-values
    p <- coef(summary(lm_result))[var, "Pr(>|t|)"]
    
    # Assign significance stars
    stars <- ifelse(p <= 0.001, "***",
                    ifelse(p <= 0.01, "**",
                           ifelse(p <= 0.05, "*",
                                  ifelse(p <= 0.10, "+", ""))))
    
    # Store result
    results <- rbind(results, data.frame(varname = var,
                                         outcome_var = outcome,
                                         Rsq_diff = Rsq_diff,
                                         stars = stars,
                                         n_partial = n_partial,
                                         n_full = n_full,
                                         stringsAsFactors = FALSE))
  }
}

head(results)
all(results$n_partial == results$n_full)

results$varname <- factor(results$varname, levels = c(
  "married", "parenthood", "working", "assisting_adult", "livealone",
  "grphouse", "anyvolunteer", "std_church_day", "std_famfriends_day",
  "std_smcount", "widowed",
  "anybrevement", "std_mhassles", "std_phassles",
  "std_uclaloneliness", "std_famsupp", "std_frndsupp", "std_marriage",
  "std_family", "std_friends", "std_communityscale", "std_pclose",
  "std_msupport3", "std_mstrength", "std_mprox", "std_pprox",
  "std_mfreq", "std_ploan", "std_padvice", "std_pchores", "std_diff_race",
  "std_diff_college", "std_sdage", "std_diverse", "std_pkin",
  "std_numphonecontact", "std_netsize", "std_b1density", "std_efctsize",
  "std_occcomplex_people", "sole", "std_weakest", "std_sdstrength", "std_sdfreq", "std_bridging"
))
levels(results$varname)

results$outcome_var <- factor(results$outcome_var, levels = c(
  "std_moca_raw", "std_execfxn", "std_epmem", "std_CRAFTDRESD"
))
levels(results$outcome_var)

ylabels <- c("widowed" = "widowed", 
             "std_weakest" = "min tie strength",
             "std_uclaloneliness" = "loneliness",
             "std_smcount" = "# social media sites",
             "std_sdstrength" = "sd tie strength",
             "std_sdfreq" = "sd contact freq",
             "std_pprox" = "prop ? proximity",
             "std_ploan" = "prop loan money",
             "std_pkin" = "prop kin",
             "std_phassles" = "prop hasslers",
             "std_pclose" = "prop very close",
             "std_pchores" = "prop help w chores",
             "std_padvice" = "prop give advice",
             "std_numphonecontact" = "# phone contacts",
             "std_netsize" = "# alters",
             "std_msupport3" = "mean # suppt functions",
             "std_mstrength" = "mean tie strength",
             "std_mprox" = "mean alter proximity",
             "std_mhassles" = "mean # hasslers",
             "std_mfreq" = "mean freq contact",
             "std_marriage" = "marriage rel. quality",
             "std_frndsupp" = "perceived suppt-friends",
             "std_friends" = "friends rel. quality",
             "std_famsupp" = "perceived suppt-family",
             "std_family" = "family rel. quality",
             "std_famfriends_day" = "# days visit family/friends",
             "std_efctsize" = "effective size",
             "std_diverse" = "network diversity",
             "std_diff_race" = "race heterogeneity",
             "std_diff_college" = "edu heterogeneity",
             "std_communityscale" = "community integration",
             "std_church_day" = "# days attend church",
             "std_bridging" = "bridging",
             "std_b1density" = "net density",
             "std_occcomplex_people" = "occ complexity w people",
             "1.working" = "currently working",
             "1.sole" = "sole bridge status",
             "1.parenthood" = "parent", 
             "1.married" = "married",
             "1.livealone" = "live alone",
             "1.grphouse" = "live in group housing",
             "1.assisting_adult" = "assisting adult",
             "1.anyvolunteer" = "volunteer",
             "1.anybrevement" = "fam or friend died past yr",
             "std_sdage" = "sd age")
xlabels <- c("std_moca_raw" = "MoCA",
             "std_execfxn" = "executive function",
             "std_epmem" = "episodic memory",
             "std_CRAFTDRESD" = "Craft delayed recall")

threshold <- quantile(results$Rsq_diff, .80)

plot <- ggplot(results, aes(x = outcome_var, y = varname, fill = Rsq_diff)) +
  geom_tile() +
  geom_text(aes(
    label = stars), 
    color = ifelse(results$Rsq_diff > threshold, "white", "black"),
    size = 4, 
    vjust = ifelse(results$stars == "+", 0.5, 0.8)) +
  scale_fill_gradient(low = "grey10", high = "white") +
  labs(title = "Heatmap of Rsq Diff with Significance Levels",
       x = "Outcome Variables",
       y = "Independent Variables") +
  scale_x_discrete(labels = xlabels, expand = expansion(mult = c(0.05, 0.3))) +
  scale_y_discrete(labels = ylabels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 10, hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
  )
plot
plot + geom_hline(yintercept = c(10.5, 15.5, 30.5), 
                  linetype = "solid", color = "black", size = 0.2)

ggsave("Heatmaps/heatmap_nodel-Rsq_diff_JB0226.png")


#standardize within column
results_scaled <- results %>%
  group_by(outcome_var) %>%
  mutate(scaled_Rsq_diff = (Rsq_diff - min(Rsq_diff)) / (max(Rsq_diff) - min(Rsq_diff))) %>%
  ungroup()

threshold <- quantile(results_scaled$scaled_Rsq_diff, .40)

plot <- ggplot(results_scaled, aes(x = outcome_var, y = varname, fill = scaled_Rsq_diff)) +
  geom_tile() +
  geom_text(aes(
    label = stars), 
    color = ifelse(results_scaled$scaled_Rsq_diff > threshold, "white", "black"),
    size = 4, 
    vjust = ifelse(results_scaled$stars == "+", 0.5, 0.8)) +
  scale_fill_gradient(low = "white", high = "grey10") +
  labs(title = "Heatmap of Rsq Diff with Significance Levels",
       x = "Outcome Variables",
       y = "Independent Variables") +
  scale_x_discrete(labels = xlabels, expand = expansion(mult = c(0.05, 0.3))) +
  scale_y_discrete(labels = ylabels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 10, hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
  )
plot
plot + geom_hline(yintercept = c(10.5, 15.5, 30.5), 
                  linetype = "solid", color = "black", size = 0.2)

ggsave("Heatmaps/heatmap_nodel-scaledRsq_diff_JB0226.png")


##### 10. Coef Heatmap Fully Standardized #####
library(ggplot2)
library(reshape2)
library(dplyr)
library(readr)
library(magrittr)
library(tidyr)
library(ppcor)
library(haven)
library(lmerTest)
library(MuMIn)

# Load the dataset
data <- read_csv("snad_coef_std_R.csv")
#View(data)

# Ensure first column (Independent Variables) has unique names
colnames(data)[1] <- "Independent_Variable"

# Reshape data into long format for ggplot
coef_long <- data %>%
  pivot_longer(cols = -Independent_Variable, names_to = "Outcome", values_to = "Coefficient")

# Extract significance markers and convert coefficients to numeric
coef_long <- coef_long %>%
  mutate(Significance = case_when(
    grepl("\\*\\*\\*", Coefficient) ~ "***",
    grepl("\\*\\*", Coefficient) ~ "**",
    grepl("\\*", Coefficient) ~ "*",
    grepl("\\+", Coefficient) ~ "+",
    TRUE ~ ""
  ),
  Coefficient = as.numeric(gsub("[^0-9.-]", "", Coefficient))  # Remove significance markers for plotting
  )

#reorder variables
coef_long <- coef_long %>%
  mutate(
    Outcome = factor(Outcome, levels = c("std_moca_raw", 
                                         "std_execfxn", 
                                         "std_epmem",
                                         "std_CRAFTDRESD",
                                         "std_MCIdementia_rev")),
    Independent_Variable = factor(Independent_Variable, 
                                  levels = c("std_married",
                                             "std_parenthood", 
                                             "std_working",
                                             "std_assisting_adult",
                                             "std_livealone",
                                             "std_grphouse",
                                             "std_anyvolunteer",
                                             "std_church_day",
                                             "std_famfriends_day",
                                             "std_smcount",
                                             "std_widowed",
                                             "std_anybrevement",
                                             "std_phassles",
                                             "std_uclaloneliness",
                                             "std_famsupp",
                                             "std_frndsupp",
                                             "std_marriage",
                                             "std_family",
                                             "std_friends",
                                             "std_communityscale",
                                             "std_pclose",
                                             "std_msupport3",
                                             "std_mstrength",
                                             "std_pprox",
                                             "std_mfreq",
                                             "std_ploan",
                                             "std_padvice",
                                             "std_pchores",
                                             "std_diff_race",
                                             "std_diff_college",
                                             "std_sdage",
                                             "std_diverse",
                                             "std_pkin",
                                             "std_numphonecontact",
                                             "std_netsize",
                                             "std_b1density",
                                             "std_occcomplex_people",
                                             "std_efctsize",
                                             "std_sole",
                                             "std_weakest",
                                             "std_sdstrength",
                                             "std_sdfreq",
                                             "std_bridging"))
  ) %>%
  arrange(Independent_Variable, Outcome)

#set up 
threshold <- quantile(abs(coef_long$Coefficient), .90)
ylabels <- c("std_widowed" = "widowed", 
             "std_weakest" = "min tie strength",
             "std_uclaloneliness" = "loneliness",
             "std_smcount" = "# social media sites",
             "std_sdstrength" = "sd tie strength",
             "std_sdfreq" = "sd contact freq",
             "std_pprox" = "prop ? proximity",
             "std_ploan" = "prop loan money",
             "std_pkin" = "prop kin",
             "std_phassles" = "prop hasslers",
             "std_pclose" = "prop very close",
             "std_pchores" = "prop help w chores",
             "std_padvice" = "prop give advice",
             "std_numphonecontact" = "# phone contacts",
             "std_netsize" = "# alters",
             "std_msupport3" = "mean # suppt functions",
             "std_mstrength" = "mean tie strength",
             "std_mfreq" = "mean freq contact",
             "std_marriage" = "marriage rel. quality",
             "std_frndsupp" = "perceived suppt-friends",
             "std_friends" = "friends rel. quality",
             "std_famsupp" = "perceived suppt-family",
             "std_family" = "family rel. quality",
             "std_famfriends_day" = "# days visit family/friends",
             "std_efctsize" = "effective size",
             "std_diverse" = "network diversity",
             "std_diff_race" = "race heterogeneity",
             "std_diff_college" = "edu heterogeneity",
             "std_communityscale" = "community integration",
             "std_church_day" = "# days attend church",
             "std_bridging" = "bridging",
             "std_b1density" = "net density",
             "std_occcomplex_people" = "occ complexity w people",
             "std_working" = "currently working",
             "std_sole" = "sole bridge status",
             "std_parenthood" = "parent", 
             "std_married" = "married",
             "std_livealone" = "live alone",
             "std_grphouse" = "live in group housing",
             "std_assisting_adult" = "assisting adult",
             "std_anyvolunteer" = "volunteer",
             "std_anybrevement" = "fam or friend died past yr",
             "std_sdage" = "sd tie age")
xlabels <- c("std_moca_raw" = "MoCA",
             "std_execfxn" = "executive function",
             "std_epmem" = "episodic memory",
             "std_CRAFTDRESD" = "Craft delayed recall",
             "std_MCIdementia_rev" = "no MCI or dementia")

#group_labels <- data.frame(
#  Independent_Variable = c("1.grphouse", "std_occcomplex_people", "std_mstrength", "std_b1density"),  # Position of group labels
#  Group = c("Participation", "Stressors", "Bonding", "Bridging")         # Label names
#)

#grayscale plot absolute value
plot <- ggplot(coef_long, aes(x = Outcome, y = Independent_Variable, fill = abs(Coefficient))) +
  geom_tile() +
  geom_text(aes(
    label = Significance), 
    color = ifelse(abs(coef_long$Coefficient) > threshold, "white", "black"),
    size = 4, 
    vjust = ifelse(coef_long$Significance == "+", 0.5, 0.8)) +
  scale_fill_gradient2(low = "white", high = "grey10", name = "Abs(Coef)") +
  labs(title = "Heatmap of Coefficients with Significance Levels",
       x = "Outcome Variables",
       y = "Independent Variables") +
  scale_x_discrete(labels = xlabels, expand = expansion(mult = c(0.05, 0.3))) +
  scale_y_discrete(labels = ylabels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 10, hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA),  # Set background to white
        panel.background = element_rect(fill = "white", color = NA)   # Ensure panel is white
  )
plot
plot + geom_hline(yintercept = c(10.5, 14.5, 28.5), 
                  linetype = "solid", color = "black", size = 0.2)
ggsave("Heatmaps/heatmap_coefs-std_JB0218.png")

################# (old key but SNAD only)
##### 11. Coef Heatmap Fully Standardized - in color #####
library(ggplot2)
library(reshape2)
library(dplyr)
library(readr)
library(magrittr)
library(tidyr)
library(ppcor)
library(haven)
library(lmerTest)
library(MuMIn)

# Load the dataset
data <- read_csv("../coefs/snad_coef-std_R_0428.csv")
#View(data)

# Ensure first column (Independent Variables) has unique names
colnames(data)[1] <- "Independent_Variable"

# Reshape data into long format for ggplot
coef_long <- data %>%
  pivot_longer(cols = -Independent_Variable, names_to = "Outcome", values_to = "Coefficient")

# Extract significance markers and convert coefficients to numeric
coef_long <- coef_long %>%
  mutate(Significance = case_when(
    grepl("\\*\\*\\*", Coefficient) ~ "***",
    grepl("\\*\\*", Coefficient) ~ "**",
    grepl("\\*", Coefficient) ~ "*",
    grepl("\\+", Coefficient) ~ "+",
    TRUE ~ ""
  ),
  Coefficient = as.numeric(gsub("[^0-9.-]", "", Coefficient))  # Remove significance markers for plotting
  )

#set up 
ylabels <- c("std_married" = "married",
             "std_parenthood" = "parent",
             "std_working" = "currently working",
             "std_livealone_rev" = "does not live alone",
             "std_grphouse" = "live in group housing",
             "std_anyvolunteer" = "volunteer",
             "std_church_day" = "# days attend church",
             "std_famfriends_day" = "# days visit family/friends",
             "std_smcount" = "# social media sites",
             "std_widowed" = "widowed", 
             "std_anybrevement" = "fam or friend died past yr",
             "std_phassles" = "prop hasslers",
             "std_assisting_adult" = "assisting adult",
             "std_famsupp" = "perceived family suppt",
             "std_frndsupp" = "perceived friend suppt",
             "std_marriage" = "marriage rel. quality",
             "std_family" = "family rel. quality",
             "std_friends" = "friends rel. quality",
             "std_communityscale" = "community integration",
             "std_pclose" = "prop very close",
             "std_msupport3" = "mean # suppt functions",
             "std_pprox" = "prop alters live <30min away",
             "std_ploan" = "prop loan money",
             "std_padvice" = "prop give advice",
             "std_pchores" = "prop help w chores",
             "std_pkin" = "prop kin",
             "std_mfreq" = "mean contact freq",
             "std_diff_race" = "race heterophily",
             "std_diff_college" = "edu heterophily",
             "std_diverse" = "network diversity",
             "std_numphonecontact" = "# phone contacts",
             "std_netsize" = "network size",
             "std_b1density_rev" = "network density - reversed",
             "std_density_rev" = "mean tie strength btwn alters - reversed",
             "std_occcomplex_people_rev" = "occ complexity w people",
             "std_efctsize" = "effective size",
             "std_sole" = "sole bridge status",
             "std_weakest_rev" = "min tie strength - reversed",
             "std_bridging" = "bridging",
             "std_pfriend" = "prop friends",
             "std_agesimilarity_rev" = "age heterophily")
xlabels <- c("std_moca_raw" = "MoCA",
             "std_epmem" = "episodic memory",
             "std_language" = "verbal fluency",
             "std_MCIdementia_rev" = "no MCI or dementia")

#set IV groups
group_lookup <- c(
  "std_married" = "Participation",
  "std_parenthood" = "Participation",
  "std_working" = "Participation",
  "std_livealone_rev" = "Participation",
  "std_grphouse" = "Participation",
  "std_anyvolunteer" = "Participation",
  "std_church_day" = "Participation",
  "std_famfriends_day" = "Participation",
  "std_smcount" = "Participation",
  "std_widowed" = "Stress",
  "std_anybrevement" = "Stress",
  "std_phassles" = "Stress",
  "std_assisting_adult" = "Stress",
  "std_famsupp" = "Bonding",
  "std_frndsupp" = "Bonding",
  "std_marriage" = "Bonding",
  "std_family" = "Bonding",
  "std_friends" = "Bonding",
  "std_communityscale" = "Bonding",
  "std_pclose" = "Bonding",
  "std_msupport3" = "Bonding",
  "std_pprox" = "Bonding",
  "std_ploan" = "Bonding",
  "std_padvice" = "Bonding",
  "std_pchores" = "Bonding",
  "std_pkin" = "Bonding",
  "std_mfreq" = "Bonding",
  "std_diff_race" = "Bridging",
  "std_diff_college" = "Bridging",
  "std_diverse" = "Bridging",
  "std_numphonecontact" = "Bridging",
  "std_netsize" = "Bridging",
  "std_b1density_rev" = "Bridging",
  "std_density_rev" = "Bridging",
  "std_occcomplex_people_rev" = "Bridging",
  "std_efctsize" = "Bridging",
  "std_sole" = "Bridging",
  "std_weakest_rev" = "Bridging",
  "std_bridging" = "Bridging",
  "std_pfriend" = "Bridging",
  "std_agesimilarity_rev" = "Bridging"
)

#orders the IV groups
coef_long <- coef_long %>%
  mutate(Group = group_lookup[Independent_Variable])
coef_long <- coef_long %>%
  mutate(Group = factor(Group, levels = c("Participation", "Stress", "Bonding", "Bridging")))

#orders the dependent variables by average of the abs value of the DV's coef
#outcome_order <- coef_long %>%
#  group_by(Outcome) %>%
#  summarize(AvgAbsCoef = mean(abs(Coefficient), na.rm = TRUE)) %>%
#  arrange(-AvgAbsCoef) %>%
#  pull(Outcome)  # Extract ordered outcome names
#coef_long <- coef_long %>%
#  mutate(Outcome = factor(Outcome, levels = outcome_order))

#order DVs by Brea's request
coef_long <- coef_long %>%
   mutate(Outcome = factor(Outcome, levels = c("std_epmem", 
                                               "std_language", 
                                               "std_moca_raw", 
                                               "std_MCIdementia_rev")))
  
#orders by the average of the absolute value of each IV's 5 coefficients
coef_long <- coef_long %>%
  group_by(Group, Independent_Variable, Outcome) %>%  
  summarize(
    Coefficient = mean(Coefficient, na.rm = TRUE),   # Aggregate coefficients per outcome
    Significance = first(Significance),  # Preserve significance levels
    .groups = "drop"
  ) %>%
  group_by(Group, Independent_Variable) %>%  # Aggregate across outcomes
  mutate(MeanAbsCoef = mean(abs(Coefficient), na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Group, MeanAbsCoef) %>% # - before MeanAbsCoef reverses order
  mutate(Independent_Variable = factor(Independent_Variable, levels = unique(Independent_Variable))) 

# star colors
threshold <- quantile(abs(coef_long$Coefficient), .999)

# group labels position calculation
group_labels <- coef_long %>%
  group_by(Group) %>%
  summarize(y_pos = mean(as.numeric(Independent_Variable)), .groups = "drop")

#red - green color plot
ggplot() +
  geom_tile(data = coef_long, aes(x = Outcome, y = Independent_Variable, fill = Coefficient)) +
  geom_text(data = coef_long, aes(x = Outcome, y = Independent_Variable, label = Significance), 
    color = ifelse(abs(coef_long$Coefficient) > threshold, "white", "black"),
    size = 4, 
    vjust = ifelse(coef_long$Significance == "+", 0.5, 0.8)) +
  scale_fill_gradient2(low = "#8E2A2A", mid = "white", high = "#225E3D", name = "Coefficient") +
  labs(title = "Heatmap of Coefficients with Significance Levels",
       x = "Outcome Variables",
       y = "Independent Variables") +
  scale_x_discrete(labels = xlabels, expand = expansion(mult = c(0.05, 0.3))) +
  scale_y_discrete(labels = ylabels) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.title = element_text(hjust = 0.7),
        legend.margin = margin(t = 0, r = 10, b = 0, l = -10)) +
  geom_segment(data = data.frame(y = c(9.5, 13.5, 27.5)), 
               aes(x = min(as.numeric(coef_long$Outcome)) - 0.5, 
                   xend = max(as.numeric(coef_long$Outcome)) + 0.5, 
                   y = y, yend = y),
               linetype = "solid", color = "black", linewidth = 0.2) +
  geom_text(data = group_labels, 
            aes(x = max(as.numeric(coef_long$Outcome)) + .65, 
                y = y_pos, 
                label = Group),
            angle = 270,
            size = 3,
            fontface = "plain") +
  coord_cartesian(clip = "off") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave("../Heatmaps/heatmap_coefs-std-RG_JB0319.png")

ggsave("../Heatmaps/heatmap_coefs-std-RG_JB0319.tiff")

##### 12. Coef Mini Heatmaps fully standardized
range_of_coefs <- range(coef_long$Coefficient, na.rm = TRUE)
threshold <- quantile(abs(coef_long$Coefficient), .999)

# Participation
coef_long_partic <- coef_long[coef_long$Group == "Participation",]
ggplot() +
  geom_tile(data = coef_long_partic, aes(x = Outcome, y = Independent_Variable, fill = Coefficient)) +
  geom_text(data = coef_long_partic, aes(x = Outcome, y = Independent_Variable, label = Significance), 
            color = ifelse(abs(coef_long_partic$Coefficient) > threshold, "white", "black"),
            size = 4, 
            vjust = ifelse(coef_long_partic$Significance == "+", 0.5, 0.8)) +
  scale_fill_gradient2(low = "#8E2A2A", 
                       mid = "white", 
                       high = "#225E3D", 
                       name = "Coefficient",
                       limits = range_of_coefs) +
  labs(title = "Heatmap of Coefficients with Significance Levels",
       subtitle = "Social Participation",
       x = "Outcome Variables",
       y = "Independent Variables") +
  scale_x_discrete(labels = xlabels, expand = expansion(mult = c(0.05, 0.3))) +
  scale_y_discrete(labels = ylabels) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.title = element_text(hjust = 0.7),
        legend.margin = margin(t = 0, r = 10, b = 0, l = -10)) +
  coord_cartesian(clip = "off") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave("../Heatmaps/heatmap_coefs-std_participation.png")

#Stress
coef_long_stress <- coef_long[coef_long$Group == "Stress",]
ggplot() +
  geom_tile(data = coef_long_stress, aes(x = Outcome, y = Independent_Variable, fill = Coefficient)) +
  geom_text(data = coef_long_stress, aes(x = Outcome, y = Independent_Variable, label = Significance), 
            color = ifelse(abs(coef_long_stress$Coefficient) > threshold, "white", "black"),
            size = 4, 
            vjust = ifelse(coef_long_stress$Significance == "+", 0.5, 0.8)) +
  scale_fill_gradient2(low = "#8E2A2A", 
                       mid = "white", 
                       high = "#225E3D", 
                       name = "Coefficient",
                       limits = range_of_coefs) +
  labs(title = "Heatmap of Coefficients with Significance Levels",
       subtitle = "Social Stressors",
       x = "Outcome Variables",
       y = "Independent Variables") +
  scale_x_discrete(labels = xlabels, expand = expansion(mult = c(0.05, 0.3))) +
  scale_y_discrete(labels = ylabels) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.title = element_text(hjust = 0.7),
        legend.margin = margin(t = 0, r = 10, b = 0, l = -10)) +
  coord_cartesian(clip = "off") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave("../Heatmaps/heatmap_coefs-std_stress.png")

#Bonding
coef_long_bond <- coef_long[coef_long$Group == "Bonding",]
ggplot() +
  geom_tile(data = coef_long_bond, aes(x = Outcome, y = Independent_Variable, fill = Coefficient)) +
  geom_text(data = coef_long_bond, aes(x = Outcome, y = Independent_Variable, label = Significance), 
            color = ifelse(abs(coef_long_bond$Coefficient) > threshold, "white", "black"),
            size = 4, 
            vjust = ifelse(coef_long_bond$Significance == "+", 0.5, 0.8)) +
  scale_fill_gradient2(low = "#8E2A2A", 
                       mid = "white", 
                       high = "#225E3D", 
                       name = "Coefficient",
                       limits = range_of_coefs) +
  labs(title = "Heatmap of Coefficients with Significance Levels",
       subtitle = "Social Bonding",
       x = "Outcome Variables",
       y = "Independent Variables") +
  scale_x_discrete(labels = xlabels, expand = expansion(mult = c(0.05, 0.3))) +
  scale_y_discrete(labels = ylabels) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.title = element_text(hjust = 0.7),
        legend.margin = margin(t = 0, r = 10, b = 0, l = -10)) +
  coord_cartesian(clip = "off") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave("../Heatmaps/heatmap_coefs-std_bonding.png")

#Bridging
coef_long_bridge <- coef_long[coef_long$Group == "Bridging",]
ggplot() +
  geom_tile(data = coef_long_bridge, aes(x = Outcome, y = Independent_Variable, fill = Coefficient)) +
  geom_text(data = coef_long_bridge, aes(x = Outcome, y = Independent_Variable, label = Significance), 
            color = ifelse(abs(coef_long_bridge$Coefficient) > threshold, "white", "black"),
            size = 4, 
            vjust = ifelse(coef_long_bridge$Significance == "+", 0.5, 0.8)) +
  scale_fill_gradient2(low = "#8E2A2A", 
                       mid = "white", 
                       high = "#225E3D", 
                       name = "Coefficient",
                       limits = range_of_coefs) +
  labs(title = "Heatmap of Coefficients with Significance Levels",
       subtitle = "Social Bridging",
       x = "Outcome Variables",
       y = "Independent Variables") +
  scale_x_discrete(labels = xlabels, expand = expansion(mult = c(0.05, 0.3))) +
  scale_y_discrete(labels = ylabels) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.title = element_text(hjust = 0.7),
        legend.margin = margin(t = 0, r = 10, b = 0, l = -10)) +
  coord_cartesian(clip = "off") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave("../Heatmaps/heatmap_coefs-std_bridging.png")


######################################################################
### KEY ############
#######################################################################
##### 13. Coef Heatmap Fully Standardized - in color - COMBINED SNAD/SNACK #####
library(ggplot2)
library(reshape2)
library(dplyr)
library(readr)
library(magrittr)
library(tidyr)
library(ppcor)
library(haven)
library(lmerTest)
library(MuMIn)

# Load the dataset
data <- read_csv("snadsnack_0723_JB.csv")
#View(data)

# Ensure first column (Independent Variables) has unique names
colnames(data)[1] <- "Independent_Variable"

# Reshape data into long format for ggplot
coef_long <- data %>%
  pivot_longer(cols = -Independent_Variable, names_to = "Outcome", values_to = "Coefficient")

# Extract significance markers and convert coefficients to numeric
coef_long <- coef_long %>%
  mutate(Significance = case_when(
    grepl("\\*\\*\\*", Coefficient) ~ "***",
    grepl("\\*\\*", Coefficient) ~ "**",
    grepl("\\*", Coefficient) ~ "*",
    grepl("\\+", Coefficient) ~ "+",
    TRUE ~ ""
  ),
  Coefficient = as.numeric(gsub("[^0-9.-]", "", Coefficient))  # Remove significance markers for plotting
  )

#set up 
ylabels <- c("std_married" = "married",
             "std_parenthood" = "parent",
             "std_working" = "currently working",
             "std_livealone_rev" = "does not live alone",
             "std_grphouse" = "live in group housing",
             "std_anyvolunteer" = "volunteer",
             "std_church_day" = "# days attend church",
             "std_famfriends_day" = "# days visit family/friends",
             "std_smcount" = "# social media sites",
             "std_widowed" = "widowed", 
             "std_anybrevement" = "fam or friend died past yr",
             "std_phassles" = "prop hasslers",
             "std_assisting_adult" = "assisting adult",
             "std_famsupp" = "perceived family suppt",
             "std_frndsupp" = "perceived friend suppt",
             "std_marriage" = "marriage rel. quality",
             "std_family" = "family rel. quality",
             "std_friends" = "friends rel. quality",
             "std_communityscale" = "community integration",
             "std_pclose" = "prop very close",
             "std_msupport3" = "mean # suppt functions",
             "std_pprox" = "prop alters live <30min away",
             "std_ploan" = "prop loan money",
             "std_padvice" = "prop give advice",
             "std_pchores" = "prop help w chores",
             "std_pkin" = "prop kin",
             "std_mfreq" = "mean contact freq",
             "std_diff_white" = "white heterophily",
             "std_diff_college" = "edu heterophily",
             "std_diverse" = "network diversity",
             "std_numphonecontact" = "# phone contacts",
             "std_netsize" = "network size",
             "std_b1density_rev" = "network density - reversed",
             "std_density_rev" = "mean tie strength btwn alters - reversed",
             "std_occcomplex_people_rev" = "occ complexity w people",
             "std_efctsize" = "effective size",
             "std_sole" = "sole bridge status",
             "std_weakest_rev" = "min tie strength - reversed",
             "std_bridging" = "bridging",
             "std_pfriend" = "prop friends",
             "std_avg_euc_age" = "age heterophily")
xlabels <- c("std_moca_raw" = "MoCA",
             "std_epmem" = "episodic memory",
             "std_language" = "verbal fluency",
             "std_MCIdementia_rev" = "no MCI or dementia")

#set IV groups
group_lookup <- c(
  "std_married" = "Participation",
  "std_parenthood" = "Participation",
  "std_working" = "Participation",
  "std_livealone_rev" = "Participation",
  "std_grphouse" = "Participation",
  "std_anyvolunteer" = "Participation",
  "std_church_day" = "Participation",
  "std_famfriends_day" = "Participation",
  "std_smcount" = "Participation",
  "std_widowed" = "Stress",
  "std_anybrevement" = "Stress",
  "std_phassles" = "Stress",
  "std_assisting_adult" = "Stress",
  "std_famsupp" = "Bonding",
  "std_frndsupp" = "Bonding",
  "std_marriage" = "Bonding",
  "std_family" = "Bonding",
  "std_friends" = "Bonding",
  "std_communityscale" = "Bonding",
  "std_pclose" = "Bonding",
  "std_msupport3" = "Bonding",
  "std_pprox" = "Bonding",
  "std_ploan" = "Bonding",
  "std_padvice" = "Bonding",
  "std_pchores" = "Bonding",
  "std_pkin" = "Bonding",
  "std_mfreq" = "Bonding",
  "std_diff_white" = "Bridging",
  "std_diff_college" = "Bridging",
  "std_diverse" = "Bridging",
  "std_numphonecontact" = "Bridging",
  "std_netsize" = "Bridging",
  "std_b1density_rev" = "Bridging",
  "std_density_rev" = "Bridging",
  "std_occcomplex_people_rev" = "Bridging",
  "std_efctsize" = "Bridging",
  "std_sole" = "Bridging",
  "std_weakest_rev" = "Bridging",
  "std_bridging" = "Bridging",
  "std_pfriend" = "Bridging",
  "std_avg_euc_age" = "Bridging"
)

#orders the IV groups
coef_long <- coef_long %>%
  mutate(Group = group_lookup[Independent_Variable])
coef_long <- coef_long %>%
  mutate(Group = factor(Group, levels = c("Participation", "Stress", "Bonding", "Bridging")))

#orders the dependent variables by average of the abs value of the DV's coef
#outcome_order <- coef_long %>%
#  group_by(Outcome) %>%
#  summarize(AvgAbsCoef = mean(abs(Coefficient), na.rm = TRUE)) %>%
#  arrange(-AvgAbsCoef) %>%
#  pull(Outcome)  # Extract ordered outcome names
#coef_long <- coef_long %>%
#  mutate(Outcome = factor(Outcome, levels = outcome_order))

#order DVs by Brea's request
coef_long <- coef_long %>%
  mutate(Outcome = factor(Outcome, levels = c("std_epmem", 
                                              "std_language", 
                                              "std_moca_raw", 
                                              "std_MCIdementia_rev")))

#orders by the average of the absolute value of each IV's 5 coefficients
coef_long <- coef_long %>%
  group_by(Group, Independent_Variable, Outcome) %>%  
  summarize(
    Coefficient = mean(Coefficient, na.rm = TRUE),   # Aggregate coefficients per outcome
    Significance = first(Significance),  # Preserve significance levels
    .groups = "drop"
  ) %>%
  group_by(Group, Independent_Variable) %>%  # Aggregate across outcomes
  mutate(MeanAbsCoef = mean(abs(Coefficient), na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Group, MeanAbsCoef) %>% # - before MeanAbsCoef reverses order
  mutate(Independent_Variable = factor(Independent_Variable, levels = unique(Independent_Variable))) 

# star colors
threshold <- quantile(abs(coef_long$Coefficient), 1)

# group labels position calculation
group_labels <- coef_long %>%
  group_by(Group) %>%
  summarize(y_pos = mean(as.numeric(Independent_Variable)), .groups = "drop")

#red - green color plot
ggplot() +
  geom_tile(data = coef_long, aes(x = Outcome, y = Independent_Variable, fill = Coefficient)) +
  geom_text(data = coef_long, aes(x = Outcome, y = Independent_Variable, label = Significance), 
            color = ifelse(abs(coef_long$Coefficient) > threshold, "white", "black"),
            size = 4, 
            vjust = ifelse(coef_long$Significance == "+", 0.5, 0.8)) +
  scale_fill_gradient2(low = "#8E2A2A", mid = "white", high = "#225E3D", name = "Coefficient") +
  labs(title = "Heatmap of Coefficients with Significance Levels",
       x = "Outcome Variables",
       y = "Independent Variables") +
  scale_x_discrete(labels = xlabels, expand = expansion(mult = c(0.05, 0.3))) +
  scale_y_discrete(labels = ylabels) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.title = element_text(hjust = 0.7),
        legend.margin = margin(t = 0, r = 10, b = 0, l = -10)) +
  geom_segment(data = data.frame(y = c(9.5, 13.5, 27.5)), 
               aes(x = min(as.numeric(coef_long$Outcome)) - 0.5, 
                   xend = max(as.numeric(coef_long$Outcome)) + 0.5, 
                   y = y, yend = y),
               linetype = "solid", color = "black", linewidth = 0.2) +
  geom_text(data = group_labels, 
            aes(x = max(as.numeric(coef_long$Outcome)) + .65, 
                y = y_pos, 
                label = Group),
            angle = 270,
            size = 3,
            fontface = "plain") +
  coord_cartesian(clip = "off") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave("../Heatmaps/heatmap_coefs_SNADSNACKcombined_0723.png")

##############################################################
##### 14. Coef heatmap with combined snad/snack and adjusted p-values #####
library(ggplot2)
library(reshape2)
library(dplyr)
library(readr)
library(magrittr)
library(tidyr)
library(ppcor)
library(haven)
library(lmerTest)
library(MuMIn)

# Load the dataset
data <- read_csv("../coefs/snack_snad_imputed_coef_0513.csv")
#View(data)

# Ensure first column (Independent Variables) has unique names
colnames(data)[1] <- "Independent_Variable"

# Reshape data into long format for ggplot
coef_long <- data %>%
  pivot_longer(cols = -Independent_Variable, names_to = "Outcome", values_to = "Coefficient")

# Extract significance markers and convert coefficients to numeric
coef_long <- coef_long %>%
  mutate(Significance = case_when(
    grepl("\\*\\*\\*", Coefficient) ~ "***",
    grepl("\\*\\*", Coefficient) ~ "**",
    grepl("\\*", Coefficient) ~ "*",
    grepl("\\+", Coefficient) ~ "+",
    TRUE ~ ""
  ),
  Coefficient = as.numeric(gsub("[^0-9.-]", "", Coefficient))  # Remove significance markers for plotting
  )

#set up 
ylabels <- c("std_married" = "married",
             "std_parenthood" = "parent",
             "std_working" = "currently working",
             "std_livealone_rev" = "does not live alone",
             "std_grphouse" = "live in group housing",
             "std_anyvolunteer" = "volunteer",
             "std_church_day" = "# days attend church",
             "std_famfriends_day" = "# days visit family/friends",
             "std_smcount" = "# social media sites",
             "std_widowed" = "widowed", 
             "std_anybrevement" = "fam or friend died past yr",
             "std_phassles" = "prop hasslers",
             "std_assisting_adult" = "assisting adult",
             "std_famsupp" = "perceived family suppt",
             "std_frndsupp" = "perceived friend suppt",
             "std_marriage" = "marriage rel. quality",
             "std_family" = "family rel. quality",
             "std_friends" = "friends rel. quality",
             "std_communityscale" = "community integration",
             "std_pclose" = "prop very close",
             "std_msupport3" = "mean # suppt functions",
             "std_pprox" = "prop alters live <30min away",
             "std_ploan" = "prop loan money",
             "std_padvice" = "prop give advice",
             "std_pchores" = "prop help w chores",
             "std_pkin" = "prop kin",
             "std_mfreq" = "mean contact freq",
             "std_diff_race" = "race heterophily",
             "std_diff_college" = "edu heterophily",
             "std_diverse" = "network diversity",
             "std_numphonecontact" = "# phone contacts",
             "std_netsize" = "network size",
             "std_b1density_rev" = "network density - reversed",
             "std_density_rev" = "mean tie strength btwn alters - reversed",
             "std_occcomplex_people_rev" = "occ complexity w people",
             "std_efctsize" = "effective size",
             "std_sole" = "sole bridge status",
             "std_weakest_rev" = "min tie strength - reversed",
             "std_bridging" = "bridging",
             "std_pfriend" = "prop friends",
             "std_avg_euc_age" = "age heterophily")
xlabels <- c("std_moca_raw" = "MoCA",
             "std_epmem" = "episodic memory",
             "std_language" = "verbal fluency",
             "std_MCIdementia_rev" = "no MCI or dementia")

#set IV groups
group_lookup <- c(
  "std_married" = "Participation",
  "std_parenthood" = "Participation",
  "std_working" = "Participation",
  "std_livealone_rev" = "Participation",
  "std_grphouse" = "Participation",
  "std_anyvolunteer" = "Participation",
  "std_church_day" = "Participation",
  "std_famfriends_day" = "Participation",
  "std_smcount" = "Participation",
  "std_widowed" = "Stress",
  "std_anybrevement" = "Stress",
  "std_phassles" = "Stress",
  "std_assisting_adult" = "Stress",
  "std_famsupp" = "Bonding",
  "std_frndsupp" = "Bonding",
  "std_marriage" = "Bonding",
  "std_family" = "Bonding",
  "std_friends" = "Bonding",
  "std_communityscale" = "Bonding",
  "std_pclose" = "Bonding",
  "std_msupport3" = "Bonding",
  "std_pprox" = "Bonding",
  "std_ploan" = "Bonding",
  "std_padvice" = "Bonding",
  "std_pchores" = "Bonding",
  "std_pkin" = "Bonding",
  "std_mfreq" = "Bonding",
  "std_diff_race" = "Bridging",
  "std_diff_college" = "Bridging",
  "std_diverse" = "Bridging",
  "std_numphonecontact" = "Bridging",
  "std_netsize" = "Bridging",
  "std_b1density_rev" = "Bridging",
  "std_density_rev" = "Bridging",
  "std_occcomplex_people_rev" = "Bridging",
  "std_efctsize" = "Bridging",
  "std_sole" = "Bridging",
  "std_weakest_rev" = "Bridging",
  "std_bridging" = "Bridging",
  "std_pfriend" = "Bridging",
  "std_avg_euc_age" = "Bridging"
)

# remove significance column
head(coef_long)
coef_long <- coef_long %>% dplyr::select(-Significance)
head(coef_long)

#### NEW
# Load the p value data
p <- read_csv("../coefs/snadsnack_0513_p_JB.csv")
#View(data)

# Ensure first column (Independent Variables) has unique names
colnames(p)[1] <- "Independent_Variable"

# Reshape data into long format for ggplot
p_long <- p %>%
  pivot_longer(cols = -Independent_Variable, names_to = "Outcome", values_to = "Pvalue")

p_long <- p_long %>%
  mutate(Significance = case_when(
    Outcome == "std_moca_raw"        & Pvalue < .0013514 ~ "*",
    Outcome == "std_epmem"           & Pvalue < .0013889 ~ "*",
    Outcome == "std_language"        & Pvalue < .0014706 ~ "*",
    Outcome == "std_MCIdementia_rev" & Pvalue < .0013889 ~ "*",
    TRUE ~ ""  # default: no asterisk
  ))
head(p_long)

coef_long <- coef_long %>%
  left_join(
    p_long %>% dplyr::select(Independent_Variable, Outcome, Significance),
    by = c("Independent_Variable", "Outcome")
  )
head(coef_long)
### END NEW

#orders the IV groups
coef_long <- coef_long %>%
  mutate(Group = group_lookup[Independent_Variable])
coef_long <- coef_long %>%
  mutate(Group = factor(Group, levels = c("Participation", "Stress", "Bonding", "Bridging")))

#orders the dependent variables by average of the abs value of the DV's coef
#outcome_order <- coef_long %>%
#  group_by(Outcome) %>%
#  summarize(AvgAbsCoef = mean(abs(Coefficient), na.rm = TRUE)) %>%
#  arrange(-AvgAbsCoef) %>%
#  pull(Outcome)  # Extract ordered outcome names
#coef_long <- coef_long %>%
#  mutate(Outcome = factor(Outcome, levels = outcome_order))

#order DVs by Brea's request
coef_long <- coef_long %>%
  mutate(Outcome = factor(Outcome, levels = c("std_epmem", 
                                              "std_language", 
                                              "std_moca_raw", 
                                              "std_MCIdementia_rev")))

#orders by the average of the absolute value of each IV's 5 coefficients
coef_long <- coef_long %>%
  group_by(Group, Independent_Variable, Outcome) %>%  
  summarize(
    Coefficient = mean(Coefficient, na.rm = TRUE),   # Aggregate coefficients per outcome
    Significance = first(Significance),  # Preserve significance levels
    .groups = "drop"
  ) %>%
  group_by(Group, Independent_Variable) %>%  # Aggregate across outcomes
  mutate(MeanAbsCoef = mean(abs(Coefficient), na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Group, MeanAbsCoef) %>% # - before MeanAbsCoef reverses order
  mutate(Independent_Variable = factor(Independent_Variable, levels = unique(Independent_Variable))) 

# star colors
threshold <- quantile(abs(coef_long$Coefficient), 1)

# group labels position calculation
group_labels <- coef_long %>%
  group_by(Group) %>%
  summarize(y_pos = mean(as.numeric(Independent_Variable)), .groups = "drop")

#red - green color plot
ggplot() +
  geom_tile(data = coef_long, aes(x = Outcome, y = Independent_Variable, fill = Coefficient)) +
  geom_text(data = coef_long, aes(x = Outcome, y = Independent_Variable, label = Significance), 
            color = ifelse(abs(coef_long$Coefficient) > threshold, "white", "black"),
            size = 4, 
            vjust = ifelse(coef_long$Significance == "+", 0.5, 0.8)) +
  scale_fill_gradient2(low = "#8E2A2A", mid = "white", high = "#225E3D", name = "Coefficient") +
  labs(title = "Heatmap of Coefficients with Significance Levels",
       x = "Outcome Variables",
       y = "Independent Variables") +
  scale_x_discrete(labels = xlabels, expand = expansion(mult = c(0.05, 0.3))) +
  scale_y_discrete(labels = ylabels) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.title = element_text(hjust = 0.7),
        legend.margin = margin(t = 0, r = 10, b = 0, l = -10)) +
  geom_segment(data = data.frame(y = c(9.5, 13.5, 27.5)), 
               aes(x = min(as.numeric(coef_long$Outcome)) - 0.5, 
                   xend = max(as.numeric(coef_long$Outcome)) + 0.5, 
                   y = y, yend = y),
               linetype = "solid", color = "black", linewidth = 0.2) +
  geom_text(data = group_labels, 
            aes(x = max(as.numeric(coef_long$Outcome)) + .65, 
                y = y_pos, 
                label = Group),
            angle = 270,
            size = 3,
            fontface = "plain") +
  coord_cartesian(clip = "off") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave("../Heatmaps/heatmap_coefs_SNADSNACKcombined_adjp.png")

##############################################################
##### 15. Coef heatmap with combined snad/snack baseline imputed only #####
library(ggplot2)
library(reshape2)
library(dplyr)
library(readr)
library(magrittr)
library(tidyr)
library(ppcor)
library(haven)
library(lmerTest)
library(MuMIn)

# Load the dataset
data <- read_csv("../coefs/snadsnack_0505_JB.csv")
#View(data)

# Ensure first column (Independent Variables) has unique names
colnames(data)[1] <- "Independent_Variable"

# Reshape data into long format for ggplot
coef_long <- data %>%
  pivot_longer(cols = -Independent_Variable, names_to = "Outcome", values_to = "Coefficient")

# Extract significance markers and convert coefficients to numeric
coef_long <- coef_long %>%
  mutate(Significance = case_when(
    grepl("\\*\\*\\*", Coefficient) ~ "***",
    grepl("\\*\\*", Coefficient) ~ "**",
    grepl("\\*", Coefficient) ~ "*",
    grepl("\\+", Coefficient) ~ "+",
    TRUE ~ ""
  ),
  Coefficient = as.numeric(gsub("[^0-9.-]", "", Coefficient))  # Remove significance markers for plotting
  )

#set up 
ylabels <- c("std_married" = "married",
             "std_parenthood" = "parent",
             "std_working" = "currently working",
             "std_livealone_rev" = "does not live alone",
             "std_grphouse" = "live in group housing",
             "std_anyvolunteer" = "volunteer",
             "std_church_day" = "# days attend church",
             "std_famfriends_day" = "# days visit family/friends",
             "std_smcount" = "# social media sites",
             "std_widowed" = "widowed", 
             "std_anybrevement" = "fam or friend died past yr",
             "std_phassles" = "prop hasslers",
             "std_assisting_adult" = "assisting adult",
             "std_famsupp" = "perceived family suppt",
             "std_frndsupp" = "perceived friend suppt",
             "std_marriage" = "marriage rel. quality",
             "std_family" = "family rel. quality",
             "std_friends" = "friends rel. quality",
             "std_communityscale" = "community integration",
             "std_pclose" = "prop very close",
             "std_msupport3" = "mean # suppt functions",
             "std_pprox" = "prop alters live <30min away",
             "std_ploan" = "prop loan money",
             "std_padvice" = "prop give advice",
             "std_pchores" = "prop help w chores",
             "std_pkin" = "prop kin",
             "std_mfreq" = "mean contact freq",
             "std_diff_race" = "race heterophily",
             "std_diff_college" = "edu heterophily",
             "std_diverse" = "network diversity",
             "std_numphonecontact" = "# phone contacts",
             "std_netsize" = "network size",
             "std_b1density_rev" = "network density - reversed",
             "std_density_rev" = "mean tie strength btwn alters - reversed",
             "std_occcomplex_people_rev" = "occ complexity w people",
             "std_efctsize" = "effective size",
             "std_sole" = "sole bridge status",
             "std_weakest_rev" = "min tie strength - reversed",
             "std_bridging" = "bridging",
             "std_pfriend" = "prop friends",
             "std_avg_euc_age" = "age heterophily")
xlabels <- c("std_moca_raw" = "MoCA",
             "std_epmem" = "episodic memory",
             "std_language" = "verbal fluency",
             "std_MCIdementia_rev" = "no MCI or dementia")

#set IV groups
group_lookup <- c(
  "std_married" = "Participation",
  "std_parenthood" = "Participation",
  "std_working" = "Participation",
  "std_livealone_rev" = "Participation",
  "std_grphouse" = "Participation",
  "std_anyvolunteer" = "Participation",
  "std_church_day" = "Participation",
  "std_famfriends_day" = "Participation",
  "std_smcount" = "Participation",
  "std_widowed" = "Stress",
  "std_anybrevement" = "Stress",
  "std_phassles" = "Stress",
  "std_assisting_adult" = "Stress",
  "std_famsupp" = "Bonding",
  "std_frndsupp" = "Bonding",
  "std_marriage" = "Bonding",
  "std_family" = "Bonding",
  "std_friends" = "Bonding",
  "std_communityscale" = "Bonding",
  "std_pclose" = "Bonding",
  "std_msupport3" = "Bonding",
  "std_pprox" = "Bonding",
  "std_ploan" = "Bonding",
  "std_padvice" = "Bonding",
  "std_pchores" = "Bonding",
  "std_pkin" = "Bonding",
  "std_mfreq" = "Bonding",
  "std_diff_race" = "Bridging",
  "std_diff_college" = "Bridging",
  "std_diverse" = "Bridging",
  "std_numphonecontact" = "Bridging",
  "std_netsize" = "Bridging",
  "std_b1density_rev" = "Bridging",
  "std_density_rev" = "Bridging",
  "std_occcomplex_people_rev" = "Bridging",
  "std_efctsize" = "Bridging",
  "std_sole" = "Bridging",
  "std_weakest_rev" = "Bridging",
  "std_bridging" = "Bridging",
  "std_pfriend" = "Bridging",
  "std_avg_euc_age" = "Bridging"
)

#orders the IV groups
coef_long <- coef_long %>%
  mutate(Group = group_lookup[Independent_Variable])
coef_long <- coef_long %>%
  mutate(Group = factor(Group, levels = c("Participation", "Stress", "Bonding", "Bridging")))

#orders the dependent variables by average of the abs value of the DV's coef
#outcome_order <- coef_long %>%
#  group_by(Outcome) %>%
#  summarize(AvgAbsCoef = mean(abs(Coefficient), na.rm = TRUE)) %>%
#  arrange(-AvgAbsCoef) %>%
#  pull(Outcome)  # Extract ordered outcome names
#coef_long <- coef_long %>%
#  mutate(Outcome = factor(Outcome, levels = outcome_order))

#order DVs by Brea's request
coef_long <- coef_long %>%
  mutate(Outcome = factor(Outcome, levels = c("std_epmem", 
                                              "std_language", 
                                              "std_moca_raw", 
                                              "std_MCIdementia_rev")))

#orders by the average of the absolute value of each IV's 5 coefficients
coef_long <- coef_long %>%
  group_by(Group, Independent_Variable, Outcome) %>%  
  summarize(
    Coefficient = mean(Coefficient, na.rm = TRUE),   # Aggregate coefficients per outcome
    Significance = first(Significance),  # Preserve significance levels
    .groups = "drop"
  ) %>%
  group_by(Group, Independent_Variable) %>%  # Aggregate across outcomes
  mutate(MeanAbsCoef = mean(abs(Coefficient), na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Group, MeanAbsCoef) %>% # - before MeanAbsCoef reverses order
  mutate(Independent_Variable = factor(Independent_Variable, levels = unique(Independent_Variable))) 

# star colors
threshold <- quantile(abs(coef_long$Coefficient), 1)

# group labels position calculation
group_labels <- coef_long %>%
  group_by(Group) %>%
  summarize(y_pos = mean(as.numeric(Independent_Variable)), .groups = "drop")

#red - green color plot
ggplot() +
  geom_tile(data = coef_long, aes(x = Outcome, y = Independent_Variable, fill = Coefficient)) +
  geom_text(data = coef_long, aes(x = Outcome, y = Independent_Variable, label = Significance), 
            color = ifelse(abs(coef_long$Coefficient) > threshold, "white", "black"),
            size = 4, 
            vjust = ifelse(coef_long$Significance == "+", 0.5, 0.8)) +
  scale_fill_gradient2(low = "#8E2A2A", mid = "white", high = "#225E3D", name = "Coefficient") +
  labs(title = "Heatmap of Coefficients with Significance Levels",
       x = "Outcome Variables",
       y = "Independent Variables") +
  scale_x_discrete(labels = xlabels, expand = expansion(mult = c(0.05, 0.3))) +
  scale_y_discrete(labels = ylabels) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.title = element_text(hjust = 0.7),
        legend.margin = margin(t = 0, r = 10, b = 0, l = -10)) +
  geom_segment(data = data.frame(y = c(9.5, 13.5, 27.5)), 
               aes(x = min(as.numeric(coef_long$Outcome)) - 0.5, 
                   xend = max(as.numeric(coef_long$Outcome)) + 0.5, 
                   y = y, yend = y),
               linetype = "solid", color = "black", linewidth = 0.2) +
  geom_text(data = group_labels, 
            aes(x = max(as.numeric(coef_long$Outcome)) + .65, 
                y = y_pos, 
                label = Group),
            angle = 270,
            size = 3,
            fontface = "plain") +
  coord_cartesian(clip = "off") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

##########################################################################
# Cross-sectional snad/snack combined

library(ggplot2)
library(reshape2)
library(dplyr)
library(readr)
library(magrittr)
library(tidyr)
library(ppcor)
library(haven)
library(lmerTest)
library(MuMIn)

# Load the dataset
data <- read_csv("snack_snad_imputed_coef_0514_JB.csv")
#View(data)

# Ensure first column (Independent Variables) has unique names
colnames(data)[1] <- "Independent_Variable"

# Reshape data into long format for ggplot
coef_long <- data %>%
  pivot_longer(cols = -Independent_Variable, names_to = "Outcome", values_to = "Coefficient")

# Extract significance markers and convert coefficients to numeric
coef_long <- coef_long %>%
  mutate(Significance = case_when(
    grepl("\\*\\*\\*", Coefficient) ~ "***",
    grepl("\\*\\*", Coefficient) ~ "**",
    grepl("\\*", Coefficient) ~ "*",
    grepl("\\+", Coefficient) ~ "+",
    TRUE ~ ""
  ),
  Coefficient = as.numeric(gsub("[^0-9.-]", "", Coefficient))  # Remove significance markers for plotting
  )

#set up 
ylabels <- c("std_married" = "married",
             "std_parenthood" = "parent",
             "std_working" = "currently working",
             "std_livealone_rev" = "does not live alone",
             "std_grphouse" = "live in group housing",
             "std_anyvolunteer" = "volunteer",
             "std_church_day" = "# days attend church",
             "std_famfriends_day" = "# days visit family/friends",
             "std_smcount" = "# social media sites",
             "std_widowed" = "widowed", 
             "std_anybrevement" = "fam or friend died past yr",
             "std_phassles" = "prop hasslers",
             "std_assisting_adult" = "assisting adult",
             "std_famsupp" = "perceived family suppt",
             "std_frndsupp" = "perceived friend suppt",
             "std_marriage" = "marriage rel. quality",
             "std_family" = "family rel. quality",
             "std_friends" = "friends rel. quality",
             "std_communityscale" = "community integration",
             "std_pclose" = "prop very close",
             "std_msupport3" = "mean # suppt functions",
             "std_pprox" = "prop alters live <30min away",
             "std_ploan" = "prop loan money",
             "std_padvice" = "prop give advice",
             "std_pchores" = "prop help w chores",
             "std_pkin" = "prop kin",
             "std_mfreq" = "mean contact freq",
             "std_diff_race" = "race heterophily",
             "std_diff_college" = "edu heterophily",
             "std_diverse" = "network diversity",
             "std_numphonecontact" = "# phone contacts",
             "std_netsize" = "network size",
             "std_b1density_rev" = "network density - reversed",
             "std_density_rev" = "mean tie strength btwn alters - reversed",
             "std_occcomplex_people_rev" = "occ complexity w people",
             "std_efctsize" = "effective size",
             "std_sole" = "sole bridge status",
             "std_weakest_rev" = "min tie strength - reversed",
             "std_bridging" = "bridging",
             "std_pfriend" = "prop friends",
             "std_avg_euc_age" = "age heterophily")
xlabels <- c("std_moca_raw" = "MoCA",
             "std_epmem" = "episodic memory",
             "std_language" = "verbal fluency",
             "std_MCIdementia_rev" = "no MCI or dementia")

#set IV groups
group_lookup <- c(
  "std_married" = "Participation",
  "std_parenthood" = "Participation",
  "std_working" = "Participation",
  "std_livealone_rev" = "Participation",
  "std_grphouse" = "Participation",
  "std_anyvolunteer" = "Participation",
  "std_church_day" = "Participation",
  "std_famfriends_day" = "Participation",
  "std_smcount" = "Participation",
  "std_widowed" = "Stress",
  "std_anybrevement" = "Stress",
  "std_phassles" = "Stress",
  "std_assisting_adult" = "Stress",
  "std_famsupp" = "Bonding",
  "std_frndsupp" = "Bonding",
  "std_marriage" = "Bonding",
  "std_family" = "Bonding",
  "std_friends" = "Bonding",
  "std_communityscale" = "Bonding",
  "std_pclose" = "Bonding",
  "std_msupport3" = "Bonding",
  "std_pprox" = "Bonding",
  "std_ploan" = "Bonding",
  "std_padvice" = "Bonding",
  "std_pchores" = "Bonding",
  "std_pkin" = "Bonding",
  "std_mfreq" = "Bonding",
  "std_diff_race" = "Bridging",
  "std_diff_college" = "Bridging",
  "std_diverse" = "Bridging",
  "std_numphonecontact" = "Bridging",
  "std_netsize" = "Bridging",
  "std_b1density_rev" = "Bridging",
  "std_density_rev" = "Bridging",
  "std_occcomplex_people_rev" = "Bridging",
  "std_efctsize" = "Bridging",
  "std_sole" = "Bridging",
  "std_weakest_rev" = "Bridging",
  "std_bridging" = "Bridging",
  "std_pfriend" = "Bridging",
  "std_avg_euc_age" = "Bridging"
)

#orders the IV groups
coef_long <- coef_long %>%
  mutate(Group = group_lookup[Independent_Variable])
coef_long <- coef_long %>%
  mutate(Group = factor(Group, levels = c("Participation", "Stress", "Bonding", "Bridging")))

#orders the dependent variables by average of the abs value of the DV's coef
#outcome_order <- coef_long %>%
#  group_by(Outcome) %>%
#  summarize(AvgAbsCoef = mean(abs(Coefficient), na.rm = TRUE)) %>%
#  arrange(-AvgAbsCoef) %>%
#  pull(Outcome)  # Extract ordered outcome names
#coef_long <- coef_long %>%
#  mutate(Outcome = factor(Outcome, levels = outcome_order))

#order DVs by Brea's request
coef_long <- coef_long %>%
  mutate(Outcome = factor(Outcome, levels = c("std_epmem", 
                                              "std_language", 
                                              "std_moca_raw", 
                                              "std_MCIdementia_rev")))

#orders by the average of the absolute value of each IV's 5 coefficients
coef_long <- coef_long %>%
  group_by(Group, Independent_Variable, Outcome) %>%  
  summarize(
    Coefficient = mean(Coefficient, na.rm = TRUE),   # Aggregate coefficients per outcome
    Significance = first(Significance),  # Preserve significance levels
    .groups = "drop"
  ) %>%
  group_by(Group, Independent_Variable) %>%  # Aggregate across outcomes
  mutate(MeanAbsCoef = mean(abs(Coefficient), na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Group, MeanAbsCoef) %>% # - before MeanAbsCoef reverses order
  mutate(Independent_Variable = factor(Independent_Variable, levels = unique(Independent_Variable))) 

# star colors
threshold <- quantile(abs(coef_long$Coefficient), 1)

# group labels position calculation
group_labels <- coef_long %>%
  group_by(Group) %>%
  summarize(y_pos = mean(as.numeric(Independent_Variable)), .groups = "drop")

#red - green color plot
ggplot() +
  geom_tile(data = coef_long, aes(x = Outcome, y = Independent_Variable, fill = Coefficient)) +
  geom_text(data = coef_long, aes(x = Outcome, y = Independent_Variable, label = Significance), 
            color = ifelse(abs(coef_long$Coefficient) > threshold, "white", "black"),
            size = 4, 
            vjust = ifelse(coef_long$Significance == "+", 0.5, 0.8)) +
  scale_fill_gradient2(low = "#8E2A2A", mid = "white", high = "#225E3D", name = "Coefficient") +
  labs(title = "Heatmap of Coefficients with Significance Levels",
       x = "Outcome Variables",
       y = "Independent Variables") +
  scale_x_discrete(labels = xlabels, expand = expansion(mult = c(0.05, 0.3))) +
  scale_y_discrete(labels = ylabels) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.title = element_text(hjust = 0.7),
        legend.margin = margin(t = 0, r = 10, b = 0, l = -10)) +
  geom_segment(data = data.frame(y = c(9.5, 13.5, 27.5)), 
               aes(x = min(as.numeric(coef_long$Outcome)) - 0.5, 
                   xend = max(as.numeric(coef_long$Outcome)) + 0.5, 
                   y = y, yend = y),
               linetype = "solid", color = "black", linewidth = 0.2) +
  geom_text(data = group_labels, 
            aes(x = max(as.numeric(coef_long$Outcome)) + .65, 
                y = y_pos, 
                label = Group),
            angle = 270,
            size = 3,
            fontface = "plain") +
  coord_cartesian(clip = "off") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave("../Heatmaps/heatmap_coefs_SNADSNACKcombined_cross-sectional-imputed.png")



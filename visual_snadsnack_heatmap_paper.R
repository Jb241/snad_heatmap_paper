## Analysis for SNAD/SNACK heatmap paper
## Created: 09/16/26
## Data used:
  ## cog_heatmap_coefs.csv
  ## mh_heatmap_coefs.csv
  ## from analysis_snadsnack_heatmap_paper.do

# Working directory (set to "Code"):
setwd("/Users/jessicalogin/Desktop/Fall 2026 Working Folder/Code")

# Load packages
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(tibble)


############################
#### Create heatmap function

# Helper function:
read_esttab_csv <- function(file) {
  raw <- read_csv(file, col_names = FALSE,
                  col_types = cols(.default = col_character()))
  raw[] <- lapply(raw, function(x) trimws(gsub('[="]', "", x)))
  header_row <- which(is.na(raw[[1]]) | raw[[1]] == "")[1]  # row with DV names
  names(raw) <- c("Independent_Variable", unlist(raw[header_row, -1]))
  raw[-seq_len(header_row), ] %>%
    filter(!is.na(Independent_Variable), Independent_Variable != "")
}

# Primary function:
make_heatmap <- function(file, dv_labels, title, out_file,
                         width = 7, height = 11) {
  
  coefs <- read_esttab_csv(file) %>%
    pivot_longer(-Independent_Variable, names_to = "Outcome", values_to = "raw") %>%
    mutate(Significance = gsub("[^*+]", "", raw),
           Coefficient  = as.numeric(gsub("[^0-9.-]", "", raw)))
  
  # Warn about anything that won't be plotted
  miss_iv <- setdiff(unique(coefs$Independent_Variable), iv_info$var)
  miss_dv <- setdiff(unique(coefs$Outcome), names(dv_labels))
  if (length(miss_iv)) warning("Not in iv_info (dropped): ", paste(miss_iv, collapse = ", "))
  if (length(miss_dv)) warning("Not in dv_labels (dropped): ", paste(miss_dv, collapse = ", "))
  
  coefs <- coefs %>%
    inner_join(iv_info, by = c("Independent_Variable" = "var")) %>%
    filter(Outcome %in% names(dv_labels)) %>%
    mutate(Group   = factor(group, levels = group_levels),
           Outcome = factor(Outcome, levels = names(dv_labels))) %>%
    group_by(Independent_Variable) %>%
    mutate(MeanAbsCoef = mean(abs(Coefficient), na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(Group, MeanAbsCoef) %>%
    mutate(Independent_Variable = factor(Independent_Variable,
                                         levels = unique(Independent_Variable)))
  
  # Star color and position
  threshold <- 0.6 * max(abs(coefs$Coefficient), na.rm = TRUE)
  coefs <- coefs %>%
    mutate(text_col = ifelse(abs(Coefficient) > threshold, "white", "black"),
           star_vjust = ifelse(Significance == "+", 0.5, 0.8))
  
  # Group label positions and divider lines, computed from the data
  group_pos <- coefs %>%
    distinct(Group, Independent_Variable) %>%
    mutate(y = as.numeric(Independent_Variable)) %>%
    group_by(Group) %>%
    summarize(y_mid = mean(y), y_top = max(y), .groups = "drop")
  dividers <- head(group_pos$y_top, -1) + 0.5
  n_out <- length(dv_labels)
  
  p <- ggplot(coefs, aes(x = Outcome, y = Independent_Variable)) +
    geom_tile(aes(fill = Coefficient)) +
    geom_text(aes(label = Significance, color = text_col, vjust = star_vjust), size = 4) +
    scale_color_identity() +
    scale_fill_gradient2(low = "#8E2A2A", mid = "white", high = "#225E3D",
                         name = "Coefficient") +
    scale_x_discrete(labels = dv_labels, expand = expansion(mult = c(0.05, 0.3))) +
    scale_y_discrete(labels = setNames(iv_info$label, iv_info$var)) +
    annotate("segment", x = 0.5, xend = n_out + 0.5, y = dividers, yend = dividers,
             color = "black", linewidth = 0.2) +
    annotate("text", x = n_out + 0.65, y = group_pos$y_mid,
             label = as.character(group_pos$Group), angle = 270, size = 3) +
    coord_cartesian(clip = "off") +
    labs(title = title, x = "Outcome Variables", y = "Independent Variables") +
    theme_minimal() +
    theme(axis.text.x = element_text(hjust = 0.5),
          plot.background  = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          panel.grid = element_blank(),
          legend.title  = element_text(hjust = 0.7),
          legend.margin = margin(t = 0, r = 10, b = 0, l = -10))
  
  dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
  ggsave(out_file, p, width = width, height = height, bg = "white")
  invisible(p)
}

############################
# Set up information about independent variables
iv_info <- tribble(
  ~var,                     ~label,                                   ~group,
  "std_married",                "married",                                "Participation",
  "std_parenthood",             "parent",                                 "Participation",
  "std_working",                "currently working",                      "Participation",
  "std_livealone_rev",              "does not live alone",                            "Participation",
  "std_grphouse",               "live in group housing",                  "Participation",
  "std_anyvolunteer",           "volunteer",                              "Participation",
  "std_church_day",         "# days attend church",                   "Participation",
  "std_famfriends_day",     "# days visit family/friends",            "Participation",
  "std_smcount",            "# social media sites",                   "Participation",
  "std_widowed",                "widowed",                                "Stress",
  "std_anybrevement",           "fam or friend died past yr",             "Stress",
  "std_phassles",               "prop hasslers",                          "Stress",
  "std_assisting_adult",        "assisting adult",                        "Stress",
  "std_famsupp",            "perceived family suppt",                 "Bonding",
  "std_frndsupp",           "perceived friend suppt",                 "Bonding",
  "std_marriage",           "marriage rel. quality",                  "Bonding",
  "std_family",             "family rel. quality",                    "Bonding",
  "std_friends",            "friends rel. quality",                   "Bonding",
  "std_communityscale_rev",     "community integration",                  "Bonding",
  "std_pclose",             "prop very close",                        "Bonding",
  "std_msupport3",          "mean # suppt functions",                 "Bonding",
  "std_pprox",              "prop alters live <30min away",           "Bonding",
  "std_ploan",              "prop loan money",                        "Bonding",
  "std_padvice",            "prop give advice",                       "Bonding",
  "std_pchores",            "prop help w chores",                     "Bonding",
  "std_pkin",               "prop kin",                               "Bonding",
  "std_mfreq",              "mean contact freq",                      "Bonding",
  "std_diff_race",          "race heterophily",                       "Bridging",
  "std_diff_college",       "edu heterophily",                        "Bridging",
  "std_avg_euc_age",        "age heterophily",                        "Bridging",
  "std_diverse",            "network diversity",                      "Bridging",
  "std_numphonecontact",    "# phone contacts",                       "Bridging",
  "std_netsize",            "network size",                           "Bridging",
  "std_b1density_rev",      "network density - reversed",             "Bridging",
  "std_density_rev",        "mean tie strength btwn alters - reversed","Bridging",
  "std_occcomplex_people",  "occ complexity w people",     "Bridging",
  "std_efctsize",           "effective size",                         "Bridging",
  "std_sole",                   "sole bridge status",                     "Bridging",
  "std_weakest",            "min tie strength - reversed",            "Bridging",
  "std_bridging",           "bridging",                               "Bridging",
  "std_pfriend",            "prop friends",                           "Bridging")

group_levels <- c("Participation", "Stress", "Bonding", "Bridging")

############################
# Cognitive heatmap
make_heatmap(file = "../Output/Coefficients/cog_heatmap_coefs.csv",
             dv_labels = c(std_epmem = "episodic memory",
                           std_language = "verbal fluency",
                           std_moca_raw = "MoCA",
                           MCI_rev = "no MCI or dementia"),
             title = "Cognitive Outcomes",
             out_file = "../Output/Figures/cog_snadsnack_heatmap.png")

# Mental health heatmap
make_heatmap(file = "../Output/Coefficients/mh_heatmap_coefs.csv",
             dv_labels = c(std_gds15_rev = "depression - reversed",
                           std_anxiety_rev = "anxiety - reversed",
                           std_happiness = "happiness",
                           std_whole = "life satisfaction"),
             title = "Mental Health Outcomes",
             out_file = "../Output/Figures/mh_snadsnack_heatmap.png")



# INFO634 26S2 - Group 21 Project
# Part 1: 2021-2025  main models
# Part 2: 2006-2025  twenty-year comparison
# Part 3: 2021-2025  year-by-year view and a data-quality check
# Part 4: 2021-2025  plain-language view and an exposure check

library(ggplot2)
library(caTools)
library(MASS)
library(rpart)
library(randomForest)

PART1FILE <- "CAS_2021_2025_part1.csv"
PART2FILE <- "CAS_2006_2025_trend.csv"
FIGDIR    <- "figures"
if (!dir.exists(FIGDIR)) dir.create(FIGDIR)

# create a color vector for later use
user_cols <- c("Car only" = "#2a78d6", "Motorcyclist" = "#eb6834",
               "Cyclist"  = "#1baf7a", "Pedestrian"   = "#eda100")

# define two functions of vulnerable road users and traffic controls 
label_user <- function(d) {
  for (v in c("bicycle", "motorcycle", "moped", "pedestrian")) {
    x <- suppressWarnings(as.numeric(d[[v]])); x[is.na(x)] <- 0; d[[v]] <- x
  }
  d$VRU_type <- "Car only"
  d$VRU_type[d$motorcycle > 0 | d$moped > 0] <- "Motorcyclist"
  d$VRU_type[d$bicycle    > 0]               <- "Cyclist"
  d$VRU_type[d$pedestrian > 0]               <- "Pedestrian"
  d$VRU_type <- factor(d$VRU_type,
                       levels = c("Car only", "Motorcyclist", "Cyclist", "Pedestrian"))
  d
}

tidy_control <- function(x) {
  y <- rep(NA_character_, length(x))
  y[x == "Traffic Signals"] <- "Traffic signals"
  y[x == "Give way"]        <- "Give-way sign"
  y[x == "Stop"]            <- "Stop sign"
  y[x == "Nil"]             <- "No control"
  factor(y, levels = c("Traffic signals", "Give-way sign", "Stop sign", "No control"))
}

############################################
# PART 1 - the current picture, 2021 to 2025

## ---- 1. data read, cleanse and process ----------------------
#load the csv into crash data frame
crash <- read.csv(PART1FILE, stringsAsFactors = FALSE)
cat("crashes 2021-2025:", nrow(crash), "\n")

crash <- label_user(crash)

# convert any non-numeric or missing entries to 0
for (v in c("truck", "bus")) {
  x <- suppressWarnings(as.numeric(crash[[v]])); x[is.na(x)] <- 0; crash[[v]] <- x
}
crash$heavy_vehicle <- ifelse(crash$truck > 0 | crash$bus > 0, 1, 0)

# define two new columns about severity (serious_fatal and fatal)
crash$serious_fatal <- ifelse(crash$crashSeverity %in% c("Serious Crash", "Fatal Crash"), 1, 0)
crash$fatal         <- ifelse(crash$crashSeverity == "Fatal Crash", 1, 0)

# Standardize traffic control
crash$traffic_control <- tidy_control(crash$trafficControl)

crash$state_highway <- factor(ifelse(crash$crashSHDescription == "Yes", "State highway",
                                     ifelse(crash$crashSHDescription == "No",  "Local road", NA)))

crash$road_surface <- NA
crash$road_surface[crash$roadSurface == "Sealed"] <- "Sealed"
crash$road_surface[crash$roadSurface %in% c("Unsealed", "End of seal")] <- "Unsealed"
crash$road_surface <- factor(crash$road_surface)

rc <- crash$roadCharacter
crash$road_character <- ifelse(rc == "Nil", "Standard road",
                               ifelse(rc == "Bridge", "Bridge",
                                      ifelse(rc == "Motorway ramp", "Motorway ramp",
                                             ifelse(rc %in% c("Overpass", "Underpass", "Tunnel"), "Over/underpass",
                                                    ifelse(rc == "Rail xing", "Rail crossing", "Other feature")))))
crash$road_character <- relevel(factor(crash$road_character), ref = "Standard road")

crash$NumberOfLanes[crash$NumberOfLanes == 0] <- NA
crash$lanes <- cut(crash$NumberOfLanes, breaks = c(0, 1, 2, 20),
                   labels = c("1 lane", "2 lanes", "3+ lanes"))

# Clean outliers in speed limits
crash$speedLimit[crash$speedLimit < 10 | crash$speedLimit > 110] <- NA
crash$speed_band <- cut(crash$speedLimit, breaks = c(0, 40, 50, 80, 120),
                        labels = c("40 km/h or less", "50 km/h", "60-80 km/h", "90 km/h or more"))
crash$speed_band <- relevel(crash$speed_band, ref = "50 km/h")

# classify area
crash$area <- factor(ifelse(crash$urban == "Urban", "Urban",
                            ifelse(crash$urban == "Open",  "Open road", NA)))

crash$light_cond <- NA
crash$light_cond[crash$light %in% c("Bright sun", "Overcast")] <- "Daylight"
crash$light_cond[crash$light == "Twilight"] <- "Twilight"
crash$light_cond[crash$light == "Dark"]     <- "Dark"
crash$light_cond <- factor(crash$light_cond)
#rearrange weather
crash$weather <- NA
crash$weather[crash$weatherA == "Fine"] <- "Fine"
crash$weather[crash$weatherA %in% c("Light rain", "Heavy rain", "Mist or Fog",
                                    "Snow", "Hail or Sleet")] <- "Adverse"
crash$weather <- factor(crash$weather)

crash$terrain <- factor(ifelse(crash$flatHill %in% c("Flat", "Hill Road"), crash$flatHill, NA))
# define a vector containing all the variables needed for downstream analysis
mv <- c("serious_fatal", "VRU_type", "traffic_control", "speed_band", "area",
        "state_highway", "road_surface", "lanes", "road_character",
        "light_cond", "weather", "terrain", "heavy_vehicle")
#drop any row containing missing values
model_data <- na.omit(crash[, mv])
cat("complete cases for modelling:", nrow(model_data), "\n")
cat("serious or fatal:", round(100 * mean(crash$serious_fatal), 2), "%\n")

## ---- 2. descriptive tables --------------------
#calculate the average proportion and convert into percentage
tab_user <- aggregate(cbind(serious_fatal, fatal) ~ VRU_type, data = crash, FUN = mean)
tab_user$n <- as.numeric(table(crash$VRU_type))
tab_user$pct_SF <- round(100 * tab_user$serious_fatal, 1)
tab_user$pct_F  <- round(100 * tab_user$fatal, 2)
print(tab_user[, c("VRU_type", "n", "pct_SF", "pct_F")])

tab_core <- aggregate(serious_fatal ~ traffic_control + VRU_type, data = crash, FUN = mean)
tab_core$pct <- round(100 * tab_core$serious_fatal, 1)
tab_core$n   <- aggregate(serious_fatal ~ traffic_control + VRU_type,
                          data = crash, FUN = length)$serious_fatal
print(tab_core)

## ---- 3. chi-square with effect size -------------
# Cramer's V shows how strong an association is. With 150,000 rows
# nearly everything is "significant", so size matters more than p.
cramers_v <- function(tbl) {
  cs <- suppressWarnings(chisq.test(tbl))
  as.numeric(sqrt(cs$statistic / (sum(tbl) * (min(dim(tbl)) - 1))))
}
chi_vars <- c("VRU_type", "traffic_control", "state_highway", "road_character",
              "road_surface", "lanes", "speed_band", "area", "light_cond",
              "weather", "terrain", "heavy_vehicle")
chi_out <- data.frame()
for (v in chi_vars) {
  tbl <- table(crash[[v]], crash$serious_fatal)
  cs  <- suppressWarnings(chisq.test(tbl))
  chi_out <- rbind(chi_out, data.frame(
    variable = v, chi_sq = round(as.numeric(cs$statistic), 1),
    df = as.numeric(cs$parameter), p_value = cs$p.value,
    cramers_v = round(cramers_v(tbl), 4)))
}
chi_out <- chi_out[order(-chi_out$cramers_v), ]
print(chi_out, row.names = FALSE)

## ---- 4. train / test split ----------------------
set.seed(123)
#set the train and test group to be 0.8 and 0.2
split <- sample.split(model_data$serious_fatal, SplitRatio = 0.8)
train <- model_data[split == TRUE, ]
test  <- model_data[split == FALSE, ]
#check the split
cat("train:", nrow(train), " test:", nrow(test), "\n")

## ---- 5. logistic regression ---------------------
#use training dataset to generate model of how environment affects severe crashes
lmodel1 <- glm(serious_fatal ~ VRU_type + traffic_control + speed_band + area +
                 state_highway + road_surface + lanes + road_character +
                 light_cond + weather + terrain + heavy_vehicle,
               data = train, family = binomial)
summary(lmodel1)

# Model 2 adds the combinations we care about. VRU_type * traffic_control
# asks whether traffic control works differently for different road users.
lmodel2 <- glm(serious_fatal ~ VRU_type * traffic_control + VRU_type * speed_band +
                 area + state_highway + road_surface + lanes + road_character +
                 light_cond + weather + terrain + heavy_vehicle,
               data = train, family = binomial)
summary(lmodel2)

lr <- anova(lmodel1, lmodel2, test = "Chisq")
print(lr)

odds <- data.frame(OR = exp(coef(lmodel1)), exp(confint.default(lmodel1)))
colnames(odds) <- c("OR", "CI_low", "CI_high")
odds$p <- summary(lmodel1)$coefficients[, 4]
print(round(odds, 3))

## --- 6. confusion matrix ---------------------
res <- predict(lmodel2, test, type = "response")

# Only about 8% of crashes are serious. At the usual 0.5 cut-off the
# model would call every crash minor. We use the training rate instead.
cutoff <- mean(train$serious_fatal)
cat("cut-off:", round(cutoff, 3), "\n")

t1 <- table(ActVal = test$serious_fatal, PreVal = res > cutoff)
print(t1)
cat("accuracy   :", round((t1[1,1] + t1[2,2]) / sum(t1), 3), "\n")
cat("sensitivity:", round(t1[2,2] / sum(t1[2, ]), 3), "\n")
cat("specificity:", round(t1[1,1] / sum(t1[1, ]), 3), "\n")
auc_glm <- as.numeric(colAUC(res, test$serious_fatal))
cat("AUC        :", round(auc_glm, 4), "\n")

## ---- 7. adjusted predictions -----------------
#create a baseline scenario grid
grid <- expand.grid(
  VRU_type        = levels(model_data$VRU_type),
  traffic_control = levels(model_data$traffic_control),
  speed_band = "50 km/h", area = "Urban", state_highway = "Local road",
  road_surface = "Sealed", lanes = "2 lanes", road_character = "Standard road",
  light_cond = "Daylight", weather = "Fine", terrain = "Flat", heavy_vehicle = 0)
#calculate predicted probability
grid$pred_pct <- round(100 * predict(lmodel2, grid, type = "response"), 1)
adjusted <- reshape(grid[, c("traffic_control", "VRU_type", "pred_pct")],
                    idvar = "traffic_control", timevar = "VRU_type", direction = "wide")
print(adjusted, row.names = FALSE)

## --- 8. feature selection -------------
#evaluate predictor importance
print(drop1(lmodel1, test = "Chisq"))
lmodel_aic <- stepAIC(lmodel1, direction = "both", trace = FALSE)
print(formula(lmodel_aic))
print(AIC(lmodel1, lmodel2, lmodel_aic))

## ---- 9. decision tree ---------------------
#prepare target variables
train$sf_factor <- factor(train$serious_fatal, labels = c("Minor", "Serious"))
test$sf_factor  <- factor(test$serious_fatal,  labels = c("Minor", "Serious"))
#make sure of reproducibility
set.seed(123)
#generate a tree
mytree <- rpart(sf_factor ~ VRU_type + traffic_control + speed_band + area +
                  state_highway + road_surface + lanes + light_cond +
                  weather + terrain + heavy_vehicle,
                data = train, method = "class",
                control = rpart.control(cp = 0.0005, minbucket = 50))

# prune back to the size with the lowest cross-validation error
best_cp <- mytree$cptable[which.min(mytree$cptable[, "xerror"]), "CP"]
prune_tree <- prune(mytree, cp = best_cp)
cat("leaves after pruning:", sum(prune_tree$frame$var == "<leaf>"), "\n")
#evaluate performance by test dataset
tree_prob <- predict(prune_tree, test)[, "Serious"]
t2 <- table(ActVal = test$sf_factor, PreVal = tree_prob > cutoff)
print(t2)
auc_tree <- as.numeric(colAUC(tree_prob, test$serious_fatal))
cat("tree accuracy:", round(sum(diag(t2)) / sum(t2), 3), " AUC:", round(auc_tree, 4), "\n")
#export and save
options(scipen = 999)
png(file.path(FIGDIR, "fig5_tree.png"), width = 1800, height = 1000, res = 190)
par(mar = c(1, 1, 3.5, 1), xpd = NA)
plot(prune_tree, uniform = TRUE, compress = TRUE, branch = 0.4, margin = 0.08,
     main = "Decision tree: which crashes end up serious or fatal")
text(prune_tree, use.n = TRUE, all = FALSE, cex = 0.78, pretty = 0, font = 1)
dev.off()

## --- 10. random forest ------------------
# Serious crashes are rare, so we draw an equal number of serious and
# minor crashes into every tree. Without this the forest finds nothing.
n_pos <- sum(train$sf_factor == "Serious")
#make sure of reproducibility
set.seed(123)
#generate the random forest
rf <- randomForest(sf_factor ~ VRU_type + traffic_control + speed_band + area +
                     state_highway + road_surface + lanes + light_cond +
                     weather + terrain + heavy_vehicle,
                   data = train, ntree = 300, nodesize = 50,
                   strata = train$sf_factor,
                   sampsize = c(Minor = n_pos, Serious = n_pos))
print(rf)
#extract and rank feature importance
imp <- data.frame(variable = rownames(importance(rf)),
                  gini = as.numeric(importance(rf)))
imp <- imp[order(-imp$gini), ]
print(imp, row.names = FALSE)
#evaluate performance by test data set
rf_prob <- predict(rf, test, type = "prob")[, "Serious"]
t3 <- table(ActVal = test$sf_factor, PreVal = rf_prob > 0.5)
print(t3)
auc_rf <- as.numeric(colAUC(rf_prob, test$serious_fatal))
cat("forest accuracy:", round(sum(diag(t3)) / sum(t3), 3), " AUC:", round(auc_rf, 4), "\n")
#cross model comparison
cat("\n--- model comparison ---\n")
cat("logistic regression AUC:", round(auc_glm, 4), "\n")
cat("decision tree       AUC:", round(auc_tree, 4), "\n")
cat("random forest       AUC:", round(auc_rf,  4), "\n")

## --- 11. how concentrated is the harm? --------------
vru <- crash[crash$VRU_type != "Car only" & crash$serious_fatal == 1, ]
vru <- vru[!is.na(vru$X) & !is.na(vru$Y) & vru$X > 1050000 & vru$X < 2100000 &
             vru$Y > 4700000 & vru$Y < 6250000, ]
vru$cell <- paste(floor(vru$X / 1000), floor(vru$Y / 1000))
cells <- sort(table(vru$cell), decreasing = TRUE)
cat("serious VRU crashes:", nrow(vru), " in", length(cells), "one-km squares\n")
for (n in c(10, 50, 100, 250, 500)) {
  cat("  worst", n, "squares hold",
      round(100 * sum(head(cells, n)) / sum(cells), 1), "%\n")
}

###########################################################
# PART 2 - the twenty-year comparison, 2006 to 2025

hist <- read.csv(PART2FILE, stringsAsFactors = FALSE)
hist <- label_user(hist)
hist$serious_fatal <- ifelse(hist$crashSeverity %in% c("Serious Crash", "Fatal Crash"), 1, 0)
hist$traffic_control <- tidy_control(hist$trafficControl)
hist$era <- cut(hist$crashYear, breaks = c(2005, 2010, 2015, 2020, 2025),
                labels = c("2006-10", "2011-15", "2016-20", "2021-25"))
cat("\ntwenty-year panel:", nrow(hist), "crashes\n")

# how much safer are traffic signals than no control, for each group?
gap_tab <- data.frame()
for (e in levels(hist$era)) {
  for (u in levels(hist$VRU_type)) {
    s <- hist[hist$era == e & hist$VRU_type == u &
                hist$traffic_control %in% c("Traffic signals", "No control"), ]
    sig <- mean(s$serious_fatal[s$traffic_control == "Traffic signals"])
    non <- mean(s$serious_fatal[s$traffic_control == "No control"])
    gap_tab <- rbind(gap_tab, data.frame(era = e, VRU_type = u,
                                         gap_pp = round(100 * (non - sig), 1)))
  }
}
gap_tab$VRU_type <- factor(gap_tab$VRU_type, levels = names(user_cols))
print(reshape(gap_tab, idvar = "era", timevar = "VRU_type", direction = "wide"),
      row.names = FALSE)

# counts of serious and fatal crashes, no adjustment at all
print(xtabs(serious_fatal ~ era + VRU_type, data = hist))

# is the pedestrian difference significant in every period?
for (e in levels(hist$era)) {
  s <- hist[hist$era == e & hist$traffic_control %in% c("Traffic signals", "No control"), ]
  s$traffic_control <- droplevels(s$traffic_control)
  m <- glm(serious_fatal ~ VRU_type * traffic_control, data = s, family = binomial)
  cf <- summary(m)$coefficients
  k <- grep("Pedestrian:traffic_controlNo control", rownames(cf))
  cat(e, " pedestrian x no-control OR =", round(exp(cf[k, 1]), 3),
      " p =", format.pval(cf[k, 4], digits = 3), "\n")
}

# the same check using injury crashes only, to remove the effect of
# changes in how non-injury crashes are reported
inj <- hist[hist$crashSeverity != "Non-Injury Crash", ]
cat("\ninjury-only check, n =", nrow(inj), "\n")
for (e in levels(inj$era)) {
  s <- inj[inj$era == e & inj$traffic_control %in% c("Traffic signals", "No control"), ]
  s$traffic_control <- droplevels(s$traffic_control)
  m <- glm(serious_fatal ~ VRU_type * traffic_control, data = s, family = binomial)
  cf <- summary(m)$coefficients
  k <- grep("Pedestrian:traffic_controlNo control", rownames(cf))
  cat(e, " OR =", round(exp(cf[k, 1]), 3), " p =", format.pval(cf[k, 4], digits = 3), "\n")
}

#############################################
# FIGURES

base_theme <- theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(colour = "grey35", size = 9),
        panel.grid.minor = element_blank())

## Figure 1 - the multivariate graphic
p1 <- tab_core[!is.na(tab_core$traffic_control), ]
fig1 <- ggplot(p1, aes(x = traffic_control, y = pct, fill = VRU_type)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = paste0(pct, "%")), vjust = -0.45, size = 3) +
  facet_wrap(~ VRU_type, scales = "free_y", nrow = 1) +
  scale_fill_manual(values = user_cols) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Traffic control makes crashes less serious for drivers and motorcyclists,\nbut not for people on foot",
       subtitle = "New Zealand CAS crashes, 2021-2025. Each panel has its own scale.",
       x = NULL, y = "Crashes that were serious or fatal (%)") +
  base_theme + theme(legend.position = "none",
                     axis.text.x = element_text(angle = 30, hjust = 1, size = 8))
ggsave(file.path(FIGDIR, "fig1_control_by_user.png"), fig1, width = 11, height = 4, dpi = 200)

## Figure 2 - speed limit
spd <- aggregate(serious_fatal ~ speed_band + VRU_type, data = crash, FUN = mean)
spd$pct <- round(100 * spd$serious_fatal, 1)
spd$speed_band <- factor(spd$speed_band,
                         levels = c("40 km/h or less", "50 km/h", "60-80 km/h", "90 km/h or more"))
fig2 <- ggplot(spd, aes(x = speed_band, y = pct, colour = VRU_type, group = VRU_type)) +
  geom_line(linewidth = 1.1) + geom_point(size = 3) +
  geom_text(aes(label = paste0(pct, "%")), vjust = -1, size = 2.9, show.legend = FALSE) +
  scale_colour_manual(values = user_cols) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(title = "Higher speed limits make every crash worse,\nbut vulnerable road users start far higher",
       subtitle = "New Zealand CAS crashes, 2021-2025",
       x = "Posted speed limit", y = "Crashes that were serious or fatal (%)", colour = "Road user") +
  base_theme
ggsave(file.path(FIGDIR, "fig2_speed_by_user.png"), fig2, width = 7.5, height = 4.5, dpi = 200)

## Figure 3 - odds ratios
lab <- c("VRU_typeMotorcyclist" = "Motorcyclist", "VRU_typeCyclist" = "Cyclist",
         "VRU_typePedestrian" = "Pedestrian", "speed_band90 km/h or more" = "90+ km/h zone",
         "speed_band60-80 km/h" = "60-80 km/h zone", "speed_band40 km/h or less" = "40 km/h or less zone",
         "traffic_controlNo control" = "No traffic control", "traffic_controlStop sign" = "Stop sign",
         "traffic_controlGive-way sign" = "Give-way sign", "heavy_vehicle" = "Truck or bus involved",
         "road_surfaceUnsealed" = "Unsealed road", "lanes2 lanes" = "2 lanes", "lanes3+ lanes" = "3+ lanes",
         "road_characterRail crossing" = "Rail crossing", "road_characterBridge" = "Bridge",
         "road_characterMotorway ramp" = "Motorway ramp", "road_characterOver/underpass" = "Over/underpass",
         "terrainHill Road" = "Hill road", "areaUrban" = "Urban area",
         "state_highwayState highway" = "State highway", "weatherFine" = "Fine weather",
         "light_condDaylight" = "Daylight")
or_df <- odds[rownames(odds) %in% names(lab), ]
or_df$term <- lab[rownames(or_df)]
or_df <- or_df[order(or_df$OR), ]
or_df$term <- factor(or_df$term, levels = or_df$term)
or_df$dir <- ifelse(or_df$OR > 1, "More serious", "Less serious")

fig3 <- ggplot(or_df, aes(x = OR, y = term, colour = dir)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey45") +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0, linewidth = 0.9) +
  geom_point(size = 2.6) +
  geom_text(aes(x = CI_high, label = sprintf("%.2f", OR)), hjust = -0.25, size = 2.9, show.legend = FALSE) +
  scale_colour_manual(values = c("More serious" = "#e34948", "Less serious" = "#2a78d6")) +
  scale_x_log10(limits = c(0.3, 22), breaks = c(0.5, 1, 2, 5, 10)) +
  labs(title = "What makes a crash serious",
       subtitle = paste0("Logistic regression, ", nrow(train), " crashes. Dot is the odds ratio, line is the 95% confidence interval."),
       x = "Odds ratio (log scale)", y = NULL, colour = NULL) +
  base_theme + theme(legend.position = "top")
ggsave(file.path(FIGDIR, "fig3_odds_ratios.png"), fig3, width = 7.5, height = 7, dpi = 200)

## Figure 4 - the twenty-year trend
fig4 <- ggplot(gap_tab, aes(x = era, y = gap_pp, colour = VRU_type, group = VRU_type)) +
  geom_hline(yintercept = 0, colour = "grey70") +
  geom_line(linewidth = 1.1) + geom_point(size = 3) +
  scale_colour_manual(values = user_cols) +
  labs(title = "Signals help every group more than before - except people on foot",
       subtitle = "How much safer a signalised site is than an uncontrolled one, in percentage points.\nHigher is better. Cyclist numbers at signals are small, so that line moves about.",
       x = NULL, y = "Percentage points safer", colour = "Road user") +
  base_theme
ggsave(file.path(FIGDIR, "fig4_twenty_year_gap.png"), fig4, width = 8, height = 4.6, dpi = 200)

## Figure 6 - ROC curves for the three models
roc_pts <- function(score, actual, label) {
  o <- order(-score); a <- actual[o]
  data.frame(fpr = cumsum(a == 0) / sum(a == 0),
             tpr = cumsum(a == 1) / sum(a == 1), model = label)
}
roc_all <- rbind(
  roc_pts(res,       test$serious_fatal, sprintf("Logistic regression (AUC %.3f)", auc_glm)),
  roc_pts(tree_prob, test$serious_fatal, sprintf("Decision tree (AUC %.3f)", auc_tree)),
  roc_pts(rf_prob,   test$serious_fatal, sprintf("Random forest (AUC %.3f)", auc_rf)))

fig6 <- ggplot(roc_all, aes(x = fpr, y = tpr, colour = model)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey60") +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = c("#2a78d6", "#eb6834", "#1baf7a")) +
  coord_equal() +
  labs(title = "All three models perform about the same",
       subtitle = "So we report the one we can explain: logistic regression.",
       x = "False positive rate", y = "True positive rate", colour = NULL) +
  base_theme + theme(legend.position = c(0.62, 0.18),
                     legend.background = element_rect(fill = "white", colour = NA))
ggsave(file.path(FIGDIR, "fig6_roc.png"), fig6, width = 5.5, height = 5.5, dpi = 200)

## Figure 7 - where the serious crashes are
fig7 <- ggplot(vru, aes(x = X, y = Y)) +
  geom_point(alpha = 0.22, size = 0.65, colour = "#2a78d6") +
  coord_equal() +
  labs(title = "Serious and fatal crashes involving vulnerable road users",
       subtitle = paste0(nrow(vru), " crashes, 2021-2025, spread across ",
                         length(cells), " one-kilometre squares")) +
  theme_void(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(colour = "grey35", size = 9))
ggsave(file.path(FIGDIR, "fig7_map.png"), fig7, width = 5, height = 6.5, dpi = 200)

#########################################
# PART 3 - year by year, 2021 to 2025

## ---- 12. severity by year -----------------
sev_year <- xtabs(~ crashYear + crashSeverity, data = crash)
print(sev_year)
print(round(100 * prop.table(sev_year, 1), 2))
sf_year <- tapply(crash$serious_fatal, crash$crashYear, sum)
cat("serious + fatal per year:\n"); print(sf_year)

## --- 13. vulnerable road users involved, by year ----------
# Here a crash is counted once for every kind of vulnerable road user
# in it, so a crash with a cyclist and a pedestrian counts in both.
inv <- sapply(c("pedestrian", "motorcycle", "bicycle", "moped"),
              function(v) tapply(crash[[v]] > 0, crash$crashYear, sum))
print(inv)
n_types <- (crash$bicycle > 0) + (crash$moped > 0) + (crash$motorcycle > 0) + (crash$pedestrian > 0)
cat("crashes with two or more kinds of VRU:", sum(n_types > 1), "\n")
cat("share of all crashes involving a VRU (%):\n")
print(round(100 * tapply(n_types > 0, crash$crashYear, mean), 1))

## ---- 14. traffic control by year -----------------
tc_year <- xtabs(~ crashYear + trafficControl, data = crash)
print(tc_year)
print(round(100 * prop.table(tc_year, 1), 2))

# "Unknown" crashes are mostly minor and mostly car-only
u <- crash$trafficControl == "Unknown"
cat("Unknown:", sum(u), "crashes, serious or fatal", round(100 * mean(crash$serious_fatal[u]), 1),
    "% (others", round(100 * mean(crash$serious_fatal[!u]), 1), "%)\n")
print(table(crash$VRU_type[u]))

## --- 15. check the main result without the "Unknown" years -------
# Almost all "Unknown" traffic control is in 2021-2022. Re-fit the
# interaction model on 2023-2025 only, where it is close to zero.
md3 <- na.omit(crash[crash$crashYear >= 2023, mv])
cat("complete cases 2023-2025:", nrow(md3), "\n")
f3 <- serious_fatal ~ VRU_type + traffic_control + speed_band + area + state_highway +
  road_surface + lanes + road_character + light_cond + weather + terrain + heavy_vehicle
m3a <- glm(f3, data = md3, family = binomial)
m3b <- glm(update(f3, . ~ . + VRU_type:traffic_control + VRU_type:speed_band),
           data = md3, family = binomial)
print(anova(m3a, m3b, test = "Chisq"))
cf3 <- summary(m3b)$coefficients
for (k in grep("Pedestrian:traffic_control", rownames(cf3), value = TRUE))
  cat(k, " OR =", round(exp(cf3[k, 1]), 3), " p =", format.pval(cf3[k, 4], digits = 3), "\n")
grid$pred_2023 <- round(100 * predict(m3b, grid, type = "response"), 1)
print(reshape(grid[, c("traffic_control", "VRU_type", "pred_2023")],
              idvar = "traffic_control", timevar = "VRU_type", direction = "wide"), row.names = FALSE)

## Figure - severity by year
sev_df <- as.data.frame(sev_year)
names(sev_df) <- c("Year", "Severity", "Crashes")
sev_df$Severity <- factor(sub(" Crash", "", sev_df$Severity),
                          levels = c("Non-Injury", "Minor", "Serious", "Fatal"))
tot <- aggregate(Crashes ~ Year, data = sev_df, FUN = sum)
sf_df <- data.frame(Year = names(sf_year), sf = as.numeric(sf_year),
                    pct = round(100 * as.numeric(sf_year) / tot$Crashes, 1))

figA <- ggplot(sev_df, aes(x = Year, y = Crashes, fill = Severity)) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.3) +
  geom_text(data = tot, aes(x = Year, y = Crashes, label = format(Crashes, big.mark = ",")),
            inherit.aes = FALSE, vjust = -0.5, size = 3.2) +
  geom_text(data = sf_df, aes(x = Year, y = sf / 2, label = format(sf, big.mark = ",")),
            inherit.aes = FALSE, size = 3, colour = "white", fontface = "bold") +
  scale_fill_manual(values = c("Non-Injury" = "#86b6ef", "Minor" = "#3987e5",
                               "Serious" = "#1c5cab", "Fatal" = "#0d366b"),
                    breaks = c("Fatal", "Serious", "Minor", "Non-Injury")) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ","),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Fewer crashes are reported each year, but serious harm has not fallen",
       subtitle = "Crashes by severity, 2021-2025. White label = serious and fatal crashes combined.",
       x = NULL, y = "Crashes", fill = "Severity") +
  base_theme
ggsave(file.path(FIGDIR, "figA_severity_by_year.png"), figA, width = 7.5, height = 4.6, dpi = 200)

## Figure - vulnerable road users by year
inv_df <- data.frame(Year = rep(rownames(inv), times = ncol(inv)),
                     Roaduser = rep(c("Pedestrian", "Motorcycle", "Bicycle", "Moped"), each = nrow(inv)),
                     Crashes = as.vector(inv))
inv_df$Roaduser <- factor(inv_df$Roaduser, levels = c("Motorcycle", "Pedestrian", "Bicycle", "Moped"))
lab_df <- inv_df[inv_df$Year %in% c("2021", "2025"), ]

figB <- ggplot(inv_df, aes(x = Year, y = Crashes, colour = Roaduser, group = Roaduser)) +
  geom_line(linewidth = 1.1) + geom_point(size = 3) +
  geom_text(data = lab_df, aes(label = format(Crashes, big.mark = ",")),
            vjust = -1, size = 2.9, show.legend = FALSE) +
  scale_colour_manual(values = c("Motorcycle" = "#eb6834", "Pedestrian" = "#eda100",
                                 "Bicycle" = "#1baf7a", "Moped" = "#e87ba4")) +
  scale_y_continuous(limits = c(0, 1700), labels = function(x) format(x, big.mark = ",")) +
  labs(title = "Motorcycle crashes are rising while every other group is flat or falling",
       subtitle = "Crashes involving each kind of vulnerable road user, 2021-2025",
       x = NULL, y = "Crashes", colour = "Road user") +
  base_theme
ggsave(file.path(FIGDIR, "figB_vru_by_year.png"), figB, width = 7.5, height = 4.6, dpi = 200)

## Figure - traffic control by year
tc_df <- as.data.frame(tc_year)
names(tc_df) <- c("Year", "Control", "Crashes")
tc_df$Control <- as.character(tc_df$Control)
tc_df$Control[tc_df$Control %in% c("Pointsman", "School Patrol/warden",
                                   "Isolated Pedestrian signal (non-intersection)")] <- "Other"
tc_df$Control[tc_df$Control == "Nil"]             <- "No control"
tc_df$Control[tc_df$Control == "Give way"]        <- "Give-way sign"
tc_df$Control[tc_df$Control == "Stop"]            <- "Stop sign"
tc_df$Control[tc_df$Control == "Traffic Signals"] <- "Traffic signals"
tc_df <- aggregate(Crashes ~ Year + Control, data = tc_df, FUN = sum)
tc_df$Control <- factor(tc_df$Control, levels = c("Other", "No control", "Give-way sign",
                                                  "Stop sign", "Traffic signals", "Unknown"))
unk <- tc_df[tc_df$Control == "Unknown" & tc_df$Crashes > 500, ]

figC <- ggplot(tc_df, aes(x = Year, y = Crashes, fill = Control)) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.3) +
  geom_text(data = unk, aes(x = Year, y = Crashes / 2, label = format(Crashes, big.mark = ",")),
            inherit.aes = FALSE, colour = "white", fontface = "bold", size = 3) +
  scale_fill_manual(values = c("Unknown" = "#e34948", "Traffic signals" = "#2a78d6",
                               "Stop sign" = "#1baf7a", "Give-way sign" = "#eda100",
                               "No control" = "#c9c8c2", "Other" = "#4a3aa7"),
                    breaks = c("Other", "No control", "Give-way sign", "Stop sign",
                               "Traffic signals", "Unknown")) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ","),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "'Unknown' traffic control almost disappears after 2022",
       subtitle = "Crashes by traffic control at the site, 2021-2025. Unknown (red): 2,489 in 2021, 1,034 in 2022,\nand fewer than 50 a year after that.",
       x = NULL, y = "Crashes", fill = "Traffic control") +
  base_theme
ggsave(file.path(FIGDIR, "figC_control_by_year.png"), figC, width = 7.5, height = 4.6, dpi = 200)

########################################################
# PART 4 - plain-language view and an exposure check

## --- 16. out of every 100 crashes ------------
# The core result as simple counts: of every 100 crashes of each kind,
# how many end in serious injury or death, at signals and with no control.
per100 <- tab_core[tab_core$traffic_control %in% c("Traffic signals", "No control"),
                   c("traffic_control", "VRU_type", "pct", "n")]
per100 <- reshape(per100, idvar = "VRU_type", timevar = "traffic_control", direction = "wide")
names(per100) <- c("VRU_type", "signals", "n_signals", "none", "n_none")
per100$difference <- round(per100$none - per100$signals, 1)
print(per100, row.names = FALSE)

## --- 17. an exposure proxy: relative involvement ---------
# We have no count of how many people walk or cycle past each kind of
# site. As a rough proxy for how busy a site type is, we use the number
# of car-only crashes there, and ask how many crashes involve each
# vulnerable road user for every 1,000 car-only crashes. A higher ratio
# means that group is more often involved at that kind of site, which
# is what we would expect where more of them are present.
cnt <- xtabs(~ traffic_control + VRU_type, data = crash)
inv_ratio <- round(1000 * cnt[, c("Motorcyclist", "Cyclist", "Pedestrian")] / cnt[, "Car only"], 1)
print(cnt)
print(inv_ratio)

## Figure - out of every 100 crashes (dumbbell)
p100 <- per100
p100$VRU_type <- factor(p100$VRU_type, levels = rev(c("Motorcyclist", "Cyclist", "Car only", "Pedestrian")))
figD <- ggplot(p100, aes(y = VRU_type)) +
  geom_segment(aes(x = signals, xend = none, yend = VRU_type), colour = "#c9c8c2", linewidth = 3) +
  geom_point(aes(x = signals, colour = "Traffic signals"), size = 5) +
  geom_point(aes(x = none, colour = "No traffic control"), size = 5) +
  geom_text(aes(x = signals, label = round(signals)), vjust = -1.3, size = 3.4, colour = "#2a78d6", fontface = "bold") +
  geom_text(aes(x = none, label = round(none)), vjust = -1.3, size = 3.4, colour = "#52514e", fontface = "bold") +
  geom_text(aes(x = 44, label = paste0(ifelse(difference >= 0, "+", ""), round(difference), " per 100")),
            hjust = 0, size = 3.3, colour = "#0b0b0b") +
  scale_colour_manual(values = c("Traffic signals" = "#2a78d6", "No traffic control" = "#52514e")) +
  scale_x_continuous(limits = c(0, 52), breaks = seq(0, 40, 10)) +
  labs(title = "Out of every 100 crashes, how many end in serious injury or death?",
       subtitle = "Traffic signals compared with no traffic control, 2021-2025. Right-hand column = difference.",
       x = "Serious or fatal crashes per 100 crashes", y = NULL, colour = NULL) +
  base_theme + theme(legend.position = "top", panel.grid.major.y = element_blank(),
                     axis.text.y = element_text(size = 11, colour = "#0b0b0b"))
ggsave(file.path(FIGDIR, "figD_per100_planner.png"), figD, width = 7.5, height = 4.2, dpi = 200)

cat("\nfigures written to", FIGDIR, "\n")
sessionInfo()

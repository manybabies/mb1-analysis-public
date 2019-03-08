
d_var_calc <- function(n, d) {
  return((2/n) + (d ^ 2 / (4 * n)))
}

# ########### SCATTER PLOT ###########
# 
# scatter <- function() {
#   req(input$scatter_curve)
#   
#   labels <- if (mod_group() == "all_mod") NULL else
#     setNames(paste(mod_data()[[mod_group()]], "  "), mod_data()[[mod_group()]])
#   
#   guide <- if (mod_group() == "all_mod") FALSE else "legend"
#   p <- ggplot(mod_data(), aes_string(x = "mean_age_months", y = es(),
#                                      colour = mod_group())) +
#     geom_jitter(aes(size = n), alpha = 0.5) +
#     geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
#     scale_colour_solarized(name = "", labels = labels, guide = guide) +
#     scale_size_continuous(guide = FALSE) +
#     xlab("\nMean Subject Age (Months)") +
#     ylab("Effect Size\n")
#   
#   #curve <- if (is.null(categorical_mods())) input$scatter_curve else "lm"
#   if (input$scatter_curve == "lm") {
#     p + geom_smooth(aes_string(weight = sprintf("1 / %s", es_var())),
#                     method = "lm", se = FALSE)
#   } else if (input$scatter_curve == "loess") {
#     p + geom_smooth(aes_string(weight = sprintf("1 / %s", es_var())),
#                     method = "loess", se = FALSE, span = 1)
#   }
#   
# }
# 
# output$scatter <- renderPlot(scatter())
# 
# output$longitudinal <- reactive({
#   req(input$dataset_name)
#   filter(datasets, name == input$dataset_name)$longitudinal
# })
# 
# outputOptions(output, "longitudinal", suspendWhenHidden = FALSE)
# 
# ########### VIOLIN PLOT ###########
# 
# violin <- function() {
#   plt_data <- mod_data()
#   mod_factor <- factor(plt_data[[mod_group()]])
#   plt_data[[mod_group()]] <- factor(plt_data[[mod_group()]],
#                                     levels = rev(levels(mod_factor)))
#   plt <- ggplot(plt_data, aes_string(x = mod_group(), y = es(),
#                                      colour = mod_group())) +
#     coord_flip() +
#     geom_violin() +
#     geom_jitter(height = 0) +
#     geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
#     scale_colour_solarized(name = "", guide = FALSE) +
#     xlab("") +
#     ylab("Effect Size\n")
#   if (mod_group() == "all_mod") {
#     plt + theme(axis.ticks.y = element_blank())
#   } else {
#     plt
#   }
# }
# 
# output$violin <- renderPlot(
#   violin(),
#   height = function() length(unique(mod_data()[[mod_group()]])) * 90 + 70
# )
# 
# ########### FOREST PLOT ###########
# 
# forest <- function() {
#   f <- fitted(model())
#   p <- predict(model())
#   
#   forest_data <- data.frame(effects = as.numeric(model()$yi.f),
#                             variances = model()$vi.f) %>%
#     mutate(effects.cil = effects -
#              qnorm(alpha / 2, lower.tail = FALSE) * sqrt(variances),
#            effects.cih = effects +
#              qnorm(alpha / 2, lower.tail = FALSE) * sqrt(variances),
#            estimate = as.numeric(f),
#            short_cite = names(f),
#            estimate.cil = p$ci.lb,
#            estimate.cih = p$ci.ub,
#            inverse_vars = 1/variances,
#            identity = 1) %>%
#     left_join(mutate(mod_data(), short_cite = make.unique(short_cite))) %>%
#     arrange_(.dots = list(sprintf("desc(%s)", input$forest_sort),
#                           "desc(effects)")) %>%
#     mutate(short_cite = factor(short_cite, levels = short_cite))
#   
#   labels <- if (mod_group() == "all_mod") NULL else
#     setNames(paste(mod_data()[[mod_group()]], "  "),
#              mod_data()[[mod_group()]])
#   guide <- if (mod_group() == "all_mod") FALSE else "legend"
#   
#   qplot(short_cite, effects, ymin = effects.cil, ymax = effects.cih,
#         geom = "linerange",
#         data = forest_data) +
#     geom_point(aes(y = effects, size = inverse_vars)) +
#     geom_pointrange(aes_string(x = "short_cite", y = "estimate",
#                                ymin = "estimate.cil", ymax = "estimate.cih",
#                                colour = mod_group()), 
#                     pch = 17) +
#     geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
#     coord_flip() +
#     scale_size_continuous(guide = FALSE) +
#     scale_colour_solarized(name = "", labels = labels, guide = guide) +
#     xlab("") +
#     ylab("Effect Size")
# }
# 
# forest_summary <- function() {
#   pred_data <- data.frame(predictor = names(coef(model())),
#                           coef = coef(model()),
#                           ci.lb = summary(model())$ci.lb,
#                           ci.ub = summary(model())$ci.ub)
#   
#   predictors <- data_frame(moderator = "", value = "", predictor = "intrcpt",
#                            print_predictor = "intercept")
#   if (!is.null(categorical_mods()) && length(categorical_mods())) {
#     mod_vals <- map_df(categorical_mods(),
#                        ~data_frame(moderator = .x,
#                                    value = unique(mod_data()[[.x]]))) %>%
#       mutate(predictor = paste0(moderator, value),
#              print_predictor = sprintf("%s: %s", moderator, value))
#     predictors <- predictors %>%
#       bind_rows(mod_vals)
#   }
#   if ("mean_age" %in% input$moderators) {
#     predictors <- predictors %>%
#       bind_rows(data_frame(moderator = "", value = "", predictor = "mean_age",
#                            print_predictor = "mean_age"))
#   }
#   pred_data %>% left_join(predictors) %>%
#     ggplot(aes(x = print_predictor, y = coef, ymin = ci.lb,
#                ymax = ci.ub)) +
#     geom_pointrange() +
#     coord_flip() +
#     xlab("") +
#     ylab("Effect Size") +
#     geom_hline(yintercept = 0, linetype = "dashed", color = "grey")
# }
# 
# output$forest <- renderPlot(forest(),
#                             height = function() nrow(mod_data()) * 10 + 100)
# 
# output$forest_summary <- renderPlot(forest_summary(),
#                                     height = 200)
# 
# output$forest_summary_text <- renderPrint({
#   summary(model())
# })
# 
# ########### FUNNEL PLOT ###########
# 
# funnel <- function() {
#   if (length(input$moderators) == 0) {
#     d <- data_frame(se = sqrt(model()$vi), es = model()$yi)
#     center <- mean(d$es)
#     xlabel <- "\nEffect Size"
#     ylabel <- "Standard Error\n"
#   } else {
#     r <- rstandard(model())
#     d <- data_frame(se = r$se, es = r$resid)
#     center <- 0
#     xlabel <- "\nResidual Effect Size"
#     ylabel <- "Residual Standard Error\n"
#   }
#   d[[mod_group()]] <- mod_data()[[mod_group()]]
#   
#   lower_lim <- max(d$se) + .05 * max(d$se)
#   funnel95 <- data.frame(x = c(center - lower_lim * CRIT_95, center,
#                                center + lower_lim * CRIT_95),
#                          y = c(-lower_lim, 0, -lower_lim))
#   
#   left_lim99 <- ifelse(center - lower_lim * CRIT_99 < min(d$es),
#                        center - lower_lim * CRIT_99,
#                        min(d$es))
#   right_lim99 <- ifelse(center + lower_lim * CRIT_99 > max(d$es),
#                         center + lower_lim * CRIT_99,
#                         max(d$es))
#   funnel99 <- data.frame(x = c(center - lower_lim * CRIT_99, center,
#                                center + lower_lim * CRIT_99),
#                          y = c(-lower_lim, 0, -lower_lim))
#   
#   labels <- if (mod_group() == "all_mod") NULL else
#     setNames(paste(mod_data()[[mod_group()]], "  "),
#              mod_data()[[mod_group()]])
#   guide <- if (mod_group() == "all_mod") FALSE else "legend"
#   
#   ggplot(d, aes(x = es, y = -se)) +
#     scale_colour_solarized(name = "", labels = labels, guide = guide) +
#     scale_x_continuous(limits = c(left_lim99, right_lim99)) +
#     scale_y_continuous(labels = function(x){abs(x)}) +
#     geom_polygon(aes(x = x, y = y), data = funnel95, alpha = .5,
#                  fill = "white") +
#     geom_polygon(aes(x = x, y = y), data = funnel99, alpha = .5,
#                  fill = "white") +
#     geom_vline(xintercept = center, linetype = "dotted", color = "black") +
#     geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
#     geom_point(aes_string(colour = mod_group())) +
#     xlab(xlabel) +
#     ylab(ylabel) +
#     geom_text(x = center + lower_lim * CRIT_95 / 2,
#               y = -lower_lim + lower_lim / 30, #family = font,
#               label = "p < .05", vjust = "bottom", hjust = "center") +
#     geom_text(x = (center + lower_lim * CRIT_95) + (lower_lim * CRIT_99 - lower_lim * CRIT_95) / 2,
#               y = -lower_lim + lower_lim / 30, #family = font,
#               label = "p < .01", vjust = "bottom", hjust = "center") +
#     theme(panel.background = element_rect(fill = "grey"),
#           panel.grid.major =  element_line(colour = "darkgrey", size = 0.2),
#           panel.grid.minor =  element_line(colour = "darkgrey", size = 0.5))
# }
# 
# output$funnel <- renderPlot(funnel())
# output$funnel_test <- renderText({
#   funnel_test <- metafor::regtest(model())
#   sprintf("Regression test for funnel plot asymmetry: z = %.3g, p = %.3g.
#           Interpret with caution due to the possible presence of confounding
#           moderators.", funnel_test$zval, funnel_test$pval)
# })

## Code description ####
## -- -- -- -- -- -- -- -- -- -- -- -- --
##
## Author: Jesus C. Compaire
## Position: Assistant Researcher Scientist
## Institution: Center for the Study of Marine Systems (CESIMAR)
## Position: Assistant Professor of Climate System
## Institution: National University of the Patagonia San Juan Bosco (UNPSJB)
## Contact details: jesus.canocompaire@uca.es
## Date created: Apr-2026
## -- -- -- -- -- -- -- -- -- -- -- -- --
##
## Code to replicate the statistical analyses and generate figures 
## performed in the manuscript:
## Compaire, J.C., Irigoyen, A.J. Simionato C.G. & Acha, E.M. (2026).
## Multiscale environmental forcing of multispecies fishery dynamics across
## climate timescales
## Fisheries Oceanography, XXX
## https://doi.org/XXXXX
##
## -- -- -- -- -- -- -- -- -- -- -- -- --
#
# DTW functions ----------------------------------------------------------------
# --- f(x) to convert cvi_list to a data frame suitable for ggplot
list_to_df <- function(cvi_list, legend_labels) {
  data_frames <- list()
  for (i in seq_along(cvi_list)) {
    df <- as.data.frame(t(cvi_list[[i]]))  # Transpose for easier handling
    df$k <- rownames(df)                   # Add k values as a column
    df <- df %>% 
      pivot_longer(cols = -k, names_to = "Index", values_to = "Score") %>%
      mutate(Method = legend_labels[i])    # Add the method label
    data_frames[[i]] <- df
  }
  # Bind all data frames into one
  combined_df <- bind_rows(data_frames)
  return(combined_df)
}
# --- f(x) to get cluster elements
get_clusters <- function(pc_obj, data, n_clusters, verbose = TRUE) {
  plot(pc_obj, type = "sc")
  cluster_assignments <- pc_obj@cluster
  print(cluster_assignments)
  clusters <- list()
  for (i in 1:n_clusters) {
    clusters[[i]] <- rownames(data)[cluster_assignments == i]
    if (verbose) {
      cat(paste0("Cluster ", i, ":\n"))
      print(clusters[[i]])
    }
  }
  return(clusters)
}
# --- f(x) to get best repetition from Dunn index
best_repetition <- function(partitional_result, verbose = FALSE){
  # Automatically evaluate Dunn index for each repetition
  cvi_results <- lapply(partitional_result,
                        function(x) cvi(x, type = c("D"))) # "Sil"
  # Convert results into a data frame for easier analysis
  cvi_df <- do.call(rbind, cvi_results)
  # Add the repetition index as a column
  cvi_df <- data.frame(Rep = seq_along(cvi_results), cvi_df)
  # Display results
  # print(cvi_df)
  # best_silhouette_index <- which.max(cvi_df$Sil)
  best_dunn_index <- which.max(cvi_df$D)
  # Print the best repetition based on Dunn index
  # cat("Best repetition based on Silhouette:", best_silhouette_index,
  #     "value:", round(max(cvi_df$Sil),3),  "\n")
  if(verbose){
    cat("Best repetition based on Dunn:", best_dunn_index,
          "value:", round(max(cvi_df$D),3),  "\n")
          }
  return(cvi_df)
}
# --- f(x) to extract and sort CVI scores by method
cvi_sorted <- function(df_pam, df_dba, cvi_name) {
  cvi_data <- data.frame(df_pam[1], df_pam[[cvi_name]], df_dba[[cvi_name]])
  colnames(cvi_data) <- c("Repetition", "PAM", "DBA")
  # Convert to long format for ggplot
  cvi_data <- melt(cvi_data,
                   id.vars = "Repetition",
                   variable.name = "Method",
                   value.name = "Score")
  
  return(cvi_data)
}
# --- f(x) to get elements in each cluster
get_cluster_elements <- function(data, clustering_result, k, verbose = TRUE) {
  # Get the cluster assignments
  cluster_assignments <- clustering_result@cluster
  # Create a list to store the elements in each cluster
  cluster_elements <- list()
  # Loop through each cluster and extract the corresponding elements
  for (i in 1:k) {
    cluster_elements[[i]] <- rownames(data)[cluster_assignments == i]
  }
  # Print the elements for each cluster
  if (verbose) {
    for (i in 1:k) {
        cat("Cluster", i, ":\n")
            print(cluster_elements[[i]])
                cat("\n")
    }
  }
  # Return the list of elements in each cluster (optional)
  return(cluster_elements)
}
# --- f(x) to plot CVI
plot_cvi <- function(cvid_df){
  ggplot(cvi_df, aes(x = k, y = Score, color = Method, group = Method)) +
    geom_line(aes(linetype = Method)) +
    geom_point() +
    facet_wrap(~ Index, scales = "free_y") + # Separate plots for each index
    labs(
      x = "Number of clusters",
      y = "Score",
      color = "Method", linetype = "Method") +
    theme_tufte() +
    theme(
      axis.title = element_text(face = "bold")) +
    theme(
      legend.position = "top",
      legend.title=element_blank(),
      strip.text = element_text(size = 12, face = "bold")) +
    theme(
      axis.line = element_line(colour = "black"),
      axis.ticks = element_line(colour = "black")) +
    theme(
      axis.text.y = element_text(color = "black", size = 12)) +
    theme(
      axis.text.x = element_text(color = "black", size = 12)) +
    theme(
      axis.text = element_text(size=12)
    )
}
# --- f(x) to plot Dunn index values across 100 random repetitions
plot_cvi_values <- function(cvi_rep_values, index_name,
                            cvi_hc_k4_x = NULL, cvi_hc_k4_y = NULL){
  p <- ggplot(cvi_rep_values, aes(x = Repetition, y = Score, color = Method)) +
    geom_line(aes(linetype = Method)) +
    labs(
      x = "Random repetitions", 
      y = paste0("Score for ", index_name, " index")) +
    scale_x_continuous(breaks = round(seq(0, 100, by = 5),1),
                       expand = c(0.01,0.01)) +
    scale_color_manual(values = c('grey23', "black")) +
    theme_tufte() +
    theme(
      axis.title = element_text(face = "bold")) +
    theme(
      legend.position = "top",
      legend.title=element_blank(),
      strip.text = element_text(size = 12, face = "bold")) +
    theme(
      axis.line = element_line(colour = "black"),
      axis.ticks = element_line(colour = "black")) +
    theme(
      axis.text.y = element_text(color = "black", size = 12)) +
    theme(
      axis.text.x = element_text(color = "black", size = 12)) +
    theme(
      axis.text = element_text(size=12)
    )
  # Add the single empty circle marker if x and y coordinates are provided
  if (!is.null(cvi_hc_k4_x) && !is.null(cvi_hc_k4_y)) {
    p <- p + annotate("point",
                      x = cvi_hc_k4_x, y = cvi_hc_k4_y,
                      shape = 1, size = 2, color = "black")
    # to show CVI score for hierarchical clustering with the same number
    # of optimal k clusters
  }
  return(p)
}
# DFA functions ----------------------------------------------------------------
fit_dfa <- function(m, dat, ts_n, show_summary = TRUE) {
  # Build Z matrix
  Z_vals <- as.list(unlist(lapply(1:ts_n, function(i) paste0("z", i, 1:m))))
  ZZ <- matrix(Z_vals, nrow = ts_n, ncol = m, byrow = TRUE)
  
  # Model specification
  mod_list <- list(
    Z = ZZ, A = "zero", D = "zero", d = "zero",
    R = "diagonal and unequal", B = "identity",
    U = "zero", C = "zero", c = "zero",
    Q = "identity"
  )
  
  init_list <- list(x0 = matrix(0, m, 1))
  
  txt <- capture.output({
    fit <- MARSS(
      y = dat,
      model = mod_list,
      inits = init_list,
      control = list(
        maxit = 9999,
        allow.degen = FALSE,
        conv.test.slope.tol = 0.01
      )
    )
  })

  # keep only summary header
  if (show_summary) {
    cat(paste(txt[c(1, 3:8)], collapse = "\n"), "\n")
  }

  return(fit)
}

dfa_calculations <- function(dfa_fit, dat, threshold = thr) {
  ts_n <- nrow(dat)
  mm   <- nrow(dfa_fit$states)
  
  # -- Rotation
  Z_est_m <- coef(dfa_fit, type = "matrix")$Z
  H_inv   <- varimax(Z_est_m)$rotmat
  Z_rot   <- round(Z_est_m %*% H_inv, 2)
  proc_rot <- solve(H_inv) %*% dfa_fit$states
  
  # -- Variance explained
  var_expl      <- apply(Z_rot^2, 2, sum)
  prop_var_expl <- var_expl / sum(var_expl) * 100
  
  # -- Reorder based on Variance Explained (Decreasing)
  ord           <- order(prop_var_expl, decreasing = TRUE)
  prop_var_expl <- prop_var_expl[ord]
  Z_rot         <- Z_rot[, ord]
  proc_rot      <- proc_rot[ord, ]
  
  # -- Flag for factor loadings cutoff
  thr <- threshold
  Z_rot_df <- as.data.frame(Z_rot, 2)
  Z_rot_df$cutoff <- apply(abs(Z_rot), 1, function(x) any(x >= thr))

  # -- Smoothed residuals
  resid_res <- MARSSresiduals(dfa_fit, type = "tT")  
  resid_smooth <- resid_res$residuals
  resid_var    <- resid_res$SE^2  
  resid_std <- resid_res$std.residuals

  # -- Return 
  return(list(
    Z_rot = Z_rot,
    Z_rot_df = Z_rot_df,
    proc_rot = proc_rot,
    prop_var_expl = prop_var_expl,
    mm = mm,
    ts_n = ts_n,
    resid_smooth = resid_smooth,
    resid_var = resid_var,
    resid_std = resid_std)
  )
}
dfa_plot <- function(dfa_results, dat, yrs, clr = NULL, threshold = 0.2) {
  Z_rot         <- dfa_results$Z_rot
  proc_rot      <- dfa_results$proc_rot
  prop_var_expl <- dfa_results$prop_var_expl
  mm            <- dfa_results$mm
  ts_n          <- dfa_results$ts_n
  
  ylbl <- rownames(dat)
  w_ts <- 1:ncol(dat)
  
  layout(matrix(c(1, 2, 3, 4, 5, 6), mm, 2), widths = c(1, 1))
  par(mai = c(0.5, 0.5, 0.5, 0.1), omi = c(0, 0, 0, 0))
  
  # -- Plot rotated states
  for (i in 1:mm) {
    ylm <- c(-1, 1) * max(abs(proc_rot[i, ]))
    plot(w_ts, proc_rot[i, ], type = "n", bty = "L", ylim = ylm,
         xlab = "", ylab = "", xaxt = "n")
    # abline(h = 0, col = "gray")
    lines(w_ts, proc_rot[i, ], lwd = 2)
    mtext(bquote("State" ~ .(i) ~ " (" * .(round(prop_var_expl[i], 2)) * "%)"),
          side = 3, line = 0.5)
    axis(1, at = 1:ncol(dat), labels = yrs)
    # X-axis labels are shown only on the last panel
    # if (i == mm) {
    #   axis(1, at = 1:ncol(dat), labels = yrs)
    # } else {
    #   axis(1, at = 1:ncol(dat), labels = FALSE)
    # }
    if (i == 2) mtext("Trends", side = 2, outer = F, line = 2.5, at = 0)
    }
  # -- Plot rotated loadings
  minZ <- 0
  ylm  <- c(-1, 1) * max(abs(Z_rot))
  
  for (i in 1:mm) {
    plot(1:ts_n, Z_rot[, i],
         xlab = "", ylab = "",
         type = "h", lwd = 2, xaxt = "n",
         ylim = ylm, xlim = c(0.5, ts_n + 0.5),
         col = "black") # first color black
    abline(h = 0, lwd = 1.5, col = "gray")
    
    for (j in 1:ts_n) {
      # Color according to threshold
      if (abs(Z_rot[j, i]) >= threshold) {
        bar_col <- clr[j]   # if > threshold custom color
      } else {
        bar_col <- "black"  # else black
      }
      
      # Redraw the lines according to color
      lines(c(j, j), c(0, Z_rot[j, i]), lwd = 2, col = bar_col)     
      
      # Tag for each species
      if (Z_rot[j, i] > minZ) {
        text(j, -0.03, 
             labels = bquote(~italic(.(ylbl[j]))),
             srt = 90, adj = 1, cex = 1.2, col = bar_col)
      }
      if (Z_rot[j, i] < -minZ) {
        text(j,  0.03,
             labels = bquote(~italic(.(ylbl[j]))),
             srt = 90, adj = 0, cex = 1.2, col = bar_col)
      }
  }
  mtext(paste("Factor loadings on state", i), side = 3, line = 0.5)
  if (i == 2) mtext("Loadings", side = 2, outer = F, line = 2.5, at = 0)
  }
}
# 
dfa_calculations_CIs <- function(dfa_fit, dat, nboot) {
  ts_n <- nrow(dat)
  mm   <- nrow(dfa_fit$states)
  
  # -- Parameter CIs 
  dfa_CIs <- MARSSparamCIs(dfa_fit, method = "parametric", nboot = nboot)
  Z_est   <- dfa_CIs$par$Z
  Z_se    <- dfa_CIs$par.se$Z
  Z_low   <- dfa_CIs$par.lowCI$Z
  Z_up    <- dfa_CIs$par.upCI$Z
  
  # -- Rotation
  Z_est_m <- coef(dfa_fit, type = "matrix")$Z
  H_inv   <- varimax(Z_est_m)$rotmat
  Z_rot   <- Z_est_m %*% H_inv
  proc_rot <- solve(H_inv) %*% dfa_fit$states
  
  # -- Variance explained
  var_expl      <- apply(Z_rot^2, 2, sum)
  prop_var_expl <- var_expl / sum(var_expl) * 100
  
  # -- Significance
  Z_signif <- data.frame(
    serie    = rep(rownames(Z_est), times = ncol(Z_est)),
    estimate = as.vector(Z_est),
    se       = as.vector(Z_se),
    lowCI    = as.vector(Z_low),
    upCI     = as.vector(Z_up)
  )
  Z_signif$sig <- ifelse(
    Z_signif$lowCI > 0, "+",
    ifelse(Z_signif$upCI < 0, "-", "ns")
  )
  
  # -- Get indices
  Z_signif$serie_idx <- as.numeric(gsub("z([0-9]+)([0-9]+)", "\\1", Z_signif$serie))
  Z_signif$state     <- as.numeric(gsub("z([0-9]+)([0-9]+)", "\\2", Z_signif$serie))
  sig_tmp <- Z_signif[Z_signif$sig != "ns", ]
  signif_coords <- paste(sig_tmp$serie_idx, sig_tmp$state, sep = "_")
  # 
  ####@#### -- Bootstrapped states (trends) CIs
  boot_states <- MARSSparamCIs(dfa_fit, type = "states", method = "parametric", nboot = nboot)
  states_low  <- boot_states$states.lowCI
  states_up   <- boot_states$states.upCI
  states_est  <- boot_states$states
  ####@####
  
  #### ~~~ Added: Smoothed residuals via exported function
  resid_res <- MARSSresiduals(dfa_fit, type = "tT")  # smoothed residuals
  resid_smooth <- resid_res$residuals
  resid_var    <- resid_res$SE^2  # approximate variance
  resid_std <- resid_res$std.residuals
  #### ~~~ End added
  
  return(list(
    Z_est = Z_est,
    Z_rot = Z_rot,
    Z_rot_df = Z_rot_df,
    proc_rot = proc_rot,
    prop_var_expl = prop_var_expl,
    # signif = Z_signif,
    # signif_coords = signif_coords,
    mm = mm,
    ts_n = ts_n,
   
    resid_smooth = resid_smooth,
    resid_var = resid_var,
    resid_std = resid_std,
    
    states_est = states_est,
    states_low = states_low,
    states_up  = states_up

  ))
}
dfa_plot_CIs <- function(dfa_results, dat, yrs, clr = NULL) {
  Z_rot         <- dfa_results$Z_rot
  proc_rot      <- dfa_results$proc_rot
  prop_var_expl <- dfa_results$prop_var_expl
  Z_signif      <- dfa_results$signif
  mm            <- dfa_results$mm
  ts_n          <- dfa_results$ts_n
  
  ylbl <- rownames(dat)
  w_ts <- 1:ncol(dat)
  
  layout(matrix(c(1, 2, 3, 4, 5, 6), mm, 2), widths = c(1, 1))
  par(mai = c(0.5, 0.5, 0.5, 0.1), omi = c(0, 0, 0, 0))
  
  # -- Plot rotated states
  for (i in 1:mm) {
    ylm <- c(-1, 1) * max(abs(proc_rot[i, ]))
    plot(w_ts, proc_rot[i, ], type = "n", bty = "L", ylim = ylm,
         xlab = "", ylab = "", xaxt = "n")
    abline(h = 0, col = "gray")
    lines(w_ts, proc_rot[i, ], lwd = 2)
    mtext(bquote("State" ~ .(i) ~ " (" * .(round(prop_var_expl[i], 2)) * "%)"),
          side = 3, line = 0.5)
    # axis(1, at = 1:ncol(dat), labels = yrs)
    axis(1, at = 1:ncol(dat), labels = F)
    if (i == 2) mtext("Trends", side = 2, outer = F, line = 2.5, at = 0)
    if (i == 3) axis(1, at = 1:ncol(dat), labels = yrs)
  }
  
  # -- Plot rotated loadings
  minZ <- 0
  ylm  <- c(-1, 1) * max(abs(Z_rot))
  
  for (i in 1:mm) {
    plot(1:ts_n, Z_rot[, i],
         xlab = "", ylab = "",
         type = "h", lwd = 2, xaxt = "n",
         ylim = ylm, xlim = c(0.5, ts_n + 0.5),
         col = "black")  # first color black
    abline(h = 0, lwd = 1.5, col = "gray")
    
    for (j in 1:ts_n) {
      # Assess the significance of each loading
      sig_val <- Z_signif$sig[Z_signif$serie_idx == j & Z_signif$state == i]
      
      if (length(sig_val) == 1) {
        if (sig_val %in% c("+", "-")) {
          bar_col <- clr[j]    # if it is significant -> original color
        } else {
          bar_col <- "black"   # else black 
        }
        
        # Redraw lines according to color
        lines(c(j, j), c(0, Z_rot[j, i]), lwd = 2, col = bar_col)
        
        # Tag for each species
        if (Z_rot[j, i] > minZ) {
          text(j, -0.03, ylbl[j], srt = 90, adj = 1, cex = 1.2, col = bar_col)
        }
        if (Z_rot[j, i] < -minZ) {
          text(j,  0.03, ylbl[j], srt = 90, adj = 0, cex = 1.2, col = bar_col)
        }
      }
    }
    
    mtext(paste("Factor loadings on state", i), side = 3, line = 0.5)
    if (i == 2) mtext("Loadings", side = 2, outer = F, line = 2.5, at = 0)
  }
}

## Fig. S2 right - CORRELOGRAMS
correlograms.plot <- function(x, lags = NULL, main = NULL,
                              upperindex = NULL, 
                              y_lim = NULL, y_breaks = NULL, ...){
  if (!is.numeric(x))
    stop("'x' must be a numeric vector")
  if (is.null(lags)) {
    lags <- 36
  } else if ((lags %% 1 == 0) == FALSE || (lags < 0)) {
    stop('The argument "lags" must be a positive integer')
  }
  if (is.null(main) & is.null(upperindex)) {
    mytitle <- deparse(substitute(x))
    mysubt <- NULL
    sz <- 20
    fc <- 'plain'
    hz <- 0.5
    vt <- 1
  } else if (!is.null(main) & is.null(upperindex)) {
    if (!is.character(main))
      stop("'main' must be a character string")
    mytitle <- paste(main)
    mysubt <- NULL
    sz <- 20
    fc <- 'italic'
    hz <- 0.5
    vt <- 1
  } else if (is.null(main) & !is.null(upperindex)) {
    if (!is.character(upperindex))
      stop("'upperindex' must be a character string")
    mytitle <- NULL
    mysubt <- paste(upperindex)
    sz <- 20
    fc <- 'bold'
    hz <- 0.5
    vt <- -5
  } else if (!is.null(main) & !is.null(upperindex)) {
    if (!is.character(main))
      stop("'main' amd 'upperindex' must be a character string")
    mytitle <- paste(main)
    mysubt <- paste(upperindex)
    sz <- 20
    fc <- 'italic'
    hz <- 0.5
    vt <- -5
  }
  acf_v <- acf(x, lag.max = lags, type = "correlation", plot = FALSE)$acf
  pacf_v <- acf(x, lag.max = lags, type = "partial", plot = FALSE)$acf
  
  # Helper function for dynamic Y scale
  get_y_scale <- function(vec, custom_breaks, ylim = NULL) {

  # priority 1: user-defined limits from outside
  if (!is.null(ylim)) {
    return(
      scale_y_continuous(
        limits = ylim,
        breaks = pretty(ylim, n = 5)))
        }
  # priority 2: fixed custom breaks
  if (!is.null(custom_breaks)) {
    return(
      scale_y_continuous(
        limits = c(-0.1, 0.1),
        breaks = seq(-0.075, 0.075, by = 0.05)))
        }
  # priority 3: dynamic automatic scale
  scale_y_continuous(
    breaks = round(seq(
      plyr::round_any(min(vec), 0.1, f = floor),
      plyr::round_any(max(vec), 0.1, f = ceiling),
      by = 0.1), 1))
      }

  p1 <- ggAcf(x, lag.max = lags) +
    theme_tufte() +
    scale_x_continuous(breaks = round(seq(0, lags, by = 2),1)) +
    get_y_scale(acf_v, y_breaks, y_lim) +
    #scale_y_continuous(limits = c(-0.1, 0.1),
    #                   breaks = round(seq(-0.075, 0.075, by = 0.05),3)) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank()) +
    theme(axis.line = element_line(colour = "black"),
          axis.ticks = element_line(colour="black")) +
    theme(axis.text = element_text(color = "black", size = 12)) +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="plain")) +
    labs(title = mytitle, subtitle = mysubt) +
    theme(plot.title = element_text(
      size=sz, face = fc, hjust = hz, vjust = vt)) +
    theme(plot.subtitle = element_text(
      size=sz-4, face = "bold", hjust = hz-0.55, vjust = vt+10)) 

  p2 <- ggPacf(x, lag.max = lags) + #, type = 'partial') +
    labs(title = "") + theme_tufte() +
    scale_x_continuous(breaks = round(seq(0, lags, by = 2),1)) +
    get_y_scale(pacf_v, y_breaks, y_lim) +
    #scale_y_continuous(limits = c(-0.1, 0.1),
    #                   breaks = round(seq(-0.075, 0.075, by = 0.05),3)) +
    theme(axis.line = element_line(colour = "black"),
          axis.ticks = element_line(colour="black")) +
    theme(axis.text = element_text(color = "black", size = 12)) +
    labs(x="Lags") +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="plain"))
  pt <- p1 / p2
  pt
}


# 
get_DFA_fits <- function(MLEobj, dd = NULL, alpha = 0.05) {
  ## empty list for results 
  fits <- list()
  ## extra stuff for var() calcs
  Ey <- MARSS:::MARSShatyt(MLEobj)
  ## model params
  ZZ <- coef(MLEobj, type = "matrix")$Z
  ## number of obs ts
  nn <- dim(Ey$ytT)[1]
  ## number of time steps
  TT <- dim(Ey$ytT)[2]
  ## get the inverse of the rotation matrix
  H_inv <- varimax(ZZ)$rotmat 
  ## check for covars 
  if (!is.null(dd)) { 
    DD <- coef(MLEobj, type = "matrix")$D
    ## model expectation 
    fits$ex <- ZZ %*% H_inv %*% MLEobj$states + DD %*% dd 
  } else {
    ## model expectation 
    fits$ex <- ZZ %*% H_inv %*% MLEobj$states 
  } 
  ## Var in model fits
  VtT <- MARSSkfss(MLEobj)$VtT 
  VV <- NULL 
  for (tt in 1:TT) {
    RZVZ <- coef(MLEobj, type = "matrix")$R - ZZ %*% VtT[,
                                                         , tt] %*% t(ZZ) 
    SS <- Ey$yxtT[, , tt] - Ey$ytT[, tt, drop = FALSE] %*%
      t(MLEobj$states[, tt, drop = FALSE])
    VV <- cbind(VV, diag(RZVZ + SS %*% t(ZZ) + ZZ %*% t(SS)))
  } 
  SE <- sqrt(VV)
  ## upper & lower (1-alpha)% CI 
  fits$up <- qnorm(1 - alpha/2) * SE + fits$ex 
  fits$lo <- qnorm(alpha/2) * SE + fits$ex 
  return(fits) 
}
get_DFA_prediction_intervals <- function(MLEobj, alpha = 0.05, rotate = TRUE){
  Ey <- MARSS:::MARSShatyt(MLEobj)    # E[y_t | data]
  ZZ   <- coef(MLEobj, type = "matrix")$Z
  Rmat <- coef(MLEobj, type = "matrix")$R
  VtT  <- MARSSkfss(MLEobj)$VtT
  TT   <- dim(Ey$ytT)[2]
  nn   <- dim(Ey$ytT)[1]
  
  if (rotate){
    H_inv <- varimax(ZZ)$rotmat
    ZZ_rot <- ZZ %*% H_inv
    states_rot <- solve(H_inv) %*% MLEobj$states
  } else {
    ZZ_rot <- ZZ
    states_rot <- MLEobj$states
  }
  
  fits_ex <- ZZ_rot %*% states_rot   # fitted mean E[y]
  SEmat <- matrix(NA, nrow = nn, ncol = TT)
  
  for (tt in 1:TT){
    Vst <- VtT[,,tt]
    V_y <- ZZ_rot %*% Vst %*% t(ZZ_rot) + Rmat
    SEmat[, tt] <- sqrt(pmax(0, diag(V_y)))  # seguro no-negativos
  }
  
  z_q <- qnorm(1 - alpha/2)
  up <- fits_ex + z_q * SEmat
  lo <- fits_ex - z_q * SEmat
  
  return(list(ex = fits_ex, se = SEmat, up = up, lo = lo,
              ZZ_rot = ZZ_rot, states_rot = states_rot))
}
# Read and sort ENVRIONMENTAL and FISHING DATA functions -----------------------------------
# --- f(x) to read environmental variables from nc file
read_env_vars <- function(nc) {
  var_names <- names(nc$var)
  lag <- ncvar_get(nc, "lag")
  time <- ncvar_get(nc, "time")
  
  # Remove phase variables
  names_col <- var_names[-c(2, 4, 6)]
  
  # Read and transpose all selected variables
  env_var <- lapply(names_col, function(v) {
    mat <- t(ncvar_get(nc, v))
    mat[is.nan(mat)] <- NA
    return(mat)
  })
  # Dimensions info
  n_rows <- nrow(env_var[[1]])
  n_cols <- ncol(env_var[[1]])
  n_vars <- length(env_var)
  # Convert var_list into 3D array: n_rows x n_cols x n_vars
  arr <- array(unlist(env_var), dim = c(n_rows, n_cols, n_vars))
  
  # --- f(x)  to reorganize into lagged matrices
  env_var_lag <- lapply(1:n_cols, function(j) {
    # For each column j, collect that column from all variables along 3rd dimension
    mat <- matrix(arr[, j, ], nrow = n_rows, ncol = n_vars)
    colnames(mat) <- names_col
    rownames(mat) <- time
    mat
  })
  # Adding lag tag
  names(env_var_lag) <- paste0("Lag_", 1:n_cols - 1)
  
  return(list(env_var_lag = env_var_lag, names_col = names_col, time = time))
}
# --- f(x) to get states or centroids dataframe
get_states_or_centroids <- function(obj, years, tag = c("states", "centroids")){
  tag <- match.arg(tag)   # ensures only "states" or "centroids"
  obj <- t(obj)  # transpose to match structure
  n_states <- ncol(obj)
  # Prefix depends on the tag
  prefix <- if (tag == "states") "s" else "c"
  colnames(obj) <- paste0(prefix, seq_len(n_states))
  df <- as.data.frame(obj)
  df$year <- years
  return(df)
}
# --- f(x) to merge env vars with states
merge_env_states <- function(env_var_lag, states_df, names_col) {
  env_fish_lag <- lapply(env_var_lag, function(mat) {
    mat_df <- as.data.frame(mat)
    mat_df$year <- rownames(mat)
    merged_df <- merge(mat_df, states_df, by = "year", all = TRUE)
    rownames(merged_df) <- merged_df$year
    merged_df$year <- NULL
    merged_df
  })
  
  # Convert to numeric where possible
  env_fish_lag <- lapply(env_fish_lag, function(df) {
    df[] <- lapply(df, function(col) {
      num_col <- suppressWarnings(as.numeric(col))
      if (all(is.na(num_col))) {
        return(col)
      } else {
        return(num_col)
      }
    })
    return(df)
  })
  
  return(env_fish_lag)
}
# GLM functions ----------------------------------------------------------------
# --- f(x) to get all combinations among explanatory variables - INTERACCTION
#     considering the interaction of "x1" and "x2" with "x3"
comb_glmi <- function(xnam, ynam, inter){
  lstls <- list()
  cnum <- length(xnam) # length explanatory variables
  # List comprehension. Getting all combinations of explanatory variables
  lstls <- to_list(for(i in 1:cnum) 
    c(combn(xnam, i, simplify = FALSE)))
  for(i in 1:cnum){
    k <- length(lstls[[i]])
    for(j in 1:k){
      covariates <- lstls[[i]][[j]] # covariates names
      if (
        inter[1] %in% covariates &
        inter[2] %in% covariates & !(inter[3] %in% covariates)
      ){
        inter_t <- paste(inter[c(1,2)], collapse = "*")
      } else if (
        inter[1] %in% covariates &
        !inter[2] %in% covariates & (inter[3] %in% covariates)
      ){
        inter_t <- paste(inter[c(1,3)], collapse = "*")
      } else if (
        inter[1] %in% covariates &
        inter[2] %in% covariates & (inter[3] %in% covariates)
      ){
        inter_t <- paste(
          paste(inter[c(1,2)], collapse = "*"), "+",
          paste(inter[c(1,3)], collapse = "*"))
      } else {
        inter_t <- NULL
      }
      if (
        !is.null(inter_t)
      ){
        lstls[[i]][[j]] <- (as.formula(paste(
          paste(ynam), " ~ ", paste(covariates, collapse = "+"),
          " + ", inter_t)))
      } else {
        lstls[[i]][[j]] <- (as.formula(paste(
          paste(ynam), " ~ ", paste(covariates, collapse = "+"))))
      }
    }
  }
  lst <- matrix(unlist(lstls), ncol = 1, byrow = TRUE)
  lst <- c(as.formula(paste(paste(ynam), " ~ 1")), lst) # Adding NULL model
}
# --- f(x) to perform GLM analyses
try_glm <- function(eq, dataset, i, j, f, th) {
  model <- tryCatch(
    glm(
      eq,
      family = f,
      data = dataset,
      na.action = na.omit
    ),
    error = function(e) {
      message(
        paste0(
          "Analysis not performed. Error occurred for equation ",
          j,
          ":\n",
          deparse(eq),
          "\n",
          "Error: ",
          conditionMessage(e)
        )
      )
      return(NULL)
    }
  )
  
  DE <- NA
  max_vif_info <- data.frame(variable = NA, max_vif = NA)
  collinearity_result <- NULL
  
  if (!is.null(model)) {
    DE <- round((1 - model[["deviance"]] / model[["null.deviance"]]) * 100, digits = 2)
    pv <- summary(model)$coef[, "Pr(>|t|)"]
    covs <- attr(model$terms, "term.labels")
    cov_i <- unique(unlist(strsplit((covs[grepl(":", covs, fixed = TRUE)]), ':')))
    cov_s <- covs[-grep(pattern = ':', covs)]
    covs_d <- setdiff(cov_s, cov_i)
    thr <- 0.05
    
    pvals_no_intercept <- pv[-1]
    intercept_sig <- pv[1] < thr     
    if (
      intercept_sig &&
      (
        (all(pvals_no_intercept < thr) & length(pvals_no_intercept) > 0) |
        (any(pvals_no_intercept[grepl(":", covs, fixed = TRUE)] < thr) &
         all(pvals_no_intercept[covs_d] < thr))
      )
    ){
      
      max_vif_info <- tryCatch({
        # Check the number of terms (excluding the intercept)
        n_terms <- length(covs)
        
        if (n_terms <= 1) {
          # Case: intercept + 1 variable only; VIF cannot be calculated
          varname_single <- if(length(covs) == 1) covs[1] else NA
          data.frame(variable = varname_single, max_vif = 0)
        } else {
          collinearity_result <- check_collinearity(model)
          vif_df <- as.data.frame(collinearity_result)
          
          if (!is.null(vif_df) && "VIF" %in% names(vif_df) && nrow(vif_df) > 0) {
            idx <- which.max(vif_df$VIF)
            data.frame(
              variable = vif_df$Term[idx],
              max_vif = round(vif_df$VIF[idx], 2)
            )
          } else {
            data.frame(variable = "no_vif", max_vif = 0)
          }
        }
      }, warning = function(w) {
        if (grepl("Not enough model terms", conditionMessage(w))) {
          data.frame(variable = "single", max_vif = 0)
        } else {
          warning(w)
        }
      }, error = function(e) {
        data.frame(variable = NA, max_vif = NA)
      })
      
      cat(paste0(
        "percentile: ", th,
        "; family:", f[["family"]], "(link=", f[["link"]], ")",
        "; lag:", sprintf("%02d", i),
        "; eq:", sprintf("%02d", j),
        "; DE:", DE, "\n"
      ))
      cat("Eq: ", format(eq), "\n")
      print(summary(model))
      if (!is.null(collinearity_result)) print(collinearity_result)
      cat('---------- ######### ---------- ',"\n")
    }
  }
  
  return(list(
    lag = i,
    eq = j,
    DE = DE,
    VIF = max_vif_info$max_vif,
    var = max_vif_info$variable
  ))
}
# --- f(x) to perform stepwise backward
backward_pval_trace <- function(formula_full, data, family = gaussian(),
                                p_threshold = 0.05, force_hierarchy = TRUE,
                                verbose = FALSE, na.action = na.omit) {
  # Initial fit
  original_model <- glm(formula = formula_full, data = data,
                        family = family, na.action = na.action)
  current_model <- original_model   
  iter <- 0L
  
  # Store removal history
  removal_log <- data.frame(
    step = integer(),
    term_removed = character(),
    p_value = numeric(),
    formula = character(),
    stringsAsFactors = FALSE
  )
  
  repeat {
    iter <- iter + 1L
    s <- summary(current_model)
    coefmat <- s$coef
    if (is.null(coefmat) || nrow(coefmat) <= 1) break  # only intercept or nothing
    
    pv_col <- ncol(coefmat)
    pvals <- coefmat[, pv_col]
    coef_names <- rownames(coefmat)
    
    # Design matrix to map coefficients to terms
    mm <- model.matrix(current_model)
    assign <- attr(mm, "assign")
    term_labels <- c("(Intercept)", attr(terms(current_model), "term.labels"))
    coef_mm_names <- colnames(mm)
    
    # Aggregate p-values by term (excluding intercept)
    term_pvals <- rep(NA_real_, length(term_labels))
    names(term_pvals) <- term_labels
    term_pvals["(Intercept)"] <- NA
    for (tidx in seq_along(term_labels)[-1]) {
      cols_idx <- which(assign == tidx - 1)  
      if (length(cols_idx) == 0) {
        term_pvals[tidx] <- NA
      } else {
        coefnames_term <- coef_mm_names[cols_idx]
        keep_idx <- coefnames_term %in% coef_names
        if (!any(keep_idx)) {
          term_pvals[tidx] <- NA
        } else {
          term_pvals[tidx] <- max(pvals[coefnames_term[keep_idx]], na.rm = TRUE)
        }
      }
    }
    
    # Remove intercept
    term_pvals_no_int <- term_pvals[names(term_pvals) != "(Intercept)"]
    if (length(term_pvals_no_int) == 0) break
    
    # Order candidates by descending p-value
    ord <- order(term_pvals_no_int, decreasing = TRUE, na.last = TRUE)
    candidate_terms <- names(term_pvals_no_int)[ord]
    
    if (verbose) {
      cat("---- Iter:", iter, " ----\n")
      cat("Current formula:", deparse(formula(current_model)), "\n")
      print(round(term_pvals_no_int, 4))
    }
    
    # Select the worst eligible term
    term_to_remove <- NULL
    for (t in candidate_terms) {
      if (is.na(term_pvals_no_int[t])) next
      if (term_pvals_no_int[t] <= p_threshold) {
        term_to_remove <- NULL
        break
      }
      # Respect hierarchy: do not remove main effect if any interaction involves it
      if (force_hierarchy && !grepl(":", t)) {
        interactions <- grep(":", names(term_pvals_no_int), value = TRUE)
        involves <- any(grepl(paste0("(^|:)", t, "($|:)"), interactions))
        if (involves) next
      }
      term_to_remove <- t
      break
    }
    
    if (is.null(term_to_remove)) {
      if (verbose) cat("No terms with p-value <", p_threshold, "-> stop.\n")
      break
    }
    
    if (verbose) cat("Removing term:", term_to_remove,
                     "; p-value =", formatC(term_pvals_no_int[term_to_remove], digits = 4), "\n")
    
    # Update formula
    new_formula <- update(formula(current_model), paste(". ~ . -", term_to_remove))
    
    # Try refitting
    new_model <- tryCatch(
      glm(new_formula, data = data, family = family, na.action = na.action),
      error = function(e) {
        warning("Refit failed after removing ", term_to_remove, " : ", conditionMessage(e))
        return(NULL)
      }
    )
    if (is.null(new_model)) {
      if (verbose) cat("Could not refit; stopping.\n")
      break
    }
    
    # Save step in log
    removal_log <- rbind(
      removal_log,
      data.frame(step = iter,
                 term_removed = term_to_remove,
                 p_value = term_pvals_no_int[term_to_remove],
                 formula = deparse(new_formula),
                 stringsAsFactors = FALSE)
    )
    current_model <- new_model
  }
  
  if (verbose) {
    cat("---- Final model ----\n")
    print(summary(current_model))
  }
  # cat("---- Original model ----\n")
  # print(summary(original_model))
  # cat("---- Final model after stepwise backward ----\n")
  # print(summary(current_model))
  
  return(list(
    original_model = original_model,
    backward_model = current_model,
    # original_model = original_model[["formula"]],
    # backward_model = current_model[["formula"]],
    final_model = current_model,
    removal_log = removal_log
  ))
}
# --- f(x) to perform GLM analyses
try_glm_stepwise <- function(frml, df, lag, eq, fmly, percentile,
                             p_threshold = 0.05) {
  # First, apply backward stepwise selection
  step_result <- tryCatch(
    backward_pval_trace(
      frml, df, family = fmly, p_threshold = p_threshold,
      force_hierarchy = TRUE,
      verbose = FALSE,
      na.action = na.omit
      ),
    error = function(e) {
      message(paste0(
        "Stepwise analysis not performed. Error for formulation ", eq, ":\n",
        deparse(frml), "\n",
        "Error: ", conditionMessage(e)
      ))
      return(NULL)
    }
  )
  
  if (is.null(step_result)) return(NULL)
  
  original_formula <- formula(step_result$original_model)
  backward_formula <- formula(step_result$backward_model)
  model <- step_result$final_model
  DE <- NA
  max_vif_info <- data.frame(variable = NA, max_vif = NA)
  collinearity_result <- NULL
  model_summary_txt = NULL #paste0("Error in lag ", i, ", frml ", eq, ": stepwise failed.\n")
  
  if (!is.null(model)) {
    
    # --- Check intercept ---
    intercept_sig <- summary(model)$coef[1, "Pr(>|t|)"] < p_threshold
    if (!intercept_sig) return(NULL)  # Intercept is not significant -> discard model
    
    
    DE <- round((1 - model[["deviance"]] / model[["null.deviance"]]) * 100, digits = 2)
    pv <- summary(model)$coef[, "Pr(>|t|)"]
    covs <- attr(model$terms, "term.labels")
    cov_i <- unique(unlist(strsplit((covs[grepl(":", covs, fixed = TRUE)]), ':')))
    cov_s <- covs[-grep(pattern = ':', covs, fixed = TRUE)]
    covs_d <- setdiff(cov_s, cov_i)
    
    # p-values excluding the intercept
    pvals_no_intercept <- pv[-1]
    intercept_sig <- pv[1] < p_threshold
    
    if (intercept_sig &&
        ((all(pvals_no_intercept < p_threshold) & length(pvals_no_intercept) > 0) |
        (any(pvals_no_intercept[grepl(":", covs, fixed = TRUE)] < p_threshold) &
         all(pvals_no_intercept[covs_d] < p_threshold)))) {
      max_vif_info <- tryCatch({
        n_terms <- length(covs)
        if (n_terms <= 1) {
          varname_single <- if(length(covs) == 1) covs[1] else NA
          data.frame(variable = varname_single, max_vif = 0)
        } else {
          collinearity_result <- check_collinearity(model)
          vif_df <- as.data.frame(collinearity_result)
          if (!is.null(vif_df) && "VIF" %in% names(vif_df) && nrow(vif_df) > 0) {
            idx <- which.max(vif_df$VIF)
            data.frame(
              variable = vif_df$Term[idx],
              max_vif = round(vif_df$VIF[idx], 2)
            )
          } else {
            data.frame(variable = "no_vif", max_vif = 0)
          }
        }
      }, warning = function(w) {
        if (grepl("Not enough model terms", conditionMessage(w))) {
          data.frame(variable = "single", max_vif = 0)
        } else warning(w)
      }, error = function(e) {
        data.frame(variable = NA, max_vif = NA)
      })
      # Capture the full summary as a character vector
      model_summary_txt <- capture.output({
        cat(paste0(
          "percentile: ", percentile,
          "; family:", fmly[["family"]], "(link=", fmly[["link"]], ")",
          "; lag:", sprintf("%02d", lag),
          "; eq:", sprintf("%02d", eq),
          "; DE:", DE, "\n"
        ))
        cat("original: ", paste(deparse(original_formula), collapse=""), "\n")
        cat("backward: ", paste(deparse(backward_formula), collapse=""), "\n")
        print(summary(model))
        if (!is.null(collinearity_result)) print(collinearity_result)
        cat('---------- ######### ---------- ',"\n")
      })
    } else {
      # f criteria are not met, return NULL to indicate non-significance
      return(NULL)
    } 
  }
  
  # Prior to converting formulas to text
  if (!is.null(model)) {
    intercept_sig <- summary(model)$coef[1, "Pr(>|t|)"] < p_threshold
    if (!intercept_sig) {
      return(NULL)  # Intercept not significant -> discard model
    }
  }
    
  # Convert to text format
  formula_to_string <- function(f) {
    paste(deparse(f), collapse = " ")
  }
  
  return(list(
    lag = lag,
    eq = eq,
    DE = DE,
    VIF = max_vif_info$max_vif,
    var = max_vif_info$variable,
    o_model = formula_to_string(original_formula),
    b_model = formula_to_string(backward_formula),
    # formula = deparse(eq),
    # final_model = deparse(model)
    # removal_log = deparse(step_result$removal_log) # store stepwise result
    # summary_txt = model_summary_txt  # all text
    summary_txt = if(!is.null(model_summary_txt))
      model_summary_txt else "model not fitted or not significant"
  ))
}

# --- f(x) to add percentile sufix to enviromental variables
add_th_suffix <- function(formula, th) {
  th <- as.character(th)
  f_txt <- deparse(formula)
  env_vars <- c("riv", "sst", "sss", "kd", "wmi")
  for (v in env_vars) {
    f_txt <- gsub(paste0("\\b", v, "\\b"), paste0(v, th), f_txt)
  }
  return(as.formula(f_txt))
  # f_txt_single <- paste(f_txt, collapse = " ")
}

# --- f (x) to generate all possible equations
generate_glm_formulas <- function(names_col,
                                  th = c("25", "50", "75"),
                                  response = "y",
                                  comb_glmi,
                                  to_vec,
                                  to_list) {
  glms_formulas_by_ix <- list()
  for(k in 1:length(th)){
    ## Variable list and index selection
    vq <- c(th[k], response)
    idx_var <- to_vec(
      for(i in 1:length(vq)) str_which(names_col, vq[i], negate = FALSE)
    )
    names_var <- c('soi','sam','enso', names_col[idx_var])
    
    # Climate index combinations
    ci <- c('soi','sam','enso')
    cixs <- to_list(
      for(i in 1:length(ci)) c(combn(ci, i, simplify = FALSE))
    )
    
    cixss <- list()
    cixss[[1]] <- cixs[[2]] # pairs
    cixss[[1]][[4]] <- unlist(cixs[[3]]) # all
    
    for(ix in 1:length(cixss[[1]])){
      cith <- paste(paste(cixss[[1]][[ix]], collapse = '|'), sep = '|')
      names_var_ix <- names_var[-grep(pattern = cith, names_var)]
      
      # Remove percentile tag
      for(tag in th[k]) names_var_ix <- gsub(tag,'', names_var_ix)
      
      # Covariates and response
      xnames <- names_var_ix # head(names_var_ix)
      ynames <- response
      
      # Formulas with interactions
      inter <- c("riv", "kd", "sss")
      glms_f <- comb_glmi(xnames, ynames, inter)
      
      if(ix < length(cixss[[1]])){
        glms_f <- glms_f[grep(pattern = paste(cixss[[1]][[4]], collapse = '|'), glms_f)]
      }
      
      glms_formulas_by_ix[[ix]] <- glms_f
    }
  }
  # Combine all formulas into a single object
  glms_formulas <- do.call(c, glms_formulas_by_ix)
  return(glms_formulas)
}

# --- f (x) to print the summary of the models
print_glm_nested_results <- function(glm_result) {
  
  for (rp in names(glm_result)) {  # response
    cat("\n==============================\n")
    cat("RESPONSE:", rp, "\n")
    cat("==============================\n\n")
    
    for (p in names(glm_result[[rp]])) {  # percentiles
      cat(">> Percentile:", p, "\n")
      cat("--------------------------------------\n")
      
      for (lg in names(glm_result[[rp]][[p]])) {  # lags
        df_lag <- glm_result[[rp]][[p]][[lg]]
        
        if (!is.null(df_lag) && nrow(df_lag) > 0) {
          
          # Keeping only one of repeated models
          glms_summary_final_first <- df_lag %>%
            dplyr::group_by(lag, backward) %>%
            dplyr::summarise(
              eq = first(eq),
              DE = first(DE),
              VIF = first(VIF),
              var = first(var),
              summary = list(first(summary)),
              .groups = "drop"
            )
          
          cat("\n--- LAG:", lg, " ---\n")
          
          for (i in seq_len(nrow(glms_summary_final_first))) {
            cat("\nModel", i, "of", nrow(glms_summary_final_first), "\n")
            cat("Eq:", glms_summary_final_first$eq[i], "\n")
            cat("DE:", glms_summary_final_first$DE[i], 
                " | VIF:", glms_summary_final_first$VIF[i],
                " | Var:", glms_summary_final_first$var[i], "\n")
            cat("Backward:", glms_summary_final_first$backward[i], "\n\n")
            
            # Print summary
            print(glms_summary_final_first$summary[[i]])
            cat("\n----------------------------------------------------\n")
          }
        }
      } # lag
    } # percentile
  } # response
}

#  --- f(x) to run all GLM analyses
run_glms_nested <- function(nvariables,
                            env_fish_lag,
                            lag,
                            th,
                            glms_formulas,
                            fmly,
                            add_th_suffix,
                            try_glm_stepwise) {
  response <- tail(colnames(env_fish_lag[[1]]), nvariables)
  glms_summary_by_response <- list()
  for(rp in response){
    glms_summary_by_percentile <- list()
    for(p in seq_along(th)){
      glms_summary_by_lag <- list()
      for(lg in seq_along(lag)){
        spq <- env_fish_lag[[lg]]
        glms_summary_eq <- data.frame(
          lag = rep(NA, length(glms_formulas)),
          eq = NA,
          DE = NA,
          VIF = NA,
          var = NA,
          original = NA,
          backward = NA,
          summary = I(vector("list", length(glms_formulas)))  # list column
        )
        colnames(glms_summary_eq) <- c('lag',
                                       'eq',
                                       'DE',
                                       'VIF',
                                       'var',
                                       'original',
                                       'backward',
                                       'summary')
        for (eq in seq_along(glms_formulas)) {
          frml <- glms_formulas[[eq]]
          frml <- update(frml, as.formula(paste(rp, "~ .")))
          frml <- add_th_suffix(frml, th[p])
          glms_formulas_th <- lapply(glms_formulas, add_th_suffix, th[p])
          output <- try_glm_stepwise(frml, spq, lg - 1, eq, fmly[[1]], th[p])
          if (!is.null(output)) {
            glms_summary_eq[eq, ] <- list(
              output$lag,
              output$eq,
              output$DE,
              output$VIF,
              output$var,
              gsub("\\s*\\*\\s*", " * ", deparse(output$o_model)),
              deparse(output$b_model),
              list(output$summary_txt)
            )}
          glms_summary_eq <- glms_summary_eq[!is.na(glms_summary_eq$VIF), ]
          # Get formula columns as a vector, removing backslashes, double quotes and spaces
          backward_chr <- trimws(gsub('\\\\|"', '', glms_summary_eq[, "backward"]))
          original_chr <- trimws(gsub('\\\\|"', '', glms_summary_eq[, "original"]))
          # Conditions to keep formulas
          cond1 <- backward_chr == original_chr # backward is equal to original
          cond2 <- !(backward_chr %in% glms_formulas_th) # backward is not in original formula's list
          # cond2 <- !(backward_chr %in% glms_formulas) # backward is not in original formula's list
          keep_idx <- cond1 | cond2
          glms_summary_eq_filtered <- unique(glms_summary_eq[keep_idx, , drop = FALSE])
          # Get only the first occurrence
          glms_summary_eq_filtered <- glms_summary_eq_filtered %>%
            group_by(lag, backward) %>%
            slice(1) %>%
            ungroup()
          # Get all original equations that converge backward
          # glms_summary_eq_filtered <- glms_summary_eq_filtered %>%
          #   group_by(lag, backward) %>%
          #    summarise(
          #     eq = first(eq),
          #     DE = first(DE),
          #     VIF = first(VIF),
          #     original = list(unique(original)),
          #     .groups = "drop"
          #    )
        } # closes f - formulas
        if(nrow(glms_summary_eq_filtered) > 0) {
          glms_summary_eq_filtered$percentile <- th[p]
        }
        # glms_summary_by_ix[[ix]] <- glms_summary_eq_filtered
        # } # closes ix -
        glms_summary_by_lag[[lg]] <- glms_summary_eq_filtered #glms_summary_by_ix
      } # closes lg - lag
      glms_summary_by_lag <- setNames(glms_summary_by_lag, c(paste0('lag_',lag)))
      glms_summary_by_percentile[[p]] <- glms_summary_by_lag
      # glms_summary_by_th[[p]] <- list(th = th[p],results = glms_summary_by_lag)
    } # closes p - percentile
    glms_summary_by_percentile <- setNames(glms_summary_by_percentile, c(th))
    glms_summary_by_response[[rp]] <- glms_summary_by_percentile
  }
  return(glms_summary_by_response)
}
# 
# --- f(x) to evaluate deviance and vif explained (max, mean, sd) by each lag 
table_de_lag_vif_glm <- function(dev, lagv, vifv, varv, tag) {
  # Create initial data frame
  df <- data.frame(lag = lagv, DE = as.numeric(dev), VIF = as.numeric(vifv), var = varv)
  # Create logical mask for filtering based on VIF thresholds
  interaction_term <- grepl(":", df$var)
  keep_rows <- (interaction_term & df$VIF < 10) | (!interaction_term & df$VIF < 5)
  # Filter the data
  df <- df[keep_rows, ]
  if (nrow(df) == 0) {
    # No valid rows, return full NA table
    dfna = data.frame(lag = 0:10, max = NA, avg = NA, sd = NA, n = NA, state = tag)
    return(dfna)
  }
  # Aggregated statistics
  dfmx <- aggregate(DE ~ lag, data = df, FUN = max)
  dfav <- aggregate(DE ~ lag, data = df, FUN = mean)
  dfsd <- aggregate(DE ~ lag, data = df, FUN = sd)
  dfn  <- aggregate(DE ~ lag, data = df, FUN = length)
  # Merge into final summary
  df2 <- data.frame(
    lag = dfmx$lag,
    max = dfmx$DE,
    avg = dfav$DE,
    sd  = dfsd$DE,
    n   = dfn$DE,
    state = tag
  )
  # Add NA rows for missing lags
  dfna = data.frame(lag = 0:10, max = NA, avg = NA, sd = NA, n = NA, state = tag)
  missing_lags <- setdiff(dfna$lag, df2$lag)
  if (length(missing_lags) > 0) {
    df2 <- rbind(df2, dfna[dfna$lag %in% missing_lags, ])
  }
  # Sort and reset rownames
  output <- df2[order(df2$lag), ]
  rownames(output) <- NULL
  return(output)
}
# --- f(x) to get statistics per lag for GLM 
tables_states_glm <- function(glms_list, tag) {
  ths <- names(glms_list)
  df_names <- names(glms_list[[ths[1]]])
  results_list <- lapply(ths, function(current_th) {
    lag <- vector_de_lag(glms_list, current_th, df_names, 'lag')
    de  <- vector_de_lag(glms_list, current_th, df_names, 'DE')
    vif <- vector_de_lag(glms_list, current_th, df_names, 'VIF')
    var <- vector_de_lag(glms_list, current_th, df_names, 'var')
    table_de_lag_vif_glm(de, as.numeric(lag), vif, var, tag)
  })
  names(results_list) <- paste0("th", ths)
  return(results_list)
}

# --- f(x) to compute marginal effect data for a single predictor
compute_glm_effect_data <- function(model, focal_var, non_focal_vars,
                                    n_points = 100, ci_level = 0.95, range_max_factor = 0.05) {
  
  cleaned_data <- na.omit(model$data)
  
  # Means of non-focal variables (robust, no across)
  if (length(non_focal_vars) == 0) {
    means <- numeric()
  } else {
    means <- sapply(non_focal_vars, function(v) {
      x <- cleaned_data[[v]]
      mean(x[is.finite(x)], na.rm = TRUE)
    })
  }
  
  # Range for focal variable
  finite_values <- cleaned_data[[focal_var]][is.finite(cleaned_data[[focal_var]])]
  if (length(finite_values) == 0) stop(paste("No finite values for:", focal_var))
  
  min_val <- min(finite_values)
  max_val <- max(finite_values)
  range_end <- max_val * (1 + range_max_factor)
  var_range <- seq(min_val, range_end, length.out = n_points)
  
  # Build prediction dataset
  new_data <- tibble(!!focal_var := var_range)
  
  for (v in non_focal_vars) {
    new_data[[v]] <- means[[v]]
  }
  
  # Predict
  pred_link <- predict(model, newdata = new_data, type = "link", se.fit = TRUE)
  
  # CI
  alpha <- 1 - ci_level
  z_val <- qnorm(1 - alpha/2)
  
  fit_link <- pred_link$fit
  se_link <- pred_link$se.fit
  
  predictions <- data.frame(
    new_data,
    fit_response = fit_link,
    lower_ci = fit_link - z_val * se_link,
    upper_ci = fit_link + z_val * se_link
  )
  
  return(predictions)
}

# --- f(x) to iterate and compute all marginal effects from a model
compute_glm_all_effects <- function(model) {
  
  # Get the model formula and its terms
  formula <- formula(model)
  
  # The response variable (left side of the formula)
  response_var <- all.vars(formula)[[1]] 
  
  # All predictor variables (right side of the formula)
  predictor_vars <- all.vars(formula)[-1] 
  
  # Initialize an empty list to store the effect data frames
  effects_list <- list()
  
  # Loop over all predictor variables
  for (current_focal_var in predictor_vars) {
    
    # 1. Identify the non-focal variables for the current loop
    # Non-focal = all predictors MINUS the focal variable
    current_non_focal_vars <- setdiff(predictor_vars, current_focal_var)
    
    # 2. Generate the prediction data for the current focal variable
    # We call the 'worker' function that calculates means, range, and CI
    effect_data <- compute_glm_effect_data(
      model = model, 
      focal_var = current_focal_var, 
      non_focal_vars = current_non_focal_vars
    )
    
    # 3. Store the resulting data frame in the list
    effects_list[[current_focal_var]] <- effect_data
  }
  
  # Return the list containing one data frame per predictor
  return(effects_list)
}

# --- f(x) to plot a single marginal GLM effect ---
plot_glm_effect <- function(prediction_data, 
                            model_data, 
                            focal_var, 
                            line_color = "black", 
                            plot_title = NULL,
                            y_label = NULL
                            ) {
  
  # Define Y-axis limits (automatic)
  min_pred_y <- floor(min(prediction_data$lower_ci, na.rm = TRUE) * 10) / 10
  max_pred_y <- ceiling(max(prediction_data$upper_ci, na.rm = TRUE) * 10) / 10
  
  # Calculate X-axis limits (automatic)
  min_pred_x <- min(prediction_data[[focal_var]])
  max_pred_x <- max(prediction_data[[focal_var]])
  
  # Create clean labels by removing digits (e.g., "riv75" -> "riv")
  label_x <- gsub("\\d+", "", focal_var) 
  
  num_breaks <- 5 # Number of breaks/points for the Y-axis

  # Create the graph
  g <- ggplot(prediction_data, aes_string(x = focal_var, y = "fit_response")) +
    
    # Confidence Interval Ribbon
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), 
                alpha = 0.5, fill = line_color) + # 
    
    # Predicted Mean Line
    geom_line(color = line_color, linewidth = 1) +
    
    # Rug plot
    geom_rug(data = model_data, 
             aes_string(x = focal_var, y = "NULL"), 
             inherit.aes = FALSE, 
             sides = "b", color = "black", alpha = 0.6) + 
    
    # Set axis limits
    scale_x_continuous(limits = c(min_pred_x, max_pred_x),
                       breaks = seq(min_pred_x, max_pred_x, length.out = num_breaks),
                       labels = function(breaks) {
                         if (max(breaks) > 100) {
                           return(round(breaks, 0)) # no decimal places
                         }
                         else if (max(abs(breaks)) <= 100 & max(abs(breaks)) > 10) {
                           return(sprintf("%.1f", breaks)) # one decimal place
                         }
                         else {
                           return(sprintf("%.2f", breaks)) # two decimal places
                         }}) +
    scale_y_continuous(limits = c(min_pred_y, max_pred_y),
                       # breaks = seq(min_pred_y, max_pred_y, length.out = 4),
                       breaks = seq(min_pred_y, max_pred_y, length.out = num_breaks),
                       labels = function(breaks) {
                         # Use two decimals for consistent alignment (e.g., 0.00, -2.50)
                         ifelse(abs(breaks) < 0.01,
                                "0.00",
                                sprintf("%.2f", breaks)
                         )}) +
                         # )
    coord_cartesian(xlim = c(min_pred_x, max_pred_x)) + 
    
    # Labs
    labs(title = paste(plot_title), #, "of", label_x), 
         y = y_label,
         x = label_x) +
    
    # Themes
    theme_tufte() +
    theme(axis.line = element_line(colour = "black"),
          axis.ticks = element_line(colour="black")) +
    theme(axis.text = element_text(color = "black", size = 12)) +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="plain")) +
    theme(
      legend.position = "none"
    )
  return(g)
}

# --- f(x) to generate ggpredict terms for one or two variables ---
predict_terms_auto <- function(model,
                               focal_var,
                               moderator_var = NULL,
                               n_points = 8,
                               probs = c(0.1, 0.9),
                               expand_range = 0.05) {
  # Extract model data
  model_data <- model$model
  
  # --- FOCAL VARIABLE ---
  focal_min <- min(model_data[[focal_var]], na.rm = TRUE)
  focal_max <- max(model_data[[focal_var]], na.rm = TRUE)
  by_focal <- (focal_max - focal_min) / (n_points - 1)
  focal_seq <- seq(focal_min, focal_max, by = by_focal)
  
  # Expand range by a given proportion (e.g., 5%)
  focal_range <- focal_max - focal_min
  focal_min_exp <- focal_min - focal_range * expand_range
  focal_max_exp <- focal_max + focal_range * expand_range
  
  # Sequence for focal variable
  by_focal <- (focal_max_exp - focal_min_exp) / (n_points - 1)
  focal_seq <- seq(focal_min_exp, focal_max_exp, by = by_focal)
  
  # --- SINGLE VARIABLE CASE ---
  if (is.null(moderator_var)) {
    pred <- ggpredict(
      model,
      terms = list(setNames(list(focal_seq), focal_var)),
      ci.lvl = 0.95
    )
    return(pred)
  }
  
  # --- INTERACTION CASE ---
  # Moderator levels defined by percentiles (user controlled)
  moderator_levels <- round(quantile(
    model_data[[moderator_var]], probs = probs, na.rm = TRUE
  ), 2)
  
  # Generate predictions with both variables
  pred <- ggpredict(
    model,
    terms = c(
      paste0(focal_var, " [", paste(round(focal_seq, 3), collapse = ","), "]"),
      paste0(moderator_var, " [", paste(moderator_levels, collapse = ","), "]")
    ),
    ci.lvl = 0.95
  )
  
  return(pred)
}



# --- f(x) to plot GLM interaction effect ---
plot_glm_interaction <- function(model,
                                 model_data,
                                 focal_var,
                                 moderator_var,
                                 moderator_levels = NULL,
                                 probs = c(0.1, 0.9),   # <-- new input for percentiles
                                 custom_colors = NULL,
                                 plot_title = NULL,
                                 y_label = NULL) {
  library(ggeffects)
  library(ggplot2)
  library(ggthemes)
  
  # --- Generate predictions automatically ---
  pred <- predict_terms_auto(model, focal_var, moderator_var, probs = probs)
  
  # --- Format groups for legend ---
  pred$group <- factor(
    ifelse(
      abs(as.numeric(as.character(pred$group))) < 1,
      round(as.numeric(as.character(pred$group)), 2),
      round(as.numeric(as.character(pred$group)), 0)
    )
  )
  
  # --- Axis limits ---
  min_pred_y <- floor(min(pred$conf.low, na.rm = TRUE) * 10) / 10
  max_pred_y <- ceiling(max(pred$conf.high, na.rm = TRUE) * 10) / 10
  min_pred_x <- min(pred$x, na.rm = TRUE)
  max_pred_x <- max(pred$x, na.rm = TRUE)
  num_breaks <- 5
  
  label_x <- gsub("\\d+", "", focal_var)
  label_legend <- gsub("\\d+", "", moderator_var)
  
  # --- Colors and legend labels ---
  n_groups <- length(unique(pred$group))
  legend_labels <- paste0(label_legend, " = ", levels(pred$group))
  colors <- if (!is.null(custom_colors)) {
    custom_colors
  } else {
    RColorBrewer::brewer.pal(max(3, n_groups), "Dark2")[seq_len(n_groups)]
  }
  
  # --- Plot ---
  # Plot
  g <- ggplot(pred, aes(x = x, y = predicted, color = group, fill = group)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                alpha = 0.5, colour = NA) +
    geom_line(linewidth = 1) +
    geom_rug(data = model_data,
             aes_string(x = focal_var, y = "NULL"),
             inherit.aes = FALSE,
             sides = "b", color = "black", alpha = 0.6) +
    scale_x_continuous(
      limits = c(min_pred_x, max_pred_x),
      # limits = range(model_data[[focal_var]], na.rm = TRUE), #c(min_pred_x, max_pred_x),
      # expand = expansion(mult = c(0.05, 0.05)),
      breaks = seq(min_pred_x, max_pred_x, length.out = num_breaks),
      labels = function(breaks) {
        if (max(breaks) > 100) round(breaks, 0)
        else if (max(abs(breaks)) <= 100 & max(abs(breaks)) > 10) sprintf("%.1f", breaks)
        else sprintf("%.2f", breaks)
      }
    ) +
    scale_y_continuous(
      limits = c(min_pred_y, max_pred_y),
      breaks = seq(min_pred_y, max_pred_y, length.out = num_breaks),
      labels = function(breaks) ifelse(abs(breaks) < 0.01, "0.00", sprintf("%.2f", breaks))
    ) +
    coord_cartesian(xlim = c(min_pred_x, max_pred_x)) +
    scale_color_manual(
      name = label_legend,
      labels = legend_labels,
      values = colors
    ) +
    scale_fill_manual(
      name = label_legend,
      labels = legend_labels,
      values = colors
    ) +
    labs(
      title = plot_title,
      y = y_label,
      x = label_x,
      color = moderator_var,
      fill = moderator_var
    ) +
    theme_tufte() +
    theme(
      axis.line = element_line(colour = "black"),
      axis.ticks = element_line(colour = "black"),
      axis.text = element_text(color = "black", size = 12),
      axis.title = element_text(size = 14, face = "plain"),
      legend.title = element_blank(), #element_text(element_blank(), size = 13, face = "italic"),
      legend.text = element_text(size = 12),
      legend.position = c(0.5, 0.85), #"bottom",
      legend.direction="vertical",
      plot.title = element_text(hjust = 0.5, face = "plain", size = 14)
    )
  
  return(g)
}
 
# GAM functions ----------------------------------------------------------------
# --- f(x) to get all combinations among explanatory variables - INTERACCTION
#     considering the interaction of "x1" and "x2" with "x3"
comb_gami <- function(xnam, ynam, kdf, inter){
  lstls <- list()
  cnum <- length(xnam) # length explanatory variables
  # List comprehension. Getting all combinations of explanatory variables
  lstls <- to_list(for(i in 1:cnum) 
    c(combn(xnam, i, simplify = FALSE)))
  for(i in 1:cnum){
    k <- length(lstls[[i]])
    for(j in 1:k){
      covariates <- lstls[[i]][[j]] # covariates names
      if (
        inter[1] %in% covariates &
        inter[2] %in% covariates & !(inter[3] %in% covariates)
      ){
        inter_t <- paste(
          "ti(", paste(inter[c(1,2)],
                       collapse = ", "), ", k = ", kdf, ", bs = 'cr')")
      } else if (
        inter[1] %in% covariates &
        !inter[2] %in% covariates & (inter[3] %in% covariates)
      ){
        inter_t <- paste(
          "ti(", paste(inter[c(1,3)],
                       collapse = ", "), ", k = ", kdf, ", bs = 'cr')")
      } else if (
        inter[1] %in% covariates &
        inter[2] %in% covariates & (inter[3] %in% covariates)
      ){
        inter_t <- paste(
          paste("ti(", paste(inter[c(1,2)],
                             collapse = ", "), ", k = ", kdf, ", bs = 'cr')"),
          "+",
          paste("ti(", paste(inter[c(1,3)],
                             collapse = ", "), ", k = ", kdf, ", bs = 'cr')"))
      } else {
        inter_t <- NULL
      }
      if (
        !is.null(inter_t)
      ){
        lstls[[i]][[j]] <- (as.formula(paste(
          paste(ynam), " ~ ",
          paste("s(", lstls[[i]][[j]], 
                ", k = ", kdf, ", bs = 'cr')", collapse = "+"),
          "+", inter_t)))
      } else {
        lstls[[i]][[j]] <- (as.formula(paste(
          paste(ynam), " ~ ",
          paste("s(", lstls[[i]][[j]], 
                ", k = ", kdf, ", bs = 'cr')", collapse = "+"))))
      }
    }
  }
  lst <- matrix(unlist(lstls), ncol = 1, byrow = TRUE)
  lst <- c(as.formula(paste(paste(ynam), " ~ 1")), lst) # Adding NULL model
}
# --- f(x) 1: extract p-values
get_pvalues <- function(model) {
  pv <- c(summary.gam(model)$p.pv,
          summary(model)$s.table[, "p-value"])
  names(pv) <- c("(Intercept)", rownames(summary(model)$s.table))
  pv
}
# --- f(x) 2: extract smooth terms
get_covariates <- function(model) {
  covs <- rownames(summary(model)$s.table)
  cov_s <- covs[startsWith(covs, 's(') ] 
  cov_s <- gsub("[\\(\\)]", "",
                regmatches(cov_s, gregexpr("\\(.*?\\)", cov_s)))
  cov_i <- gsub("[^[:alnum:] ]", "", 
                gsub("ti", "", 
                     unlist(strsplit(covs[grepl("ti", covs, fixed = TRUE)], ","))))
  list(all = covs, smooth = cov_s, ti = cov_i)
}
# --- f(x) 3: compute concurvity summary
get_concurvity <- function(model, full = TRUE) {
  ccv <- concurvity(model, full = full)
  
  if (full) {
    # In full = TRUE mode, the result is a matrix
    worst <- ccv["worst", , drop = TRUE]
    round(worst, 2)
  } else {
    # In full = FALSE mode, the result is a list of matrices
    w <- ccv$worst
    diag(w) <- NA
    round(apply(w, 2, max, na.rm = TRUE), 2)
  }
}

# --- f(x) to perform stepwise backward
backward_pval_trace_gam <- function(formula_full, data, family = gaussian(),
                                    p_threshold = 0.05, force_hierarchy = TRUE,
                                    verbose = FALSE, na.action = na.omit,
                                    method = "REML") {
  # helper: simplifies a label like "s(x, k=4, bs='cr')" -> "s(x)"
  simplify_label <- function(lbl) {
    lbl_chr <- as.character(lbl)
    # function name (before the parenthesis)
    fn <- sub("\\s*\\(.*", "", lbl_chr)
    inside <- sub(".*\\((.*)\\).*", "\\1", lbl_chr)
    parts <- strsplit(inside, ",")[[1]]
    parts <- trimws(parts)
    # keep only the first elements without '=' (these are variables)
    vars <- parts[!grepl("=", parts)]
    # if there are more than 2 variables, keep them all; paste without spaces
    paste0(fn, "(", paste(vars, collapse = ","), ")")
  }
  
  # Initial model fit
  original_model <- mgcv::gam(formula = formula_full, data = data,
                              family = family, na.action = na.action, method = method)
  current_model <- original_model
  iter <- 0L
  
  removal_log <- data.frame(
    step = integer(),
    term_removed = character(),
    p_value = numeric(),
    formula = character(),
    stringsAsFactors = FALSE
  )
  
  repeat {
    iter <- iter + 1L
    s <- summary(current_model)
    
    # p-values of parametric and smooth terms
    p_param <- if (!is.null(s$p.table)) s$p.table[, 4] else numeric(0)
    terms_param <- if (!is.null(s$p.table)) rownames(s$p.table) else character(0)
    
    # Exclude intercept if present
    if ("(Intercept)" %in% terms_param) {
      keep_idx <- which(terms_param != "(Intercept)")
      p_param <- p_param[keep_idx]
      terms_param <- terms_param[keep_idx]
    }
    
    p_smooth <- if (!is.null(s$s.table)) s$s.table[, 4] else numeric(0)
    terms_smooth <- if (!is.null(s$s.table)) rownames(s$s.table) else character(0)
    
    
    term_pvals <- c(p_param, p_smooth)
    names(term_pvals) <- c(terms_param, terms_smooth)
    term_pvals <- term_pvals[!is.na(term_pvals)]
    
    if (length(term_pvals) == 0) {
      if (verbose) cat("No remaining terms. Stop.\n")
      break
    }
    
    ord <- order(term_pvals, decreasing = TRUE, na.last = TRUE)
    candidate_terms <- names(term_pvals)[ord]
    
    if (verbose) {
      cat("---- Iter:", iter, " ----\n")
      cat("Current formula:", paste(deparse(formula(current_model)), collapse = " "), "\n")
      print(round(term_pvals, 4))
    }
    
    # Select term to remove (worst p-value > threshold, respecting hierarchy)
    term_to_remove <- NULL
    for (t in candidate_terms) {
      if (is.na(term_pvals[t])) next
      if (term_pvals[t] <= p_threshold) { term_to_remove <- NULL; break }
      
      # --- Hierarchy Logic ---
      if (force_hierarchy) {
        # 1. Identify if 't' is a main effect (i.e., NOT an interaction 'ti' or ':')
        is_main_effect <- !grepl("ti\\(", t) && !grepl(":", t)
        if (is_main_effect) {
          # 2. Extract the base variable name from 't' "s(riv25)" -> "riv25"
          var_base <- sub("^s\\(([^,)]+).*\\)$", "\\1", t)
          # 3. Find all interactions (ti and :) still in the model
          all_interactions <- grep("ti\\(|:", names(term_pvals), value = TRUE)
          # 4. Check if 'var_base' appears in any interaction
          #    Use \\b (word boundary) so 'riv25' doesn’t match 'riv250'
          is_protected <- any(grepl(paste0("\\b", var_base, "\\b"), all_interactions))
          if (is_protected) {
            if (verbose) cat("Skipping", t, "(protected by interaction)\n")
            next # Skip this term; it’s protected
          }
        }
      }
      
      # If not skipped, this is the term to remove
      term_to_remove <- t
      break
    }
    
    if (is.null(term_to_remove)) {
      if (verbose) cat("No terms with p-value >", p_threshold, " -> stop.\n")
      break
    }
    
    if (verbose) cat("Removing term:", term_to_remove,
                     "; p-value =", formatC(term_pvals[term_to_remove], digits = 4), "\n")
    
    # get full Right-Hand Side and its original labels
    rhs_full <- attr(terms(formula(current_model)), "term.labels")
    if (length(rhs_full) == 0) {
      if (verbose) cat("No RHS terms to remove. Stop.\n")
      break
    }
    # build simplified labels for matching
    rhs_simple <- vapply(rhs_full, simplify_label, FUN.VALUE = character(1))
    
    # find index of the term to remove in the simplified version
    idx_rm <- which(rhs_simple == term_to_remove)
    if (length(idx_rm) == 0) {
      # alternative attempt: remove spaces and compare
      rhs_nospace <- gsub("\\s+", "", rhs_full)
      term_nospace <- gsub("\\s+", "", term_to_remove)
      idx_rm <- which(rhs_nospace == term_nospace)
    }
    if (length(idx_rm) == 0) {
      # if still not found, try matching by pattern (as safety)
      idx_rm <- which(grepl(gsub("\\(", "\\\\(", gsub("\\)", "\\\\)", term_to_remove)), rhs_full, fixed = FALSE))
    }
    
    if (length(idx_rm) == 0) {
      if (verbose) {
        cat("No match found between summary-term and formula terms for:",
            term_to_remove, "\nStopping to avoid wrong removal.\n")
      }
      break
    }
    
    # rebuild RHS without the removed term
    rhs_new <- rhs_full[-idx_rm]
    # if empty -> intercept-only formula
    if (length(rhs_new) == 0) {
      new_formula <- as.formula(paste(deparse(formula(current_model)[[2]]), "~ 1"))
    } else {
      new_formula <- as.formula(paste(deparse(formula(current_model)[[2]]),
                                      "~", paste(rhs_new, collapse = " + ")))
    }
    
    # check for real change
    f_old <- paste(deparse(formula(current_model)), collapse = "")
    f_new <- paste(deparse(new_formula), collapse = "")
    if (identical(f_old, f_new)) {
      if (verbose) cat("Formula unchanged after removal. Stop.\n")
      break
    }
    
    # refit
    new_model <- tryCatch(
      gam(new_formula, data = data, family = family,
          na.action = na.action, method = method),
      error = function(e) {
        warning("Refit failed after removing ", term_to_remove, " : ", conditionMessage(e))
        return(NULL)
      }
    )
    if (is.null(new_model)) {
      if (verbose) cat("Could not refit; stopping.\n")
      break
    }
    
    removal_log <- rbind(
      removal_log,
      data.frame(step = iter,
                 term_removed = unname(term_to_remove),
                 p_value = unname(term_pvals[term_to_remove]),
                 formula = paste(
                   deparse(new_formula), collapse = " "),
                 stringsAsFactors = FALSE)
    )
    current_model <- new_model
  }
  
  if (verbose) {
    cat("---- Final model ----\n")
    print(summary(current_model))
  }
  
  return(list(
    original_model = original_model,
    backward_model = current_model,
    final_model = current_model,
    removal_log = removal_log
  ))
}


# --- f(x) to perform GAM analyses
try_gam_stepwise <- function(frml, df, lag, eq, fmly, percentile,
                             p_threshold = 0.05,
                             concurvity_threshold = 0.3,
                             method = "REML") {
  
  # --- 1. Run Stepwise Backward ---
  step_result <- tryCatch(
    backward_pval_trace_gam(
      frml, df, family = fmly, p_threshold = p_threshold,
      force_hierarchy = TRUE,
      verbose = FALSE,
      na.action = na.omit,
      method = method
    ),
    error = function(e) {
      message(paste0(
        "Stepwise GAM analysis not performed. Error for formulation ", eq, ":\n",
        deparse(frml), "\n",
        "Error: ", conditionMessage(e)
      ))
      return(NULL)
    }
  )
  
  if (is.null(step_result)) return(NULL)
  
  # --- 2. Initialize variables ---
  original_formula <- formula(step_result$original_model)
  backward_formula <- formula(step_result$backward_model)
  model <- step_result$final_model
  DE <- NA
  max_concurvity_info <- data.frame(
    variable = NA_character_,
    max_concurvity = NA_real_
  )
  concurvity_result <- NULL
  model_summary_txt <- NULL
  
  if (is.null(model)) return(NULL)
  
  # --- 3. Final Model Validation ---
  # --- 3a. Intercept Validation (using get_pvalues)
  pv <- get_pvalues(model)
  
  if ("(Intercept)" %in% names(pv)) {
    if (is.na(pv["(Intercept)"]) || pv["(Intercept)"] >= p_threshold) {
      # Intercept is not significant or is NA, discard
      return(NULL) 
    }
  }
  # If there is no intercept (e.g., model ~ 0 + ...), continue
  
  # --- 3b. Deviance Explained Calculation ---
  DE <- round((1 - model[["deviance"]] / model[["null.deviance"]]) * 100, digits = 2)
  
  # --- 3c. Validation of Term Significance ---
  # (Note: This is a double check, since backward_... should have
  # already ensured this, but it's a good validation practice).
  
  if (length(pv) <= 1) return(NULL) # Model only has intercept
  
  covs <- get_covariates(model)
  covs_dpv <- setdiff(covs$smooth, covs$ti) # main effect smoothers
  
  is_significant <- (
    # Option 1: All p-values (including intercept) are significant
    (all(pv[!is.na(pv)] < p_threshold) & length(pv) > 1) |
      # Option 2: At least one interaction (ti) is significant AND
      # all main effects (s) are also significant
      (
        any(pv[names(pv) %in% covs$all[grepl("ti", covs$all, fixed = TRUE)]] < p_threshold, na.rm = TRUE) &
          all(pv[covs_dpv] < p_threshold, na.rm = TRUE)
      )
  )
  
  if (!is_significant) return(NULL)
  
  # --- 3d. Concurvity Check ---
  n_terms <- length(covs$all)
  
  if (n_terms == 0) {
    max_concurvity_info <- data.frame(variable = NA_character_, max_concurvity = 0)
  } else if (n_terms == 1) {
    max_concurvity_info <- data.frame(variable = covs$all[1], max_concurvity = 0)
  } else {
    # Run concurvity check
    concurvity_result <- tryCatch(
      get_concurvity(model), # Usa tu helper
      error = function(e) { 
        warning("Concurvity check failed: ", conditionMessage(e))
        return(NULL) 
      }
    )
    
    if (is.null(concurvity_result)) return(NULL) # Check failed
    
    # Check if any term exceeds the threshold
    if (any(concurvity_result > concurvity_threshold, na.rm = TRUE)) {
      return(NULL) # Model has high concurvity, discard
    }
    
    # If passes, record the maximum value for output
    if (length(concurvity_result) > 0) {
      max_idx <- which.max(concurvity_result)
      max_concurvity_info <- data.frame(
        variable = names(concurvity_result)[max_idx],
        max_concurvity = round(concurvity_result[max_idx], 2)
      )
    } else {
      max_concurvity_info <- data.frame(variable = "no_terms", max_concurvity = 0)
    }
  }
  
  # --- 4. Capture Summary ---
  model_summary_txt <- capture.output({
    cat(paste0(
      "percentile: ", percentile,
      "; family:", fmly[["family"]], "(link=", fmly[["link"]], ")",
      "; lag:", sprintf("%02d", lag),
      "; eq:", sprintf("%02d", eq),
      "; DE:", DE, "\n"
    ))
    cat("original: ", paste(deparse(original_formula), collapse=""), "\n")
    cat("backward: ", paste(deparse(backward_formula), collapse=""), "\n")
    
    print(summary(model))
    
    # Print concurvity
    if (!is.null(concurvity_result) && length(concurvity_result) > 0) {
      cat("\n--- Concurvity (Worst) ---\n")
      print(concurvity_result)
    }
    cat('---------- ######### ---------- ',"\n")
  })
  
  # --- 5. Prepare Final Output ---
  formula_to_string <- function(f) {
    paste(deparse(f), collapse = " ")
  }
  
  return(list(
    lag = lag,
    eq = eq,
    DE = DE,
    # Concurvity outputs
    max_cc_value = max_concurvity_info$max_concurvity,
    max_cc_term = max_concurvity_info$variable,
    # Formulas
    o_model = formula_to_string(original_formula),
    b_model = formula_to_string(backward_formula),
    # Text summary
    summary_txt = if(!is.null(model_summary_txt))
      model_summary_txt else "model not fitted or not significant"
  ))
}


# --- f(x) to run all GAM analyses
# Make sure dplyr is loaded for group_by() and slice()
# library(dplyr) 
run_gams_nested <- function(nvariables,
                            env_fish_lag,
                            lag,
                            th,
                            gams_formulas,
                            fmly,
                            add_th_suffix,
                            try_gam_stepwise, 
                            p_threshold = 0.05,
                            concurvity_threshold = 0.3, 
                            method = "REML") {
  
  response <- tail(colnames(env_fish_lag[[1]]), nvariables)
  gams_summary_by_response <- list()
  
  for(rp in response){
    gams_summary_by_percentile <- list()
    for(p in seq_along(th)){
      gams_summary_by_lag <- list()
      for(lg in seq_along(lag)){
        spq <- env_fish_lag[[lg]]
        
        # Pre-allocated data frame for GAM results
        gams_summary_eq <- data.frame(
          lag = rep(NA, length(gams_formulas)),
          eq = NA,
          DE = NA,
          max_cc_value = NA,
          max_cc_term = NA,
          original = NA,
          backward = NA,
          summary = I(vector("list", length(gams_formulas)))
        )
        colnames(gams_summary_eq) <- c('lag', 'eq', 'DE',
                                       'max_cc_value', 'max_cc_term',
                                       'original', 'backward', 'summary')
        
        for (eq in seq_along(gams_formulas)) {
          frml <- gams_formulas[[eq]]
          frml <- update(frml, as.formula(paste(rp, "~ .")))
          frml <- add_th_suffix(frml, th[p])
          
          # This is used in the filtering step later
          gams_formulas_th <- lapply(gams_formulas, add_th_suffix, th[p])
          
          # Call the GAM stepwise wrapper
          output <- try_gam_stepwise(
            frml, spq, lg - 1, eq, fmly[[1]], th[p],
            p_threshold = p_threshold,
            concurvity_threshold = concurvity_threshold,
            method = method
          )
          
          if (!is.null(output)) {
            # Store GAM-specific results
            gams_summary_eq[eq, ] <- list(
              output$lag,
              output$eq,
              output$DE,
              output$max_cc_value, 
              output$max_cc_term,  
              deparse(output$o_model), # Kept deparse to match your template
              deparse(output$b_model), # Kept deparse to match your template
              list(output$summary_txt)
            )
          }
        } # closes f - formulas
        
        # Filter based on concurvity column
        gams_summary_eq <- gams_summary_eq[!is.na(gams_summary_eq$max_cc_value), ]
        
        if (nrow(gams_summary_eq) > 0) {
          # This filtering logic is identical to your template
          backward_chr <- trimws(gsub('\\\\|"', '', gams_summary_eq[, "backward"]))
          original_chr <- trimws(gsub('\\\\|"', '', gams_summary_eq[, "original"]))
          
          cond1 <- backward_chr == original_chr
          cond2 <- !(backward_chr %in% gams_formulas_th)
          
          keep_idx <- cond1 | cond2
          gams_summary_eq_filtered <- unique(gams_summary_eq[keep_idx, , drop = FALSE])
          
          # Get only the first occurrence (requires dplyr)
          gams_summary_eq_filtered <- gams_summary_eq_filtered %>%
            group_by(lag, backward) %>%
            slice(1) %>%
            ungroup()
          
          if(nrow(gams_summary_eq_filtered) > 0) {
            gams_summary_eq_filtered$percentile <- th[p]
          }
          
          gams_summary_by_lag[[lg]] <- gams_summary_eq_filtered
          
        } else {
          # Ensure an empty data.frame is stored if no models pass
          gams_summary_by_lag[[lg]] <- gams_summary_eq[0, ] 
        }
        
      } # closes lg - lag
      gams_summary_by_lag <- setNames(gams_summary_by_lag, c(paste0('lag_',lag)))
      gams_summary_by_percentile[[p]] <- gams_summary_by_lag
    } # closes p - percentile
    gams_summary_by_percentile <- setNames(gams_summary_by_percentile, c(th))
    gams_summary_by_response[[rp]] <- gams_summary_by_percentile
  }
  return(gams_summary_by_response)
}

# --- f (x) to print the summary of the GAM models
print_gam_nested_results <- function(gam_result) {
  
  for (rp in names(gam_result)) {  # response
    cat("\n==============================\n")
    cat("RESPONSE:", rp, "\n")
    cat("==============================\n\n")
    
    for (p in names(gam_result[[rp]])) {  # percentiles
      cat(">> Percentile:", p, "\n")
      cat("--------------------------------------\n")
      
      for (lg in names(gam_result[[rp]][[p]])) {  # lags
        df_lag <- gam_result[[rp]][[p]][[lg]]
        
        if (!is.null(df_lag) && nrow(df_lag) > 0) {
          
          # Keeping only one of repeated models
          gams_summary_final_first <- df_lag %>%
            dplyr::group_by(lag, backward) %>%
            dplyr::summarise(
              eq = first(eq),
              DE = first(DE),
              max_cc_value = first(max_cc_value), # CHANGED from VIF
              max_cc_term = first(max_cc_term),   # CHANGED from var
              summary = list(first(summary)),
              .groups = "drop"
            )
          
          cat("\n--- LAG:", lg, " ---\n")
          
          for (i in seq_len(nrow(gams_summary_final_first))) {
            cat("\nModel", i, "of", nrow(gams_summary_final_first), "\n")
            cat("Eq:", gams_summary_final_first$eq[i], "\n")
            
            # CHANGED: Print concurvity instead of VIF
            cat("DE:", gams_summary_final_first$DE[i], 
                " | Max Concurvity:", gams_summary_final_first$max_cc_value[i],
                " | Term:", gams_summary_final_first$max_cc_term[i], "\n")
            
            cat("Backward:", gams_summary_final_first$backward[i], "\n\n")
            
            # Print summary (summary is a list-column)
            # Use cat() to print the text cleanly
            cat(gams_summary_final_first$summary[[i]], sep = "\n")
            
            cat("\n----------------------------------------------------\n")
          }
        }
      } # end lag loop
    } # end percentile loop
  } # end response loop
}

# --- f(x) to perform GAM analyses
try_gam <- function(eq, dataset, i, j, f, th, threshold){
  model <- tryCatch(
    gam(eq, family=f, data=dataset, na.action=na.omit, method="REML"),
    error = function(e) NULL
  )
  # Default output
  output <- list(
    lag = i, eq = j,
    DE = NA, cc_term = NA_character_, cc_value = NA_real_
    )
  if (is.null(model)) return(output)
  
  # Deviance explained
  DE <- round((1 - model$deviance/model$null.deviance) * 100, 2)
  
  # P-values
  pv <- get_pvalues(model)
  if (length(pv) <= 1) return(output)
  
  # Covariates
  covs <- get_covariates(model)
  covs_dpv <- setdiff(covs$smooth, covs$ti)
  
  # Significance test
  thr <- 0.05
  if (!(
    (all(pv[!is.na(pv)] < thr) & length(pv) > 1) |
    (any(pv[names(pv) %in% covs$all[grepl("ti", covs$all, fixed = TRUE)]] < thr, na.rm = TRUE) &
     all(pv[covs_dpv] < thr, na.rm = TRUE))
  ))
    return(output)
  
  # Concurvity
  max_per_col <- get_concurvity(model)
  if (any(max_per_col > threshold))
    return(output)
  # Select maximum concurvity term
  max_idx <- which.max(max_per_col)
  output <- list(
    lag = i, eq = j, DE = DE,
    cc_term = names(max_per_col)[max_idx],
    cc_value = max_per_col[max_idx]
  )
  
  # Print ouput
  cat('---------- ######### ---------- ',"\n")
  cat(paste0('percentile:',
             paste0(th),
             '; family:',
             paste0(f[["family"]],'(link=',f[["link"]],')', sep = ""),
             '; lag:',
             sprintf("%02d", i),
             '; eq:',
             sprintf("%02d", j),
             '; DE:',
             round(DE, digits = 2), "\n"))
  print(summary(model))
  cat("Maximum concurvity values per term:\n")
  print(max_per_col)
  return(output)
}
# 
# --- f(x) to evaluate deviance explained (max, mean, sd) by each lag filtering
#     by concurvity (cc_value) instead of VIF
table_de_lag_cc_gam <- function(dev, lagv, cc_val, cc_term, tag, cc_thr = 0.3) {
  # Create initial data frame
  df <- data.frame(lag = lagv, DE = as.numeric(dev),
                   cc_value = as.numeric(cc_val), cc_term = cc_term)
  # Keep rows under concurvity threshold
  df <- df[df$cc_value <= cc_thr, ]
  if (nrow(df) == 0) {
    dfna = data.frame(lag = 0:10, max = NA, avg = NA, sd = NA, n = NA, state = tag)
    return(dfna)
  }
  # Aggregated statistics
  dfmx <- aggregate(DE ~ lag, data = df, FUN = max)
  dfav <- aggregate(DE ~ lag, data = df, FUN = mean)
  dfsd <- aggregate(DE ~ lag, data = df, FUN = sd)
  dfn  <- aggregate(DE ~ lag, data = df, FUN = length)
  df2 <- data.frame(
    lag = dfmx$lag,
    max = dfmx$DE,
    avg = dfav$DE,
    sd  = dfsd$DE,
    n   = dfn$DE,
    state = tag
  )
  # Add missing lags (0–10) as NA
  dfna = data.frame(lag = 0:10, max = NA, avg = NA, sd = NA, n = NA, state = tag)
  missing_lags <- setdiff(dfna$lag, df2$lag)
  if (length(missing_lags) > 0) {
    df2 <- rbind(df2, dfna[dfna$lag %in% missing_lags, ])
  }
  output <- df2[order(df2$lag), ]
  rownames(output) <- NULL
  return(output)
}

# --- f(x) to get statistics per lag for GAMs
tables_states_gam <- function(gams_list, tag, cc_thr = 0.3) {
  ths <- names(gams_list)
  df_names <- names(gams_list[[ths[1]]])
  results_list <- lapply(ths, function(current_th) {
    lag <- vector_de_lag(gams_list, current_th, df_names, 'lag')
    de  <- vector_de_lag(gams_list, current_th, df_names, 'DE')
    cc_val <- vector_de_lag(gams_list, current_th, df_names, 'max_cc_value')
    cc_term <- vector_de_lag(gams_list, current_th, df_names, 'max_cc_term')
    table_de_lag_cc_gam(de, as.numeric(lag), cc_val, cc_term, tag, cc_thr = cc_thr)
  })
  names(results_list) <- paste0("th", ths)
  return(results_list)
}

# --- f(x) to plot a single marginal GAM effect ---
plot_gam_effect <- function(model, var_x, col_line, y_label = NULL){
  plot_elements <- plot(model, select = var_x)
  idx <- 1:length(plot_elements)
  new_names <- to_list(for(i in 1:length(idx))
    plot_elements[[i]][["xlab"]])
  
  names(plot_elements) <- unlist(new_names)
  rug = plot_elements[[var_x]]$raw
  df <- data.frame(
    x = plot_elements[[var_x]]$x,
    y = plot_elements[[var_x]]$fit, 
    ymin = plot_elements[[var_x]]$fit - plot_elements[[var_x]]$se, 
    ymax = plot_elements[[var_x]]$fit + plot_elements[[var_x]]$se,
    rug = c(rug, rep(NA, length(plot_elements[[var_x]][["x"]]) - length(rug)))
  )
  #
  xbreaks <- range(plot_elements[[var_x]]$raw)
  p<- ggplot(data = df, aes(x = x, y = y)) +
    geom_line(col = col_line[1]) +
    geom_ribbon(aes(x = x, ymin = ymin, ymax = ymax),
                alpha = 0.5, fill = col_line[1]) +
    geom_rug(
      data = df,
      aes(
        x = rug,
        y = Inf),
      col = "black", sides = "b", size = 0.3, inherit.aes = FALSE) +
    scale_x_continuous(
      breaks = seq(
        xbreaks[1] - abs(xbreaks[1] * 0.05),
        xbreaks[2] + abs(xbreaks[2] * 0.05),
        length.out = 5
      ),
      labels = label_number(accuracy = 0.01)) +
    scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
    labs(
      y = y_label,
      x = gsub("\\d+", "", plot_elements[[var_x]]$xlab)
    ) +
    theme_tufte() +
    theme(axis.line = element_line(colour = "black"),
          axis.ticks = element_line(colour="black")) +
    theme(axis.text = element_text(color = "black", size = 12)) +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="plain")) +
    theme(
      legend.title = element_blank(),
      legend.justification = c(0, 1),
      legend.position = c(0.05, 0.90),
      legend.direction = "vertical",
      legend.spacing.x = unit(0.25, 'cm'),
      legend.spacing.y = unit(1, 'cm'),
      legend.key.size = unit(1, "cm"),
      legend.background = element_blank(),
      legend.text = element_text(
        colour = "black",
        size = 14,
        face = "italic"
      )
    )
  return(p)
}

# GLM and GAM functions -------------------------------------------------------- 
# --- f(x) to get a vector containing lag or DE according to family and index
vector_de_lag <- function(lst, th, ix, vrbl){
  output <- to_vec(for(i in 1:length(ix))
    (lst[[th]][[ix[i]]][[vrbl]]))
  return(output)
}

#  --- f(x) to combine states and centroids into single list by percentiles
combine_by_percentiles <- function(thresholds, prefix, n_elements) {
  list_names <- paste0(prefix, seq_len(n_elements)) # Create names
  combined_list <- lapply(thresholds, function(th) {
    data_list <- lapply(list_names, function(name) get(name)[[th]])
    names(data_list) <- list_names # add names
    dplyr::bind_rows(data_list, .id = "source") # combine into a single df
  })
  names(combined_list) <- thresholds # threshold names
  return(combined_list)
}
# 
# --- f(x) to plot summary of deviance explained by lag and family -------------
plot_de_lag <- function(df, show_legend = FALSE, fill_label) {
  if (!is.data.frame(df))
    stop(
      "'df' must be a dataframe containing DE values at different time lags
         for both error distributions"
    )
  # 
  ggplot(df, aes(
    x = factor(lag, levels = seq(0, 10, 1)),
    y = max,
    fill = state
  )) +
    geom_bar(stat = "identity",
             color = "black",
             position = position_dodge()
             ) +
    # geom_errorbar(aes(ymin  = avg - sd, ymax  = avg + sd),
    #               width = 0.5, size  = 0.5, position = position_dodge(.9)) +
    geom_point(
      aes(x = factor(lag, levels = seq(0, 10, 1)), #y = avg),
          y = ifelse(!is.na(avg) & !is.na(max) & avg != max, avg, NA_real_)
      ),
      position = position_dodge(0.9),
      size = 2,
      shape = 17,
      na.rm = TRUE,
      show.legend = FALSE
    ) +
    geom_text(
      aes(
        label = ifelse (n > 1, n, NA),
        y = max + 0.33  # place the number slightly above the bar
      ),
      position = position_dodge(0.9),
      size = 3,
      color = "black",
      vjust = 0
    ) +
    scale_fill_manual(values = c(
      "white",
      "grey85",
      "grey40",
      "grey23"
    )) +
    coord_cartesian(ylim = c(0, 100)) +
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100),
                       expand = c(0, 0)) +
    # geom_text(aes(x = lag, y = 5,
    #          label= n,
    #          na.rm = T,
    #          group = error),
    #          position = position_dodge(0.9)) +
    theme_tufte() +
    theme(
      axis.title = element_text(face = "bold")
    ) +
    theme(
      legend.title = if (show_legend)
        element_text(size = 12, face = "bold", color = "black") else
          element_blank(),
      legend.position = if (show_legend) c(0.9, 0.8) else "none",
      # legend.direction = "horizontal",
      legend.key.width = unit(1, "lines"),
      legend.key.height = unit(1.5, "lines"),
      legend.text = element_text(size = 12, color = "black"),
      legend.key.size = unit(0.75, "cm")
    ) +
    theme(
      axis.line = element_line(colour = "black"),
      axis.ticks = element_line(colour = "black")
    ) +
    theme(
      axis.text.y = element_text(
        color = "black", size = 12)
    ) +
    theme(
      axis.text.x = element_text(
        color = "black", size = 12)
    ) +
    labs(
      x = "",
      y = "",
      fill = if (show_legend) fill_label else NULL) +
    theme(
      axis.text = element_text(size = 12),
      axis.title.y =  element_blank(),
      axis.title = element_blank()
    )
}
# 
# --- f(x) to customize plot_de_lag --------------------------------------------
plot_de_lag_annotated <- function(stat_data, label_text, tag_text,
                                  show_legend = FALSE, fill_label,
                                  x, y) {
  
  p <- plot_de_lag(stat_data, show_legend = show_legend, fill_label = fill_label) +
    annotate(geom = "text", x = 2, y = 90, label = label_text,
             color = "black", size = 10, family = "Times") +
    labs(tag = tag_text) +
    theme(
      plot.tag.position = c(0.12, 0.975),
      plot.tag = element_text(face = "bold", size = 20)
    )
  
  if (show_legend) {
    p <- p + theme(
      legend.position.inside = c(x, y),
      legend.direction = "vertical",
      legend.key.size = unit(0.75, "cm"),
      legend.text = element_text(size = 12, color = "black"),
      legend.title = element_text(
        size = 14, face = "bold", color = "black",
        margin = margin(b = 5),
        hjust = 1
      )
    )
  } else {
    p <- p + theme(
      legend.direction = "vertical",
      legend.position.inside = c(0.25, 0.8),
      legend.key.size = unit(0.75, "cm"),
      legend.text = element_text(size = 12, color = "black")
    )
  }
  
  return(p)
}
# --- f(x) to assess each variable's contribution in the selected model --------
var_contrib_from_full_gsus <- function(model_full) {
  # Get data
  data <- model.frame(model_full)
  frml <- formula(model_full)
  resp <- all.vars(frml)[1]
  vars <- all.vars(frml)[-1]
  fmly <- model_full$family
  # Deviance Explained by full model
  digits = 2
  DE_total <- round((1 - model_full$deviance / model_full$null.deviance) * 100, digits)
  # --- f(x) to calculate DE of each variable
  de_ev <- function(model_complete, model_reduced) {
    round((model_reduced$deviance - model_complete$deviance) / 
            model_complete$null.deviance * 100, digits)
  }
  # Get DE for reduced models (i.e., removing each variable)
  de_values <- sapply(vars, function(v) {
    formula_red <- reformulate(setdiff(vars, v), response = resp)
    model_red <- glm(formula_red, family = fmly, data = data, na.action = na.omit)
    de_ev(model_full, model_red)
  }, USE.NAMES = TRUE)
  
  # Calculate the relative contribution regarding DE of the full model 
  DE_contribution <- round(de_values / sum(de_values) * DE_total, digits)
  
  # Final result
  result <- data.frame(
    variable = vars,
    DE_contribution = DE_contribution,
    stringsAsFactors = FALSE
  )
  return(result)
  # Return a list with total DE and contributions
  # list(
  #   DE_total = DE_total,
  #   variable_contributions = cc
  # )
}

# --- f(x) to assess each term's contribution in the selected model (handles interactions) --------
var_contrib_from_full <- function(model_full, digits = 2) {
  
  # 1. Model Verification and Total Explained Deviance (DE) Calculation
  if (!inherits(model_full, "glm")) {
    stop("The object 'model_full' must be a GLM fitted with glm().")
  }
  
  # Use getCall to retrieve the original function call and environment,
  # which helps drop1 find necessary information.
  model_call <- getCall(model_full)
  if (is.null(model_call)) {
    stop("The model must have been fitted with the complete 'glm' function for 'drop1' to work correctly.")
  }
  
  # Calculate Total Explained Deviance (DE_total)
  DE_total <- tryCatch({
    round((1 - model_full$deviance / model_full$null.deviance) * 100, digits)
  }, error = function(e) {
    warning("Could not calculate total DE (null.deviance unavailable). Assuming 100% for relative contributions.")
    return(NA)
  })
  
  # 2. Calculate Marginal Contribution of Each Term using drop1()
  # Use 'update' to ensure the 'model_full' environment is appropriate for 'drop1'
  temp_model <- update(model_full, formula(model_full), evaluate = TRUE)
  
  # drop1 performs a Type II analysis of deviance by default for hierarchical GLMs.
  # We use test="none" to only retrieve the deviance change.
  drop_results <- drop1(temp_model, test = "none")
  
  # 3. Term Contribution Calculation
  
  # The first row ('<none>') corresponds to the full model.
  # Reduced model deviances (by dropping one term)
  deviance_reduced <- drop_results$Deviance[-1]
  
  # Difference in deviance (the contribution of the term: Dev_reduced - Dev_full)
  diff_deviance <- deviance_reduced - temp_model$deviance
  
  # Calculate the DE (%) contributed by each term (relative to the Null Deviance)
  # DE_term = (Dev_reduced - Dev_full) / Dev_null * 100
  DE_term_contribution <- round(diff_deviance / temp_model$null.deviance * 100, digits)
  
  # 4. Calculate the Relative Contribution w.r.t. the Total Model DE
  sum_DE_contributions <- sum(DE_term_contribution)
  
  if (abs(sum_DE_contributions) < 1e-6 || is.na(DE_total)) {
    # If the sum is near zero or DE_total is unavailable, return the direct DE change.
    DE_contribution <- DE_term_contribution
    if(is.na(DE_total)) {
      warning("Returning direct DE_change as DE_total could not be calculated for normalization.")
    } else {
      warning("Sum of term DE is ~0. Relative contribution set to 0.")
      DE_contribution <- rep(0, length(DE_term_contribution))
    }
  } else {
    # Relative Contribution: (Term DE / Sum of Term DEs) * Total Model DE
    DE_contribution <- round(DE_term_contribution / sum_DE_contributions * DE_total, digits)
  }
  
  # 5. Final Result
  terms <- rownames(drop_results)[-1]
  
  result <- data.frame(
    term = terms,
    DE_change_pct = DE_term_contribution,        # Percent change in DE from term exclusion
    DE_contribution_relative = DE_contribution,  # Contribution normalized to Total Model DE
    stringsAsFactors = FALSE
  )
  
  return(result)
}

# --- f(x) to assess each term's contribution in the selected model (handles interactions) --------
var_contrib_from_full_gam <- function(model_full, digits = 2) {
  if (!inherits(model_full, "gam")) {
    stop("The object 'model_full' must be a GAM fitted with mgcv::gam().")
  }
  
  # Total Explained Deviance
  DE_total <- tryCatch({
    round((1 - model_full$deviance / model_full$null.deviance) * 100, digits)
  }, error = function(e) {
    warning("Could not calculate total DE (null.deviance unavailable). Assuming 100%.")
    return(NA)
  })
  
  # Try to extract from anova()
  an <- tryCatch(anova(model_full), error = function(e) NULL)
  
  if (!is.null(an) && any(c("Chi.sq", "F") %in% names(an))) {
    # Case 1: anova.gam() produced the right columns
    if ("Chi.sq" %in% names(an)) stat_col <- "Chi.sq" else stat_col <- "F"
    terms <- rownames(an)
    stat_values <- an[[stat_col]]
    stat_values <- stat_values[!is.na(stat_values)]
  } else {
    # Case 2: fall back to summary(model)$s.table
    s_tab <- summary(model_full)$s.table
    if (is.null(s_tab)) stop("Could not extract smooth term table from model.")
    s_tab <- as.data.frame(s_tab)
    terms <- rownames(s_tab)
    if ("Chi.sq" %in% names(s_tab)) {
      stat_col <- "Chi.sq"
    } else if ("F" %in% names(s_tab)) {
      stat_col <- "F"
    } else {
      stop("No Chi.sq or F column found in summary(model)$s.table.")
    }
    stat_values <- s_tab[[stat_col]]
  }
  
  # Compute relative contributions
  stat_values <- stat_values[!is.na(stat_values)]
  DE_term_contribution <- round(stat_values / sum(stat_values, na.rm = TRUE) * DE_total, digits)
  
  result <- data.frame(
    term = terms[!is.na(stat_values)],
    stat = stat_values,
    DE_contribution_relative = DE_term_contribution,
    stringsAsFactors = FALSE
  )
  
  return(result)
}



# --- f(x) to plot observed vs fitted values -----------------------------------
plot_obs_fit <- function(obsfit, model_pal, xp, yp, upperidx = NULL){
  if (!is.list(obsfit))
    stop("'obsfit' must be a list containing the 'appraise' function output")
  if (!is.list(model_pal))
    stop("'model_pal' must be a list containing the 'lm' function output")
  if (!is.double(xp) || !is.double(yp))
    stop("'xp and yp' must be double values")
  if (!is.null(upperidx) && !is.character(upperidx))
    upperidx <- as.character(upperidx)
  # 
  ggplot(as.data.frame(obsfit["data"]),
         aes(x = (obsfit[["data"]][[2]]), y = (obsfit[["data"]])[[1]])) + 
    geom_point() +
    # adjustment line
    stat_smooth(method = "lm", se = F, fullrange = T, color = "black") +
    # perfect agreement line
    geom_line(data = model_pal,
              aes(x = model_pal[["model"]][[2]],
                  y = model_pal[["model"]][[1]]),
              lty = "dotted", lwd = 1, col = "black") +
    # r and p-value 
    annotate(
      geom = "text", x = xp, y = yp, 
      label = paste0("r-pearson = ", r2, ";  p " , pv),
      fontface = "plain", family = "Times", color = "black", size = 5
    ) + 
    theme_tufte() +
    labs(x = "Fitted values",
         y = "Observed values") +
    theme(axis.line = element_line(colour = "black"),
          axis.ticks = element_line(colour="black")) +
    theme(axis.text = element_text(color = "black", size = 12)) +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="plain")) +
    ggtitle(upperidx) +
    theme(plot.title = element_text(size=23, face = "bold",
                                    hjust = 0.01, vjust = -4))
}


get_centroids <- function(cluster_object, years){
  centroids <- ts(t(do.call(rbind, cluster_object@centroids))) # as time series
  n_clusters <- ncol(centroids) # number of clusters
  colnames(centroids) <- as.character(as.roman(
    seq_len(n_clusters))) # row names using Roman numerals
  rownames(centroids) <- years
  return(centroids)
}
get_series <- function(cluster_object, my_colors){
  # Extract cluster assignments and series names
  cluster_assignments <- cluster_object@cluster # cluster values
  series_names <- names(cluster_object@datalist) # names
  series_list <- cluster_object@datalist # data
  # Combine names and clusters
  cluster_df <- data.frame(
    name = series_names,
    cluster = cluster_assignments,
    stringsAsFactors = FALSE
  )
  # Split the time series by cluster number
  series_by_cluster <- split(cluster_df$name, cluster_df$cluster)
  # Create a list of series (values) grouped by cluster
  clustered_series <- lapply(series_by_cluster, function(names_in_cluster) {
    cluster_object@datalist[names_in_cluster]
  })
  # Name the list by cluster number
  names(clustered_series) <- paste0("Cluster_", names(clustered_series))
  # Turn each time series into a data frame with its values and time index
  series_df <- map2_dfr(series_list, series_names, ~ {
    data.frame(
      time = seq_along(.x),
      value = .x,
      series = .y,
      stringsAsFactors = FALSE
    )
  })
  # Add cluster info
  series_df$cluster <- rep(
    cluster_assignments,
    times = sapply(series_list, length)
  )
  # Named color vector (make sure number of colors matches number of series!)
  my_colors_named <- setNames(my_colors[seq_along(series_names)],series_names)
  # unique_series)],
  # unique_series)
  return(list(
    series_df,
    my_colors_named))
}
plot_clusters <- function(series_df, centroids_data, my_colors_named, years) {
  plots <- split(series_df, series_df$cluster) %>%
    imap(~ {
      idx <- as.numeric(.y)
      
      # === Ensure temporal alignment ===
      times <- sort(unique(.x$time)) # Use the time stamps from the series
      centroid_df <- data.frame(
        time = seq_len(nrow(centroids_data)), # times,
        value = centroids_data[, idx],
        series = "Centroid"
      )
      
      # Combine cluster series with centroid
      plot_data <- bind_rows(.x, centroid_df)
      # #~~~~~~ Add helper column for legend
      # plot_data <- plot_data %>%
      #   mutate(series_legend = ifelse(series == "Centroid" & idx == 1, "Centroid", series))
      # #~~~~~~ End helper column
      # === Layout parameters ===
      n_clusters <- length(unique(series_df$cluster))
      ncol_plot <- 2
      row_pos <- ceiling(idx / ncol_plot)
      max_row <- ceiling(n_clusters / ncol_plot)
      col_pos <- idx %% ncol_plot
      if (col_pos == 0) col_pos <- ncol_plot
      x_lab <- if (row_pos == max_row) "Time" else NULL
      y_lab <- if (col_pos == 1) "Standardized value (Z-score)" else NULL
      
      legend_pos <- dplyr::case_when(
        row_pos == 1 & col_pos == 1 ~ c(0.5, 0.92), # Top left
        row_pos >  1 & col_pos == 1 ~ c(0.5, 0.92), # Bottom left
        row_pos == 1 & col_pos == 2 ~ c(0.5, 0.92), # Top right
        row_pos >  1 & col_pos == 2 ~ c(0.50, 0.92), # Bottom right
        TRUE ~ c(0.5, 0.5)
      )
      
      # === Plot ===
      p <- ggplot(plot_data, aes(
        x = time, 
        y = value, 
        color = series, 
        group = series
      )) +
        # series
        geom_line(data = dplyr::filter(
          plot_data, series != "Centroid"),
          linetype = "solid", size = 1) +
        # centroid
        geom_line(data = dplyr::filter(
          plot_data, series == "Centroid"),
          aes(x = time, y = value),
          color = "black",
          linetype = "dotted",
          linewidth = 1.5,
          inherit.aes = FALSE) +
        # 
        scale_color_manual(values = my_colors_named) +
        scale_x_continuous(
          breaks = seq_along(years), # all ticks
          labels = ifelse(
            seq_along(
              years) %% 3 == 1, # label every odd index
            years, "")
        ) +
        scale_y_continuous(limits = c(-2, 4.55)) +
        labs(title = paste("Cluster", idx), x = x_lab, y = y_lab) +
        theme_tufte() +
        theme(
          axis.title = element_text(face = "bold"),
          legend.position = legend_pos,
          legend.title = element_blank(),
          legend.key.size = unit(0.4, "cm"),
          legend.text = element_text(face = "italic", size = 12),
          legend.box = "vertical",
          plot.title = element_text(face = "bold"),
          axis.line.x = element_line(colour = "black"),
          axis.line.y = element_line(colour = "black"),
          axis.ticks.x = element_line(colour = "black"),
          axis.ticks.y = element_line(colour = "black"),
          axis.ticks.length = unit(3, "pt"),
          axis.text.y = element_text(color = "black", size = 12),
          axis.text.x = element_text(color = "black", size = 12),
          axis.text = element_text(size = 12)
        ) +
        guides(color = guide_legend(ncol = 4, nrow = 3, byrow = TRUE))
      p
    })
  return(plots)
}

# Color palette ----------------------------------------------------------------
my_colors <- c(
  "Pomatomus saltatrix"       = "#E69F00",  # Orange (Perciformes)
  "Squatina guggenheim"       = "#CD5555",  # Black (Shark)
  "Urophycis brasiliensis"    = "#1F9E89",  # Teal (Gadiformes)
  "Illex argentinus"          = "#CC79A7",  # Reddish Purple (Cephalopoda)
  "Micropogonias furnieri"    = "#56B4E9",  # Sky Blue (Sciaenidae)
  "Pogonias cromis"           = "#0072B2",  # Strong Blue (Sciaenidae)
  "Mustelus schmitti"         = "#B22222",  # Mustard (Shark)
  "Chelon dumerili"           = "#66C2A5",  # Greenish Cyan (Perciformes)
  "Merluccius hubbsi"         = "#73D055",  # Green (Gadiformes)
  "Macruronus magellanicus"   = "#1B7837",  # Dark Green (Gadiformes)
  "Parona signata"            = "#D95F02",  # Vermillion (Perciformes)
  "Umbrina canosai"           = "#000066",  # Light Blue (Sciaenidae)
  "Cynoscion guatucupa"       = "#104E8B",  # Grey (Sciaenidae)
  "Macrodon atricauda"        = "#B3CDE3",  # Pale Blue (Sciaenidae)
  "Helicolenus dactylopterus" = "#A6761D"   # Brown (Sebastidae)
)

# 
## Fig. 1 - MAP ####
map_aucfz <- function(df, plygn = NULL, scale = NULL, clabs = NULL, ...){
  if (!is.data.frame(df))
    stop("'df' must be a dataframe containing longitude, latitude and the
         elevation/depth values")
  if (is.null(plygn)) {
    ppolygon <- geom_polygon()
  } else {
    if (!is.data.frame(plygn)) {
      stop("'polygon' must be a dataframe containing longitude and latitude
           values")
    } else {
      ppolygon <- geom_polygon(data = plygn, aes(x = plygn[,1], y = plygn[,2]),
                              colour = "orange", fill = NA, 
                              linetype = 1, linewidth = 0.5)
    }
  }
  if (is.null(scale) || scale == "discrete") {
    if (is.null(clabs)) {
      stop("Default 'scale_fill' is discrete, so 'clabs' must be defined as a
    character vector that contains depth scale values. If you want to draw the
    figure using a continuous scale, please choose 'scale = continuous'")
    } else {
    # Discrete scale
    clabs <- clabs
    depth <- df[,4]
    scl <- scale_fill_manual(values =
                             rev(c(blues9[2:length(unique(df[,4]))])),
                             na.value =  "gray75",
                             limits = clabs, labels = rev(clabs),
                             guide = guide_legend(reverse = TRUE))
    }
  } else if (!is.null(scale) & scale == "continuous") {
    if(!is.null(clabs)){
      warning("omiting unused argument 'clabs'")
    } else {
        }
    # Continuous scale
    depth <- df[,3]
     scl <- scale_fill_viridis_c(option = 'mako', direction = 1,
                                limits = c(min(df[,3]), 0))
    #scl <- scale_fill_distiller(palette="Blues", na.value="gray75",
    #                            limits = c(min(df[,3]), 0))
    } else if (!is.null(scale) || scale != "discrete" || scale != "continuous"){
      stop("The argument scale must be 'discrete' or 'continuous'")
    }
  # Nested function of the ggsn package modified to use "Times" as font family
  rulermap <- function(data = NULL, location = "bottomright", dist = NULL,
                       dd2km = NULL, model = NULL, height = 0.02, 
                       st.dist = 0.02, dist_unit = "km", st.bottom = TRUE,
                       st.size = 5, st.color = "black", st.family = "Times",
                       box.fill = c("black", "white"), 
                       box.color = "black", border.size = 1, 
                       x.min = NULL, x.max = NULL, y.min = NULL, y.max = NULL,
                       anchor = NULL, facet.var = NULL, facet.lev = NULL, ...){
    if (is.null(data)) {
      if (is.null(x.min) | is.null(x.max) |
          is.null(y.min) | is.null(y.max) ) {
        stop('If data is not defined, x.min, x.max, y.min and y.max must be.')
      }
      data <- data.frame(long = c(x.min, x.max), lat = c(y.min, y.max))
    }
    if (is.null(dd2km)) {
      stop("dd2km should be logical.")
    }
    if (any(class(data) %in% "sf")) {
      xmin <- sf::st_bbox(data)["xmin"]
      xmax <- sf::st_bbox(data)["xmax"]
      ymin <- sf::st_bbox(data)["ymin"]
      ymax <- sf::st_bbox(data)["ymax"]
    } else {
      xmin <- min(data$long)
      xmax <- max(data$long)
      ymin <- min(data$lat)
      ymax <- max(data$lat)
    }
    if (location == 'bottomleft') {
      if (is.null(anchor)) {
        x <- xmin
        y <- ymin
      } else {
        x <- as.numeric(anchor['x'])
        y <- as.numeric(anchor['y'])
      }
      direction <- 1
      
    }
    if (location == 'bottomright') {
      if (is.null(anchor)) {
        x <- xmax
        y <- ymin
      } else {
        x <- as.numeric(anchor['x'])
        y <- as.numeric(anchor['y'])
      }
      direction <- -1
      
    }
    if (location == 'topleft') {
      if (is.null(anchor)) {
        x <- xmin
        y <- ymax
      } else {
        x <- as.numeric(anchor['x'])
        y <- as.numeric(anchor['y'])
      }
      direction <- 1
      
    }
    if (location == 'topright') {
      if (is.null(anchor)) {
        x <- xmax
        y <- ymax
      } else {
        x <- as.numeric(anchor['x'])
        y <- as.numeric(anchor['y'])
      }
      direction <- -1
      
    }
    if (!st.bottom) {
      st.dist <-
        y + (ymax - ymin) * (height + st.dist)
    } else {
      st.dist <- y - (ymax - ymin) * st.dist
    }
    height <- y + (ymax - ymin) * height
    
    if (dd2km) {
      if (dist_unit == "m") {
        dist <- dist / 1e3
      }
      break1 <- maptools::gcDestination(lon = x, lat = y,
                                        bearing = 90 * direction,
                                        dist = dist, dist.units = 'km',
                                        model = model)[1, 1]
      break2 <- maptools::gcDestination(lon = x, lat = y,
                                        bearing = 90 * direction,
                                        dist = dist*2, dist.units = 'km',
                                        model = model)[1, 1]
    } else {
      if (location == 'bottomleft' | location == 'topleft') {
        break1 <- x + dist * 1e3
        break2 <- x + dist * 2e3
      } else {
        break1 <- x - dist * 1e3
        break2 <- x - dist * 2e3
      }
      
    }
    box1 <- data.frame(x = c(x, x, rep(break1, 2), x),
                       y = c(y, height, height, y, y), group = 1)
    box2 <- data.frame(x = c(rep(break1, 2), rep(break2, 2), break1),
                       y=c(y, rep(height, 2), y, y), group = 1)
    if (!is.null(facet.var) & !is.null(facet.lev)) {
      for (i in 1:length(facet.var)){
        if (any(class(data) == "sf")) {
          if (!is.factor(data[ , facet.var[i]][[1]])) {
            data[ , facet.var[i]] <- factor(data[ , facet.var[i]][[1]])
          }
          box1[ , facet.var[i]] <- factor(facet.lev[i],
                                          levels(data[ , facet.var[i]][[1]]))
          box2[ , facet.var[i]] <- factor(facet.lev[i],
                                          levels(data[ , facet.var[i]][[1]]))
        } else {
          if (!is.factor(data[ , facet.var[i]])) {
            data[ , facet.var[i]] <- factor(data[ , facet.var[i]])
          }
          box1[ , facet.var[i]] <- factor(facet.lev[i],
                                          levels(data[ , facet.var[i]]))
          box2[ , facet.var[i]] <- factor(facet.lev[i],
                                          levels(data[ , facet.var[i]]))
        }
        
      }
    }
    if (dist_unit == "km") {
      legend <- cbind(text = c(0, dist, dist * 2), row.names = NULL)
    }
    if (dist_unit == "m") {
      legend <- cbind(text = c(0, dist * 1e3, dist * 2e3), row.names = NULL)
    }
    
    gg.box1 <- geom_polygon(data = box1, aes(x, y),
                            fill = utils::tail(box.fill, 1),
                            color = utils::tail(box.color, 1),
                            size = border.size)
    gg.box2 <- geom_polygon(data = box2, aes(x, y), fill = box.fill[1],
                            color = box.color[1],
                            size = border.size)
    x.st.pos <- c(box1[c(1, 3), 1], box2[3, 1])
    if (location == 'bottomright' | location == 'topright') {
      x.st.pos <- rev(x.st.pos)
    }
    if (dist_unit == "km") {
      legend2 <- cbind(data[1:3, ], x = unname(x.st.pos), y = unname(st.dist),
                       label = paste0(legend[, "text"], c("", "", "km")))
    }
    if (dist_unit == "m") {
      legend2 <- cbind(data[1:3, ], x = unname(x.st.pos), y = unname(st.dist),
                       label = paste0(legend[, "text"], c("", "", "m")))
    }
    if (!is.null(facet.var) & !is.null(facet.lev)) {
      for (i in 1:length(facet.var)){
        if (any(class(data) == "sf")) {
          legend2[ , facet.var[i]] <- factor(facet.lev[i],
                                             levels(data[ , facet.var[i]][[1]]))
        } else {
          legend2[ , facet.var[i]] <- factor(facet.lev[i],
                                             levels(data[ , facet.var[i]]))
        }
      }
    } else if (!is.null(facet.var) & is.null(facet.lev)) {
      facet.levels0 <- unique(as.data.frame(data)[, facet.var])
      facet.levels <- unlist(unique(as.data.frame(data)[, facet.var]))
      legend2 <- do.call("rbind", replicate(length(facet.levels),
                                            legend2, simplify = FALSE))
      if (length(facet.var) > 1) {
        facet.levels0 <- expand.grid(facet.levels0)
        legend2[, facet.var] <-
          facet.levels0[rep(row.names(facet.levels0), each = 3), ]
      } else {
        legend2[, facet.var] <- rep(facet.levels0, each = 3)
      }
    }
    gg.legend <- geom_text(data = legend2, aes(x, y, label = label),
                           size = st.size, color = st.color,
                           family = st.family, ...)
    return(list(gg.box1, gg.box2, gg.legend))
  }
  # Coordinate numbers to plot on x- and y-axes
  xnum <- rev(seq(round(-max(df[,1])),
                   round(-min(df[,1])), by = 2))
  ynum <- rev(seq(round_any(-max(df[,2]), 1, ceiling),
                   round_any(-min(df[,2]), 1, ceiling), by = 2))
  ggplot() +
    ## Bathymetry
    geom_raster(aes(df[,1], df[,2], fill = depth), data = df) +
    coord_equal(clip = "off") +
    scl +
    labs(fill = "Depth (m)") +
    ## Polygon
    ppolygon +
    ## Map ruler
    rulermap(x.min = -59.2, x.max = -59.2, y.min = -33.6, y.max = -33.7,
             location = "topleft", dist = 50, dist_unit = "km",
             dd2km = TRUE, model = "WGS84", 
             height = 0.7, st.dist = 0.9, st.size = 3.5, st.family = "Times",
             box.fill = c("black", "white"),
             box.color = "black", border.size = 0.5) +
    ## North arrow
    annotation_north_arrow(location = "tl",
                           pad_x = unit(0.2, "cm"),
                           pad_y = unit(0.5, "cm"),
                           style = north_arrow_fancy_orienteering) +
    ## Country names
    annotate(
      geom = "text", x = -58.7, y = -36.2, label = "Argentina", 
      fontface = "plain", family = "Times", color = "grey22", size = 6) +
    annotate(
      geom = "text", x = -56, y = -33.8, label = "Uruguay", 
      fontface = "plain", family = "Times", color = "grey22", size = 6) +
    annotate(
      geom = "text", x = -53.2, y = -33.3, label = "Brazil", 
      fontface = "plain", family = "Times", color = "grey22", size = 3) +
    ## Rio de la Plata name
    annotate(
      geom = "text", x = -57, y = -35.1, label = "Rio de la Plata", 
      fontface = "plain", family = "Times", color = "grey22", size = 4.5,
      angle = -40) +
       ## Punta del Este name
    annotate(
      geom = "text", x = -54.95, y = -34.75, label = "PE", 
      fontface = "plain", family = "Times", color = "grey22", size = 4.5,
      angle = 0) +
    ## Punta Rasa del Cabo San Antonio Plata name
    annotate(
      geom = "text", x = -56.9, y = -36.5, label = "PR", 
      fontface = "plain", family = "Times", color = "grey22", size = 4.5,
      angle = 0) +
    ## Axes and theme
    scale_y_continuous(expand=c(0,0),
                       sec.axis = dup_axis(), # right axis labels
                       breaks = -ynum,
                       labels = to_vec(for(i in 1:length(ynum))
                         (paste0(ynum[i], "\u00B0S")))) +
    scale_x_continuous(expand=c(0,0),
                       sec.axis = dup_axis(), # top axis labels
                       breaks = -xnum,
                       labels = to_vec(for(i in 1:length(xnum))
                         (paste0(xnum[i], "\u00B0W")))) +
    theme_bw()+
    theme(
      panel.grid.major = element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text = element_text(size = 12),
      axis.text.x = element_text(vjust = -0.5, color = "black"),
      axis.text.y = element_text(hjust = -0.5, color = "black"),
      axis.text.x.top = element_blank(), # do not show top / right axis labels
      axis.text.y.right = element_blank(),
      axis.ticks.length = unit(-2, "mm"),
      legend.text = element_text(size = 12),
      legend.key = element_rect(fill = "white", colour = "transparent"),
      legend.position = c(.983, .33),
      legend.justification = c("right", "top"),
      legend.box.just = "right",
      legend.margin = margin(5, 5, 5, 5)) +
    theme(
      text = element_text(family = "Times"))
  }

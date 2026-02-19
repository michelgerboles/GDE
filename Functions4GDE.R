############## Functions for the GDE App ##############

#' Claudia 2025-11-19: Function to compute basic statistics - FOR NOW NOT USED ---------
#' @param df (mandatory) dataframe or data.table of data. it shall include columns: RM1 and CM1 for Type "Fixed on going" and in addition RM2 and CM2 for "Fixed Type Testing"
#' @param Pollutant (mandatory) character vector (string), pollutant name as defined in function get.DQO()
calculate_basic_stats <- function(df, Pollutant) {
  
  stopifnot(all(c("RM1", "RM2") %in% df))
  
  # Converting df to data.table
  
  
  # Uncertainty calculations
  ubsRM <- round(sqrt(sum((df$RM1 - df$RM2)^2) / (2 * nrow(df))), 2)
  ubsCM <- round(sqrt(sum((df$CM1 - df$CM2)^2) / (2 * nrow(df))), 2)
  
  # Limit value exceedance
  LV <- ifelse(Pollutant == "PM2.5", 25, ifelse(Pollutant == "PM10", 45, NA))
  n_up_LV <- sum(df$RM > LV)
  ratio <- round((n_up_LV / nrow(df)) * 100, 0)
  
  # RM statistics
  MBE_RM <- round(mean(df$RM1 - df$RM2), 2)
  sd_diff_RM <- round(sd(df$RM1 - df$RM2), 2)
  SE_diff_RM <- round(sd_diff_RM / sqrt(nrow(df)), 2)
  CI_RM <- t.test(df$RM1 - df$RM2, conf.level = 0.95)
  CI_lower_RM <- round(CI_RM$conf.int[1], 2)
  CI_upper_RM <- round(CI_RM$conf.int[2], 2)
  skewdiffRM <- round(moments::skewness(df$RM1 - df$RM2), 1)
  kurtRM <- round(moments::kurtosis(df$RM1 - df$RM2), 0)
  
  
  # CM1 statistics
  diff_CM1 <- df$CM1 - df$RM
  MBE_CM1 <- round(mean(diff_CM1), 2)
  sd_diff_CM1 <- round(sd(diff_CM1), 2)
  SE_diff_CM1 <- round(sd_diff_CM1 / sqrt(nrow(df)), 2)
  CI_CM1 <- t.test(diff_CM1, conf.level = 0.95)
  CI_lower_CM1 <- round(CI_CM1$conf.int[1], 2)
  CI_upper_CM1 <- round(CI_CM1$conf.int[2], 2)
  skewCM1 <- round(moments::skewness(diff_CM1), 1)
  kurtCM1 <- round(moments::kurtosis(diff_CM1), 0)
  
  return(list(
    ubsRM = ubsRM, ubsCM = ubsCM, LV = LV, n_up_LV = n_up_LV, ratio = ratio,
    MBE_RM = MBE_RM, sd_diff_RM = sd_diff_RM, SE_diff_RM = SE_diff_RM,
    CI_lower_RM = CI_lower_RM, CI_upper_RM = CI_upper_RM,
    skewdiffRM = skewdiffRM, kurtRM = kurtRM,
    MBE_CM1 = MBE_CM1, sd_diff_CM1 = sd_diff_CM1, SE_diff_CM1 = SE_diff_CM1,
    CI_lower_CM1 = CI_lower_CM1, CI_upper_CM1 = CI_upper_CM1,
    skewCM1 = skewCM1, kurtCM1 = kurtCM1, diff_CM1 = diff_CM1
  ))
}

# maybe it will be worth to separate the function for filtering, computing and plotting. For now not used
#' @param Pollutant (mandatory) character vector (string), pollutant name as defined in function get.DQO() - FOR NOW NOT USED ---------
filter_data <- function(data, Instrument, Pollutant) {
  
  # Verifica che la granulometria esista per questo strumento
  available_data <- data %>%
    filter(Instrument == Instrument, `Size Fraction` == Pollutant)
  
  if (nrow(available_data) == 0) {
    message(paste("  ✗ Nessun dato per", Instrument, Pollutant))
    return(available_data) # Restituisce dataframe vuoto
  }
  
  data_filtered <- available_data %>%
    filter(!is.na(RM1), !is.na(RM2), !is.na(CM1), !is.na(CM2)) %>%
    mutate(RM = rowMeans(cbind(RM1, RM2)))
  
  # Pulizia dati reference - outliers |RM1-RM2|>2
  data_filtered <- data_filtered %>%
    filter(abs(RM1 - RM2) < 2)
  
  message(paste("  ✓ Dati filtrati:", nrow(data_filtered), "righe per", Instrument, Pollutant))
  
  return(data_filtered)
}

#, Creating reference plots function ##########
#' @description
#' Verification of suitability of the reference measurements for the Fixed type testing
#' @param df (mandatory) dataframe or data.table with  measurement data. It shall include columns: RM1 and CM1 for Type "Fixed on going" and in addition RM2 and CM2 for "Fixed Type Testing"
#' @param Type (mandatory) character vector (string), can be "Fixed on going", "Fixed Type Testing" or "Indicative" as given by a PickerInput in GDE_3.R
#' @param Max.Ref.Bias (optional) numeric value, default is 2. Maximum allowed bias between 2 reference instrument for each row date
#' @param Unit (mandatory) character vector, unit of measurements
#' @param Name.CM (optional) vector of character vectors, default is NULL If not NULL: names of column of df with data for candidate analysers 
#' @param Name.RM (optional) vector of character vector, default is NULL If not NULL: names of column of df with data for reference analysers 
#' @param i.Ref.outliers (mandatory) numeric vector. Index of row for which Name.RM are flagged as outliers 
#' @param min.N (optional) numeric, default is 100 Minimum nuber of rows to carry out the data treatment
#' @return la list with the following elements:
#' - 4 ggplots: ref_time_plot, ref_hist_plot, diff_time_plot and diff_hist_plot,
#' - 1 grid.arrange with the 4 previous plots
#' - ubs_rm the as computed with the selected pairs of reference measurements, 2 digits, the square root of the square of residuals divided by 2 x number of pairs
#' - mean_diff, the mean of differences between reference measurements, 2 digits
#' - i.Ref.outliers, the row indexes of the difference between reference measurements exceeding  Max.Ref.Bias
#' @examples plots <- create_reference_plots(df_Fidas200_PM25)
create_reference_plots <- function(df, Type, Max.Ref.Bias = 2, Unit, Name.CM = NULL, Name.RM = NULL, i.Ref.outliers, min.N = 100) {
  
  # Convert to data.table for easier computation
  if(!data.table::is.data.table(df)) df <- data.table::as.data.table(df)
  
  Instrument <- unique(df$Instrument)
  Pollutant  <- unique(df$Pollutant)
  # # The following test is not necessary since Instrument and Pollutant come from the PickerInput
  # if (length(Instrument) != 1 || length(Pollutant) != 1) {
  #   stop("Data frame must contain a single Instrument and Pollutant.")
  # }
  
  # checking availability of data
  # According to the pickerInput "Type" can only be either "Fixed on going", "Fixed type testing" or "Indicative"
  Columns <- c(Name.CM, Name.RM)
  
  ############################################# Manage Alert #################################################C
  # Checking availability of complete data
  # df has been already checked for non NA row. Inutile, remove
  df <- df[is.finite(rowSums(df[,.SD,.SDcols = Columns]))]
  stopifnot(nrow(df) > min.N)
  ############################################################################################################C
  
  if(Type == "Fixed type testing"){
    # Prepare plots with ggplot2, only using complete data set for RM1 and RM2
    
    # Normally RM and RM_DELTA is already present but make it sure by testing and calculating
    if(!"RM" %in% names(df))   df[, RM   := apply(df[,.SD,.SDcols=Name.RM], MARGIN = 1, mean)]
    if(!"RM_DELTA" %in% names(df)) df[, RM_DELTA := apply(df[,.SD,.SDcols=Name.RM], MARGIN = 1, function(RMs) RMs[1] - RMs[2])]
    mean_diff <- round(mean(df$RM_DELTA, na.rm = T), 2)
    ubs_rm    <- round(sqrt(sum((df$RM_DELTA)^2, na.rm = T) / (2 * length(df$RM_DELTA[!is.na(df$RM_DELTA)]))), 2)
    ref_time_plot <-
      ggplot2::ggplot(df, aes(x = RM, y = RM_DELTA)) +
      ggplot2::geom_point(alpha = 0.6) +
      ggplot2::geom_hline(yintercept = mean_diff, color = "red", linewidth = 1) +
      ggplot2::geom_hline(yintercept = 0, color = "blue", linetype = "dashed", linewidth = 0.8) +
      ggplot2::geom_hline(yintercept = c(-Max.Ref.Bias, Max.Ref.Bias), color = "orange", linetype = "dotted", linewidth = 0.5) +
      ggplot2::labs(
        title = paste("Bland-Altman: RM1 vs RM2 -", Instrument, Pollutant),
        x = paste0("Average Concentration ((RM1 + RM2)/2, ", Unit, ")"),
        y = paste0("Difference (RM1 - RM2, ", Unit, ")"),
        subtitle = paste("Mean bias =", mean_diff, Unit, "| ubsRM =", ubs_rm, " ", Unit)
      ) +
      theme_minimal()
    
    ref_hist_plot <- 
      ggplot2::ggplot(df, aes(x = RM)) +
      ggplot2::geom_histogram(fill = "lightblue", color = "black", bins = 20) +
      ggplot2::labs(
        title = paste("Distribution of Ref. concentration -", Instrument, Pollutant),
        x = paste0("Means of Reference measurements in ", Unit), y = "Counts",
        subtitle = paste("n =", nrow(df)) # Aggiungi come sottotitolo
      ) +
      theme_minimal() +
      ggplot2::theme(
        plot.subtitle = element_text(
          color = ifelse(nrow(df) < min.N, "red", "darkgreen"),
          face = "bold"))
    
    # # Detect index of Ref.Outliers and Add a color column, already done in GDE, not necessary
    # df <- df %>% mutate(outlier = ifelse(abs(RM_DELTA) > Max.Ref.Bias, paste0("> ", Max.Ref.Bias), paste0("≤ ", Max.Ref.Bias)))
    # i.Ref.outliers <- df[outlier == paste0("> ", Max.Ref.Bias), which = TRUE]
    
    # Outliers
    diff_time_plot <- 
      ggplot2::ggplot(df[setdiff(1:.N, i.Ref.outliers)], ggplot2::aes(x = date, y = RM_DELTA)) +
      ggplot2::geom_point(color = "darkgreen", size = 1.5)
    if(length(i.Ref.outliers) > 0){
      diff_time_plot <- diff_time_plot +
        ggplot2::geom_point(data = df[i.Ref.outliers], color = "red")
    }
    diff_time_plot <- diff_time_plot +
      ggplot2::geom_hline(yintercept = 0, color = "red", linewidth = 1) +
      ggplot2::geom_hline(yintercept = c(-Max.Ref.Bias, Max.Ref.Bias), color = "orange", linetype = "dashed", linewidth = 0.5) +
      ggplot2::ylim(-6, 6) +
      ggplot2::labs(
        title = paste("(RM1 - RM2) over time -", Instrument, Pollutant),
        x = "Date",
        y = paste0("RM1 - RM2 in ", Unit)) +
      ggplot2::annotate("text",
                        x = min(df$date), y = 5.5,
                        label = paste(
                          "Points > |", Max.Ref.Bias, "|: ", length(which(df$outlier == paste0("> ", Max.Ref.Bias))),
                          "/", nrow(df),
                          " (", round(length(which(df$outlier == paste0("> ", Max.Ref.Bias))) / nrow(df) * 100, digit = 0), "%)"),
                        hjust = 0, vjust = 1,
                        color = "red", size = 4, fontface = "bold") +
      theme_minimal() +
      ggplot2::theme(legend.position = "top")
    
    diff_hist_plot <- 
      ggplot2::ggplot(df, aes(x = RM_DELTA)) +
      ggplot2::geom_histogram(fill = "lightgreen", color = "black", bins = 20) +
      ggplot2::xlim(-6, 6) +
      ggplot2::labs(
        title = paste("(RM1 - RM2) distribution -", Instrument, Pollutant),
        x = paste0("RM1 - RM2 in ",Unit), y = "Counts") +
      # Unica annotazione
      # Media con colore in base al valore (verde se >= 0.5, verde se < 0.5 - come richiesto)
      ggplot2::annotate("text",
                        x = -Inf, y = Inf,
                        label = paste("Mean =", mean_diff, " ", Unit),
                        hjust = -0.1, vjust = 1.5,
                        color = ifelse(mean_diff >= 0.5, "red", "darkgreen"),
                        size = 3.5, fontface = "bold") +
      # ubsRM con colore in base al valore (rosso se >=1, verde se <1)
      ggplot2::annotate("text",
                        x = -Inf, y = Inf,
                        label = paste0("ubsRM =", ubs_rm, " ", Unit, " (outliers not discarded)"),
                        hjust = -0.1, vjust = 3,
                        color = ifelse(ubs_rm >= 1, "red", "darkgreen"),
                        size = 3.5, fontface = "bold") +
      # Linee verticali
      ggplot2::geom_vline(xintercept = mean_diff, color = "black", linetype = "solid", linewidth = 1) +
      theme_minimal()
    
    
    # Combina i plot
    combined_plot <- gridExtra::grid.arrange(ref_time_plot, ref_hist_plot, diff_time_plot, diff_hist_plot, ncol = 2, nrow = 2)
    
    return(list(
      ref_time_plot  = ref_time_plot,
      ref_hist_plot  = ref_hist_plot,
      diff_time_plot = diff_time_plot,
      diff_hist_plot = diff_hist_plot,
      combined_plot  = combined_plot,
      ubs_rm         = ubs_rm,
      mean_diff      = mean_diff #,
      # i.Ref.outliers = i.Ref.outliers
    ))
  } # Add plots in case of Indicative and On-going test
}


# function to compute average and Ubs by Lag
DT_Weighing <- function(DT, name.date, name.V1, name.V2, Lag, outliers = FALSE, Verbose = TRUE){
  
  # checking names in DT
  stopifnot(all(c(name.date, name.V1, name.V2) %in% names(DT)))
  
  # Setting type to date and numeric
  if(!lubridate::is.Date(DT[[name.date]]) && !lubridate::is.POSIXct(DT[[name.date]])) data.table::set(DT, j = name.date, value = as.Date(DT[[name.date]]))
  for(Var in c(name.V1,name.V2)) if(!is.numeric(DT[[Var]])) data.table::set(DT, j = Var, value = as.numeric(DT[[Var]]))
  rm(Var)
  
  # Copy not to change DT
  DT.weighted <- data.table::copy(DT)
  DT.weighted <- DT.weighted[is.finite(rowSums(DT.weighted[,.SD,.SDcols=c(name.V1, name.V2)]))]
  
  # Adding Deltas(square differences) and means
  DT.weighted[, Delta2 := (DT.weighted[[name.V1]] - DT.weighted[[name.V2]])^2]
  DT.weighted[, Mean  := rowMeans(DT.weighted[,.SD,.SDcols = c(name.V1, name.V2)])]
  
  # Check of outliers
  # https://www.r-bloggers.com/2021/09/how-to-identify-outliers-grubbs-test-in-r/
  # H0: There is no outlier in the data.
  # H1: There is an outlier in the data.
  if(outliers){
    # Maximum value
    max.Grub <- outliers::grubbs.test(sqrt(DT.weighted[["Delta2"]]))
    if(max.Grub$p.value < 0.05){
      if(Verbose) futile.logger::flog.info(paste0("Applying the Grub's test, the maximum difference between ", name.V1, " and ", name.V2,
                                                  " is found to be an outlier (H1: p = ", sprintf("%.2f", max.Grub$p.value),"). The outlier is discarded."))
      DT.weighted <- DT.weighted[- which.max(DT.weighted[["Delta2"]])]
    } else {
      if(Verbose) futile.logger::flog.info(paste0("Applying the Grub's test, the null hypothesis (H0: no outlier) about the maximum difference between ", name.V1, " and ", name.V2,
                                                  " cannot be rejected (p = ", sprintf("%.2f", max.Grub$p.value),")"))}
    
    # Minimum value
    min.Grub <- outliers::grubbs.test(sqrt(DT.weighted[["Delta2"]]), opposite=TRUE)
    if(min.Grub$p.value < 0.05){
      if(Verbose) futile.logger::flog.info(paste0("Applying the Grub's test, the minimum difference between ", name.V1, " and ", name.V2,
                                                  " is found to be an outlier (H1: p = ", sprintf("%.2f", min.Grub$p.value),"). The outlier is discarded."))
      DT.weighted <- DT.weighted[- which.min(DT.weighted[["Delta2"]])]
    } else {
      if(Verbose) futile.logger::flog.info(paste0("Applying the Grub's test, the null hypothesis (H0: no outlier) about the minimum difference between ", name.V1, " and ", name.V2,
                                                  " cannot be rejected (p = ", sprintf("%.2f", min.Grub$p.value),")"))}
    
    if(length(DT.weighted[["Delta2"]]) < 30){
      # two large values outlier or not
      both.max.Grub <- outliers::grubbs.test(sqrt(DT.weighted[["Delta2"]]), type=20L)
      if(both.max.Grub$p.value < 0.05){
        if(Verbose) futile.logger::flog.info(paste0("Applying the Grub's test, the 2 maximum differences between ", name.V1, " and ", name.V2,
                                                    " are found to be outliers (H1: p = ", sprintf("%.2f", both.max.Grub$p.value),"). The 2 outliers are discarded."))
        DT.weighted <- DT.weighted[-which( sqrt(DT.weighted[["Delta2"]]) %in% sort(sqrt(DT.weighted[["Delta2"]]), decreasing = T)[1:2])[1:2]]
      } else {
        if(Verbose) futile.logger::flog.info(paste0("Applying the Grub's test, the null hypothesis (H0: no outliers) about the 2 maximum differences between ", name.V1, " and ", name.V2,
                                                    " cannot be rejected (p = ", sprintf("%.2f", both.max.Grub$p.value),")"))}
      
      # two smallest values outlier or not
      both.min.Grub <- outliers::grubbs.test(sqrt(DT.weighted[["Delta2"]]), type=20L,opposite=TRUE)
      if(both.min.Grub$p.value < 0.05){
        if(Verbose) futile.logger::flog.info(paste0("Applying the Grub's test, the 2 minimum differences between ", name.V1, " and ", name.V2,
                                                    " are found to be outliers (H1: p = ", sprintf("%.2f", both.min.Grub$p.value),"). The 2 outliers are discarded."))
        DT.weighted <- DT.weighted[-which( sqrt(DT.weighted[["Delta2"]]) %in% sort(sqrt(DT.weighted[["Delta2"]]), decreasing = F)[1:2])[1:2]]
      } else {
        if(Verbose) futile.logger::flog.info(paste0("Applying the Grub's test, the null hypothesis (H0: no outliers) about the 2 minimum differences between ", name.V1, " and ", name.V2,
                                                    " cannot be rejected (p = ", sprintf("%.2f", both.min.Grub$p.value),")"))}
    }
    
    #smallest and hightest values are outlier or not 
    min.max.Grub <- outliers::grubbs.test(sqrt(DT.weighted[["Delta2"]]), type=11L)
    if(min.max.Grub$p.value < 0.05){
      if(Verbose) futile.logger::flog.info(paste0("Applying the Grub's test, the mimimum and maximum differences between ", name.V1, " and ", name.V2,
                                                  " are found to be outliers (H1: p = ", sprintf("%.2f", min.max.Grub$p.value),"). The 2 outliers are discarded."))
      DT.weighted <- DT.weighted[-which( sqrt(DT.weighted[["Delta2"]]) %in% c(sort(sqrt(DT.weighted[["Delta2"]]), decreasing = T)[1],
                                                                              sort(sqrt(DT.weighted[["Delta2"]]), decreasing = F)[1]))[1:2]]
    } else {
      if(Verbose) futile.logger::flog.info(paste0("Applying the Grub's test, the null hypothesis (H0: no outliers) about the mimimum and maximum differences between ", name.V1, " and ", name.V2,
                                                  " cannot be rejected (p = ", sprintf("%.2f", min.max.Grub$p.value),")"))}
    
  }
  
  
  # Class of concentration in 5 ug/m3, ceiling
  DT.weighted[, Class         := DT.weighted$Mean %/% Lag * Lag]
  DT.weighted[, Mean.Class    := lapply(.SD, mean, na.rm = T), .SDcols = "Mean", by = Class]
  DT.weighted[, s_Mean        := lapply(.SD, sd, na.rm = T), .SDcols = "Mean", by = Class]
  DT.weighted[, Sum.Delta2    := lapply(.SD, sum, na.rm = T), .SDcols = "Delta2", by = Class]
  DT.weighted[, Count         := .N, by = Class]
  DT.weighted[, ubs           := sqrt(Sum.Delta2/(2*Count)), by = Class]
  DT.weighted[, s_Delta2      := lapply(.SD, sd, na.rm = T), .SDcols = "Delta2", by = Class]
  Data <- DT.weighted[,.SD,.SDcols =c(name.date, name.V1, name.V2, "Delta2", "Mean", "Class")]
  DT.weighted <- unique(DT.weighted[, .SD, .SDcols = c("Class","Mean.Class", "s_Mean", "Sum.Delta2", "Count", "ubs","s_Delta2")])
  
  #returning
  data.table::setkey(DT.weighted, "Class")
  return(list(Data = Data, DT.weighted = DT.weighted))}


# Function calculate_uncertainty_bins to group candidate measurements and reference measurements per bin
#' @param df (mandatory) dataframe or data.table with data measurements. it shall include columns: RM, RM1/RM2 and CM1/CM2.
#' @param Type charater vector, default is Type. Executing this function is only valid if Type == "Fixed Type Testing"
#' @param bin_width 8optional), integer, default value is 5. Number of lags used to compute ubSRM and UbsCM versus reference
calculate_uncertainty_bins <- function(df, bin_width = 10, Type = "Fixed Type Testing") {
  
  stopifnot(Type == "Fixed Type Testing")
  
  # Binning per RM1
  df_bin <- df %>%
    mutate(bin = cut(
      RM1,
      breaks = seq(
        floor(min(RM1, na.rm = TRUE)),
        ceiling(max(RM1, na.rm = TRUE)) + bin_width,
        by = bin_width
      ),
      include.lowest = TRUE
    ))
  
  # Extract  bin from  "(a,b]"
  get_bin_center <- function(b) {
    nums <- as.numeric(unlist(regmatches(b, gregexpr("[0-9.]+", b))))
    mean(nums)
  }
  
  ubs_by_bin <- df_bin %>%
    group_by(bin) %>%
    summarise(
      ubsRM = sqrt(sum((RM1 - RM2)^2) / (2 * n())),
      ubsCM = sqrt(sum((CM1 - CM2)^2) / (2 * n())),
      bin_center = get_bin_center(first(bin)),
      count = n(),
      .groups = "drop"
    ) %>%
    filter(!is.na(ubsRM), count >= 2)
  return(ubs_by_bin)
}

#' Create between sensor and between reference standard uncertainty plots for ubsRM and ubsCM by bin
#' @param df_ubs
#' @param Instrument (mandatory) character vector, name of the instrument that shall be included in the header of df
#' @param Pollutant
#' @param Unit (mandatory) character vector, unit of measurements
#' @param bin_width (mandatory) numeric, width per bin used to compute df_ubs
#' @param min.Count (optional) numeric default value is 4. Minimum count per bin for considering the bin as valid
#' @return 2 ggplots with  ubsRM and ubsCM vs reference concentrationa 
create_uncertainty_plots <- function(df_ubs, Instrument, Pollutant, Unit, bin_width = 10, min.Count = 4, SelectDQO, LV.Interval = 0.3) {
  
  # Limits
  y_max <- max(df_ubs$ubs.x, df_ubs$ubs.y, na.rm = TRUE)
  count_max <- max(c(df_ubs$count.x, df_ubs$Count.y), na.rm = TRUE)
  
  # --- Plot ubsRM ---
  plot_ubsRM <- ggplot(data = df_ubs[!is.na(Count.x)], aes(x = Class + bin_width/2)) +
    geom_col(aes(y = ubs.x), fill = "steelblue", width = bin_width - 1, position = "identity") +
    geom_line(aes(y = Count.x / count_max *  y_max, color = "Data count (scaled)"), linewidth = 1) +
    geom_point(aes(y = Count.x / count_max *  y_max, color = "Data count (scaled)")) +
    geom_text(aes(y = ubs.x, label = round(ubs.x,2)), vjust = -0.5, size = 4, color = "steelblue") +
    scale_y_continuous(
      limits = c(0, y_max),
      #name = "ubsRM",
      sec.axis = sec_axis(~ . * count_max / y_max , name = "# data per bin")) +
    scale_color_manual(name = "", values = c("Data count (scaled)" = "darkred")) +
    labs(
      title = paste0("ubsRM per concentration interval - ", Instrument, " ", Pollutant),
      x = "Concentration interval of RM",
      y = paste0("Between reference standard uncertainty, ubsRM in ", Unit)) +
    theme_minimal() +
    theme(
      axis.title.x       = element_text(size = 14),
      axis.text.y.left   = element_text(color = "steelblue", size = 12),                                       # Change text color
      axis.title.y.left  = element_text(color = "steelblue", size = 14),                                      # Change title color
      axis.line.y.left   = element_line(color = "steelblue"),                                                  # Change line color
      axis.text.y.right  = element_text(color = "darkred", size = 12),                                        # Change text color
      axis.title.y.right = element_text(color = "darkred", size = 14, angle = 90, hjust = 0.5, vjust = 0.5), # Change title color
      axis.line.y.right  = element_line(color = "darkred")                                                    # Change line color
    ) +
    guides(color = "none")
  
  # --- Plot ubsCM ---
  plot_ubsCM <- ggplot(data = df_ubs[!is.na(Count.y)], aes(x = Class + bin_width/2)) +
    geom_col(aes(y = ubs.y), fill = "grey60", width = bin_width - 1, position = "identity")
  
  # Showing used bin for ubsCM if any
  n.Rows <- df_ubs[Count.y >= min.Count & Class + bin_width/2 <= (1 + LV.Interval) * SelectDQO$LV & Class + bin_width/2 >= (1 - LV.Interval) * SelectDQO$LV, which = T]
  if(length(n.Rows)  > 0){
    plot_ubsCM <- plot_ubsCM + 
      geom_col(data = df_ubs[n.Rows], aes(y = ubs.y), fill = "steelblue", width = bin_width - 1, position = "identity") +
      geom_vline(xintercept = (1 - LV.Interval) * SelectDQO$LV, color= "steelblue", size = 1, linetype="dashed") +
      geom_vline(xintercept = (1 + LV.Interval) * SelectDQO$LV, color= "steelblue", size = 1, linetype="dashed")
  }
  
  plot_ubsCM <- plot_ubsCM +
    geom_line(aes(y =  Count.y / count_max *  y_max, color = "Data count (scaled)"), linewidth = 1) +
    #geom_text(aes(y = Count.y / count_max *  y_max, label = Count.y,y), vjust = -0.5, size = 4, color = "darkred") +
    geom_point(aes(y =  Count.y / count_max *  y_max, color = "Data count (scaled)")) +
    geom_text(aes(y = ubs.y, label = round(ubs.y,2)), vjust = -0.5, size = 4, color = "grey60") +
    scale_y_continuous(
      limits = c(0, y_max),
      #name = "ubsCM",
      sec.axis = sec_axis(~ . * count_max / y_max, name = "# data per bin")) +
    scale_color_manual(name = "", values = c("Data count (scaled)" = "darkred")) +
    geom_hline(yintercept = min.Count / count_max *  y_max, color= "darkred", size = 1, linetype="dashed") +
    annotate("text", x = max(df_ubs[!is.na(Count.y)]$Class, na.rm = T) + bin_width + 4, y = min.Count / count_max *  y_max, label = "Min.", color = "darkred") + 
    labs(
      title = paste0("ubsCM per concentration interval - ", Instrument, " ", Pollutant),
      x = "Concentration interval of CM",
      y = paste0("Between candidate standard uncertainty, ubsCM in ", Unit, ", the bins in blue are within ±",LV.Interval*100,"% of LV whit n >= ", min.Count)) +
    theme_minimal()+
    theme(
      axis.title.x       = element_text(size = 14),
      axis.text.y.left   = element_text(color = "grey60", size = 12),                                          # Change text color
      axis.title.y.left  = element_text(color = "grey60", size = 14),                                         # Change title color
      axis.line.y.left   = element_line(color = "grey60"),                                                     # Change line color
      axis.text.y.right  = element_text(color = "darkred", size = 12),                                        # Change text color
      axis.title.y.right = element_text(color = "darkred", size = 14, angle = 90, hjust = 0.5, vjust = 0.5), # Change title color
      axis.line.y.right  = element_line(color = "darkred")                                                    # Change line color
    ) +
    guides(color = "none")
  
  return(gridExtra::grid.arrange(plot_ubsRM, plot_ubsCM, ncol = 2, nrow = 1)) # list(ubsRM_plot = plot_ubsRM, ubsCM_plot = plot_ubsCM)
}

######## Create CM plot function ##########C
#' @description Create CM analysis plots with outlier statistics
#' @param df (Mandatory) Data frame with columns CM1, CM2, RM_AVG, Campaign, Instrument, Size Fraction
#' @param Name.CM (optional) vector of character vectors, default is NULL If not NULL: names of column in df with data for candidate analysers 
#' @param Name.SN (optional) vector of character vectors, default is NULL If not NULL: names of column in df with part number of instrument of each measurement of all candidate analysers 
#' @param Name.RM (optional) vector of character vectors, default is NULL If not NULL: names of column in df with data for reference analysers 
#' @param correction_type (optional) Character string for correction type (default: "original"). It will be used to identify if the plots are related to raw measurements or to possible correction
#' @param Type (opional) Character string, default is "Fixed type testing". Values can be "Fixed type testing", "Fixed on-going" or "Indicative" as from the PickerInput (what to to for indicative)
#' @param low_thr (optional) Numeric value for low threshold of outliers (default: 2)
#' @param thr_high.rel (optional) Numeric value for relative low threshold of outliers (default: 25%)
#' @param min.N (optional) Numeric value for the minimum values of data with valid Name.CM and Name.RM measurement data
#' @return A list containing ggplot2 objects with CM analysis plots and summary statistics
#' @examples create_cm_analysis_plots(df_Fidas200_PM25)
#' @details This function creates comprehensive analysis plots for CM (Candidate Method)
#'          data comparison with RM (Reference Method). It handles both type testing
#'          (with RM1/RM2) and ongoing verification Types.

create_cm_analysis_plots <- function(df,
                                     Name.CM = NULL, Name.SN = NULL, Name.RM = NULL,
                                     correction_type = "original",
                                     Type =  "Fixed type testing",
                                     low_thr = 2, thr_high.rel = 0.25,
                                     min.N = 100) { 
  # Load required libraries
  library(dplyr)
  library(ggplot2)
  library(gridExtra)
  library(tidyr)
  library(patchwork)
  library(moments)
  
  # Extract instrument and size fraction from data
  Instrument <- unique(df$Instrument)
  Pollutant  <- unique(df$Pollutant)
  
  # Determine CM and RM columns based on Name.CM and Name.RM
  # For Fixed on-going: use any CM columns present in the dataframe
  if (is.null(Name.CM)) Name.CM <- names(df)[grep("^CM\\d+$", names(df))]
  if (length(Name.CM) == 0) stop(paste0("No CM columns found in dataframe for Type ", Type, "."))
  # For Fixed on-going: use any CM columns present in the dataframe
  if (is.null(Name.RM)) Name.RM <- names(df)[grep("^CM\\d+$", names(df))]
  if (length(Name.RM) == 0) stop(paste0("No RM columns found in dataframe for Type ", Type, "."))
  
  # Validate data based on Type
  if (Type == "Fixed type testing") {
    if (length(Name.CM) < 2) {
      stop("Type testing requires both CM1 and CM2 columns.")
    }
    if (length(Name.RM) < 2) {
      stop("Type testing requires both RM1 and RM2 columns.")
    }
    # Filter only rows with BOTH CM and BOTH RM valid for type testing
    df_filtered <- df[is.finite(rowSums(df[,.SD,.SDcols = c(Name.CM, Name.RM)]))]
    
    # Count how many rows are discarded
    original_rows  <- nrow(df)
    filtered_rows  <- nrow(df_filtered)
    discarded_rows <- original_rows - filtered_rows
    
    if (filtered_rows < min.N) {
      stop(paste0(
        "Missing data rows available for Fixed type testing.",
        " Requires ", min.N, " rows with both CM1 & CM2 and RM1 & RM2 valid data. Currently ", nrow(df_filtered), " valid data."
      ))
    }
    
    if (discarded_rows > 0) {
      message <- paste(Type, ": Filtered", discarded_rows, "rows missing CM1/CM2 or RM1/RM2.",
                       "Using", filtered_rows, "complete rows out of", original_rows, "total.")
    } else message <- paste(Type, ": Using all", filtered_rows, "complete rows.")
    
    shinyalert::shinyalert(title = "data availability",
                           text = message,
                           closeOnEsc = TRUE,
                           closeOnClickOutside = TRUE,
                           html = FALSE,
                           type = "info",
                           showConfirmButton = TRUE,
                           showCancelButton  = FALSE,
                           confirmButtonText = "OK",
                           confirmButtonCol  = "#AEDEF4",
                           timer             = 10000,
                           imageUrl          = "",
                           animation         = FALSE)
    
    # Use filtered dataset
    df <- df_filtered
  } else if(type == "Fixed on-going") {
    
    if (length(Name.CM) == 0) {
      stop("At least one CM channel is required.")
    }
    if (length(Name.RM) == 0) {
      stop("At least one RM channel is required.")
    }
    # For ongoing verification, use available RM data
    if (length(Name.RM) == 2) {
      message("Ongoing verification: Both RM1 and RM2 available, will average them.")
    }
  } else if(type == "Indicative") {}
  
  # Reshape data to long format for plotting
  df_long <- 
    #df %>% select(date, RM, all_of(Name.CM), all_of(Name.SN), Campaign) 
    df[,.SD,.SDcols = c("date", "RM", Name.CM, Name.SN, "Campaign")] %>%
    pivot_longer(
      cols      = all_of(Name.CM),
      names_to  = "CM_type",
      values_to = "CM_value"
    ) %>%
    mutate(
      # Map serial numbers to CM types
      SN = case_when(
        CM_type == Name.CM[1] ~ !!sym(Name.SN[1]),
        CM_type == Name.CM[2] ~ !!sym(Name.SN[2])
      ),
      # Calculate differences
      diff_CM  = CM_value - RM,
      diff_abs = abs(diff_CM),
      # High threshold: 25% of RM value
      thr_high = thr_high.rel * RM,
      # Categorize outliers based on thresholds
      outlier_cm = case_when(
        diff_abs > low_thr & diff_abs > thr_high ~ "High (>25% RM)",
        diff_abs > low_thr ~ paste0("Low (>", low_thr, " µg/m³)"),
        TRUE ~ "OK"
      )
    )
  
  # Define factor levels for better ordering
  df_long$CM_type    <- factor(df_long$CM_type, levels = Name.CM)
  df_long$outlier_cm <- factor(df_long$outlier_cm,
                               levels = c("OK", paste0("Low (>", low_thr, " µg/m³)"), "High (>25% RM)"))
  df_long$SN <- factor(df_long$SN)
  
  # Create mixed thresholds for plotting (low_thr µg/m³ or 25% of RM_AVG, whichever is greater)
  df_long <- df_long %>%
    mutate(
      thr_mixed_pos = ifelse(RM <= 8,  low_thr,  thr_high.rel * RM),
      thr_mixed_neg = ifelse(RM <= 8, -low_thr, -thr_high.rel * RM)
    )
  
  # Calculate global statistics
  n_total <- nrow(df_long)
  
  # Define color scheme for outlier categories
  colors_outlier <- c(
    "OK" = "darkgreen",
    "Low (>2 µg/m³)" = "orange",
    "High (>25% RM)" = "red"
  )
  
  mean_diff_cm <- df_long %>%
    group_by(CM_type) %>%
    summarise(
      mean_diff = round(mean(diff_CM, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  # Create plots for each CM channel
  plot_list <- list()
  
  # Build CM1, CM2 ... plot if available, can manage more than 2 analysers
  for(CM in Name.CM){
    plot_list[[paste0("combined_",CM)]] <- make_plot_for_CM(CM, df_long, low_thr, Instrument, Pollutant, colors_outlier, mean_diff_cm)
    assign(paste0("mean_diff_", CM),
           ifelse(CM %in% mean_diff_cm$CM_type,
                  mean_diff_cm$mean_diff[mean_diff_cm$CM_type == CM],
                  NA))}
  
  # Create side-by-side layout if both CM1 and CM2 exist
  if (length(Name.CM) == 2) {
    plot_list$combined_side_by_side <- plot_list[[paste0("combined_", Name.CM[1])]] | plot_list[[paste0("combined_", Name.CM[2])]]
  }
  
  # Add statistics to output
  plot_list$n_total         <- n_total
  plot_list$Instrument      <- Instrument
  plot_list$Pollutant       <- Pollutant
  plot_list$correction_type <- correction_type
  plot_list$Type            <- Type
  
  return(plot_list)
  # add to return  mean_diff, median_diff, sd_diff, ubs_cm , pct_out_high, pct_out_low <- round(n_out_low / n_total * 100,1) + identified outliers at the 2 levels (2 and 5)?
}

# Function make_plot_for_CM
#' @description Helper function to create 3-panel layout for a specific CM channel used in create_cm_analysis_plots
#' @param CM_name (mandatory) character vector, column of header of one candidate analyser
#' @param df_long (mandatory) data.frame created within create_cm_analysis_plots
#' @param low_thr (mandatory) set in create_cm_analysis_plots
#' @return one ggplot object combining the 3-panel plot for all CM analyser
make_plot_for_CM <- function(CM_name, df_long, low_thr, Instrument, Pollutant, colors_outlier, mean_diff_cm) {
  
  # Filter data for specific CM channel
  df_sub        <- df_long %>% filter(CM_type == CM_name)
  mean_diff_val <- mean_diff_cm$mean_diff[mean_diff_cm$CM_type == CM_name]
  
  # Calculate outlier statistics for annotation
  outlier_stats <- df_sub %>%
    group_by(outlier_cm) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(percentage = round(count / nrow(df_sub) * 100, 1))
  
  # Panel 1: Bland-Altman plot
  p1 <- ggplot(df_sub, aes(x = RM, y = diff_CM, color = outlier_cm, shape = SN)) +
    geom_point(alpha = 0.75, size = 2) +
    geom_hline(yintercept = 0, color = "blue", linetype = "dashed", linewidth = 0.8) +
    geom_line(aes(y = thr_mixed_pos), color = "orange", linetype = "dotted", linewidth = 0.5) +
    geom_line(aes(y = thr_mixed_neg), color = "orange", linetype = "dotted", linewidth = 0.5) +
    geom_smooth(
      method = "loess",
      formula = y ~ x,
      color = "black",
      linetype = "dashed",
      linewidth = 0.8,
      se = TRUE,           # add confidence interval
      aes(group = 1)       # Group = 1 is use to avoid that geom_smooth create a line for each shape, a warning is issued but it is normal (can be avoided with suppressWarnings() but warning are always interesting
    ) +
    scale_color_manual(values = colors_outlier, name = "Outlier Category") +
    scale_shape_discrete(name = "Serial Number") +
    labs(
      title = paste("Bland-Altman:", Instrument, Pollutant, "-", CM_name),
      x = "RM_AVG (μg/m³)",
      y = "CM - RM_AVG (μg/m³)"
      #,shape = "SN"
    ) +
    annotate("text",
             x = min(df_sub$RM, na.rm = TRUE) + 0.5 * max(df_sub$RM, na.rm = TRUE),
             y = max(df_sub$diff_CM, na.rm = TRUE),
             label = paste(
               "OK: ", outlier_stats$count[outlier_stats$outlier_cm == "OK"],
               " (", outlier_stats$percentage[outlier_stats$outlier_cm == "OK"], "%)",
               "\nLow: ", outlier_stats$count[outlier_stats$outlier_cm == "Low (>2 µg/m³)"],
               " (", outlier_stats$percentage[outlier_stats$outlier_cm == "Low (>2 µg/m³)"], "%)",
               "\nHigh: ", outlier_stats$count[outlier_stats$outlier_cm == "High (>25% RM)"],
               " (", outlier_stats$percentage[outlier_stats$outlier_cm == "High (>25% RM)"], "%)"
             ),
             color = "black"
    ) +
    theme_minimal() +
    theme(
      plot.subtitle = element_text(face = "bold", color = "darkblue")
    )
  
  # Panel 2: Histogram of differences
  p2 <- ggplot(df_sub, aes(x = diff_CM)) +
    geom_histogram(bins = 20, fill = "lightblue", color = "black", alpha = 0.7) +
    geom_vline(xintercept = mean_diff_val, color = "blue", linewidth = 1) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
    geom_vline(
      xintercept = c(-low_thr, low_thr),
      linetype = "dotted", color = "orange", linewidth = 0.5
    ) +
    labs(
      title = paste("Distribution of differences -", CM_name),
      x = "CM - RM_AVG (μg/m³)",
      y = "Counts",
      subtitle = paste("MBE =", mean_diff_val, "μg/m³ - skw =",
                       round(moments::skewness(df_sub$diff_CM, na.rm = TRUE), 1), "- kurt =", 
                       round(moments::kurtosis(df_sub$diff_CM, na.rm = TRUE), 1))
    ) +
    theme_minimal() +
    theme(
      plot.subtitle = element_text(face = "bold", color = "darkblue")
    )
  
  # Panel 3: Time series of differences
  p3 <- ggplot(df_sub, aes(x = date, y = diff_CM, color = outlier_cm, shape = SN)) +
    geom_point(size = 1.8) +
    geom_hline(yintercept = 0, color = "blue", linetype = "dashed", linewidth = 0.8) +
    geom_hline(
      yintercept = c(-low_thr, low_thr),
      color = "orange", linetype = "dotted", linewidth = 0.5
    ) +
    scale_color_manual(values = colors_outlier, name = "Outlier Category") +
    scale_shape_discrete(name = "Serial Number") +
    labs(
      title = paste("Differences over time -", CM_name),
      x = "Date",
      y = "CM - RM_AVG (μg/m³)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Combine panels vertically (3 rows)
  combined_CM <- p1 / p2 / p3
  
  return(combined_CM)
}

# Function Linearity_check ##############C
#' @description Performs linearity check for CM data against RM reference data
#' @param df (mandatory) Data.table or data frame with columns CM1, CM2, RM1, RM2, RM, Instrument, Pollutant
#' @param Type (opional) Character string, default is "Fixed type testing". Values can be "Fixed type testing", "Fixed on-going" or "Indicative" as from the PickerInput (what to to for indicative)
#' @param r2_threshold (opional) R² threshold for linearity check (default: 0.95)
#' @param sd_rel_threshold (opional) Relative SD threshold for linearity check (default: 0.125)
#' @param LV_list (mandatory) data.table with columns LV, and not used: DQO and Ur as from function GetDQO()
#' @param Name.CM (optional) vector of character vectors, default is NULL If not NULL: names of column of df with data for candidate analysers 
#' @param Name.RM (optional) vector of character vector, default is NULL If not NULL: names of column of df with data for reference analysers 
#' @return A list with final_plot (combined plot), R2_stats (data frame of R² values),
#'         SD_stats (data frame of SD values), status (named vector of linearity status)
#' @details This function performs linearity assessment using OLS regression,
#'          Orthogonal regression, and LOESS smoothing. It checks both R² and
#'          relative standard deviation criteria.
#' @examples linearity_check(df_Fidas200_PM25)
#'
linearity_check <- function(df,
                            Type = "Fixed type testing", # "Fixed on-going" or indicative
                            r2_threshold     = 0.95,
                            sd_rel_threshold = 0.125,
                            LV_list,
                            Name.CM = NULL, Name.RM = NULL) {
  library(dplyr)
  library(ggplot2)
  library(deming)
  library(patchwork)
  
  # Extract metadata
  Instrument <- unique(df$Instrument)
  Pollutant  <- unique(df$Pollutant)
  LV         <- LV_list$LV
  
  # Not sure this is necessary since it is checked before
  if (length(Instrument) != 1 | length(Pollutant) != 1) {
    stop("Data frame must contain exactly one Instrument and one Size Fraction.")
    
    #### Add a warning window and remove the stop ##################################################################################################C
  }
  
  # Determine CM columns based on Name.CM
  if (!is.null(Name.CM)){
    cm_cols <- Name.CM
  } else {
    # For Fixed on-going: use any CM columns present in the dataframe
    Name.CM <- names(df)[grep("^CM\\d+$", names(df))]
  }
  if (length(Name.CM) == 0) {
    stop(paste0("No CM columns found in dataframe for Type ", Type, "."))
  }
  # Determine RM reference (RM_AVG if available, else RM1)
  ref_col <- if (length(Name.RM) > 1 && "RM" %in% names(df)) "RM" else "RM1"
  
  # Initialize storage for results
  plots <- list()
  R2_stats <- list()
  SD_stats <- list()
  status_list <- list()
  
  # Process each CM column
  for (cm_col in Name.CM) {
    
    # Filter data for current CM column and reference
    df_cm <- df %>%
      filter(!is.na(.data[[cm_col]]), !is.na(.data[[ref_col]])) %>%
      mutate(
        x = .data[[ref_col]],
        y = .data[[cm_col]]
      )
    
    if (nrow(df_cm) < 3) {
      warning(paste("Insufficient data for", cm_col, "- skipping linearity check."))
      next
    }
    
    # 1) OLS REGRESSION
    mod_ols  <- lm(y ~ x, data = df_cm)
    r2       <- summary(mod_ols)$r.squared
    resid_sd <- sd(residuals(mod_ols))
    sd_rel   <- resid_sd / LV
    
    # 2) Orthogonal REGRESSION
    n <- nrow(df_cm)
    mod_dem <- deming::deming(y ~ x,
                              data = df_cm,
                              xstd = rep(1, n),
                              ystd = rep(1, n)
    )
    
    slope_dem     <- mod_dem$coefficients[2]
    intercept_dem <- mod_dem$coefficients[1]
    
    # 3) LINEARITY STATUS
    pass_r2 <- round(r2, 2)     >= r2_threshold
    pass_sd <- round(sd_rel, 1) <= sd_rel_threshold
    
    # Get serial number
    SN_col <- if (cm_col == "CM1") "SN1" else "SN2"
    SN_value <- if (SN_col %in% names(df_cm)) {
      unique(df_cm[[SN_col]])[1]
    } else {
      "N/A"
    }
    
    # Determine status and color
    if (pass_r2 & pass_sd) {
      status <- paste0(" (SN ", SN_value, "): LINEAR")
      status_color <- "forestgreen"
    } else {
      status <- paste0(" (SN ", SN_value, "): NOT LINEAR")
      status_color <- "red3"
    }
    
    # 4) CREATE PLOT
    annotation_text <- paste0(
      "R² (OLS): ", sprintf("%.2f", r2), "\n",
      "SD(res): ", sprintf("%.1f", resid_sd), " μg/m³\n",
      "SD_rel: ", sprintf("%.1f%%", sd_rel * 100), "\n",
      "Linearity: ", ifelse(pass_r2 & pass_sd, "PASS", "FAIL"), "\n",
      "Orthogonal: y = ", sprintf("%.3f", intercept_dem),
      " + ", sprintf("%.3f", slope_dem), "·x\n",
      "Thresholds:\n",
      "- R² ≥ ", r2_threshold, "\n",
      "- SD_rel ≤ ", sd_rel_threshold * 100, "%\n",
      "- n = ", nrow(df_cm)
    )
    
    x_pos <- max(df_cm$x, na.rm = TRUE) * 0.95
    y_pos <- min(df_cm$y, na.rm = TRUE) * 1.05
    
    
    df_cm$line_type <- "Data Points"
    
    # Definisci l'ordine desiderato per la legenda
    line_order <- c("Orthogonal", "Loess", "Reference")
    
    p <- ggplot(df_cm, aes(x = x, y = y)) +
      geom_point(alpha = 0.6, size = 2) +
      # Orthogonal line
      geom_abline(
        aes(
          slope = slope_dem,
          intercept = intercept_dem,
          color = "Orthogonal"
        ),
        linewidth = 1.1,
        key_glyph = "path"
      ) +
      # Loess smooth
      geom_smooth(
        method = "loess", formula = y ~ x,
        se = TRUE,
        alpha = 0.25,
        aes(color = "Loess"),
        linewidth = 0.8
      ) +
      # Reference line (1:1)
      geom_abline(
        aes(
          slope = 1,
          intercept = 0,
          color = "Reference"
        ),
        linetype = "dashed",
        linewidth = 0.8,
        key_glyph = "path"
      ) +
      # Color scale with explicit breaks order
      scale_color_manual(
        name = "Line Types",
        values = c(
          "Orthogonal" = "blue",
          "Loess" = "black",
          "Reference" = "red"
        ),
        labels = c(
          "Orthogonal Regression",
          "Loess Smooth",
          "1:1 Reference"
        ),
        breaks = line_order # Questo forza l'ordine nella legenda
      ) +
      labs(
        title = paste0(cm_col, " – ", status),
        x = "RM Reference (μg/m³)",
        y = paste0(cm_col, " (μg/m³)")
      ) +
      theme_bw(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", color = status_color, size = 14),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "bottom",
        legend.box = "horizontal") +
      annotate("text",
               x = x_pos,
               y = y_pos,
               label = annotation_text,
               hjust = 1,
               vjust = 0,
               size = 5,
               color = "black",
               family = "mono")
    plots[[cm_col]] <- p
    
    # 5) SAVE STATISTICS
    R2_stats[[cm_col]] <- data.frame(
      CM = cm_col,
      SN = SN_value,
      R2 = round(r2, 2),
      R2_Threshold = r2_threshold,
      R2_Pass = pass_r2,
      stringsAsFactors = FALSE
    )
    
    SD_stats[[cm_col]] <- data.frame(
      CM = cm_col,
      SN = SN_value,
      SD_res = round(resid_sd, 3),
      SD_rel = round(sd_rel, 4),
      SD_rel_Percent = round(sd_rel * 100, 2),
      SD_Threshold = sd_rel_threshold,
      SD_Pass = pass_sd,
      stringsAsFactors = FALSE
    )
    
    status_list[[cm_col]] <- status
  }
  
  # Combine all plots
  if (length(plots) > 0) {
    final_plot <- patchwork::wrap_plots(plots, ncol = 2) +
      plot_annotation(
        title = paste(Instrument, Pollutant, "- Linearity Check"),
        subtitle = paste("LV =", Instrument, "μg/m³", " | Type:", Type),
        theme = theme(
          plot.title = element_text(size = 16, face = "bold"),
          plot.subtitle = element_text(size = 12)
        )
      )
  } else {
    final_plot <- NULL
    warning("No linearity plots generated due to insufficient data.")
  }
  
  return(list(
    final_plot = final_plot, # ,
    R2_stats   = do.call(rbind, R2_stats),  # to be returned
    SD_stats   = do.call(rbind, SD_stats),  # to be returned
    status     = unlist(status_list)        # to be returned
    # to be added sd(slope and intercept)
  ))
} 

### U_orth_DF: Function Orthogonal regression without plotting (Vs 180505) ====
#' Function Orthogonal Fitting regression line (using orthogonal regression, Deming, ordinary least square or Weighted Least Square), The equation of the regression line is used for computing measurement uncertainty according to the Guide for Demonstration of Equivalence of candidate Methods ... No plotting foreseen 
#' This function will fit a regression line between two columns (y vs x, or any other selected column names) included in a data.table named Mat.
#' The type of regression line depends on the value of argument "Regression" ... 
#'
#' @param Mat Mandatory, data.table or DataFrame of data including Case number, Date, x, y + optional ubsRM and/or ubss if ubsRM and/or ubss are not constant for all xi.
#' The columns shall be in the order: "case", "Date", "xis", "yis","ubsRM", "ubss" with whatever column names.
#' xis cannot be 0, otherwise the function crashes due to Ur divided by 0.
#' @param Versus character, default is NULL. If not NULL, name of the column in data.table Mat which is used with the gam fitting to fit RSi. If NULL, RSi will befitted versus reference data (xis). 
#' @param Regression character, default is "TLS", possible values are "OLS" (ordinary least square), "OLS.weighing,"WLS_OLS" (Weighted Least Square), "Deming" and "TLS" (Total Least Square or orthogonal regression). For "TLS", Delta is 1 and for "Deming" Delta is ubss^2/ubsRM^2. See https://en.wikipedia.org/wiki/Deming_regression.
#' The weighted least squares (WLS or GLS) estimator of the coefficients of a linear regression is a generalization of the ordinary least squares (OLS) estimator. It is used to deal with situations in which the OLS estimator is not BLUE (best linear unbiased estimator) because one of the main assumptions of the Gauss-Markov theorem, namely that of homoskedasticity and absence of serial correlation, is violated. In such situations, provided that the other assumptions of the Gauss-Markov theorem are satisfied, the WLS estimator is BLUE. 
#' @param Tested.Models  character vector, default is c("OLS", "OLS.Weighing", "Quantile", "WLS_OLS", "WLS_ubss", "Deming", "TLS"). This the list of Linear Regression models being compared.
#'          Possible values include : 
#'         "TLS" (total least square or orthogonal regression), Deming (ubsRM and ubss required), "OLS" (ordinary least square), "OLS.weighing, "WLS_ubss" (weighted least sqaue with ubss) and "WLS_OLS" (Weighted Least Square with residuals of OLS).
#'          For "TLS", Delta is 1 and for "Deming" Delta is ubss^2/ubsRM^2. Stangely enough Deming::deming is able to manage variable ubsRM and ubss! See https://en.wikipedia.org/wiki/Deming_regression.
#' @param ubsRM Optional, numeric, default is NULL. Random standard uncertainty of reference measurements xis, given as a constant value for all xis reference values.
#' @param variable.ubsRM logical, default is FALSE. If FALSE, ubsRM is used as constant random standard uncertainties for all xis reference values. If TRUE ubsRM can be given in Mat and is used for each raw of Mat xis or as a percentage (perc.ubsRM).
#' @param perc.ubsRM numeric default value 0.02. Use to compute ubsRm in case variable.ubsRM is TRUE and ubsRM is not included in Mat. In this case, Mat$ubsRM = perc.ubsRM * Mat[["xis"]] 
#' @param ubss numeric (default = NULL ), random standard uncertainty of sensor measurements, yis, given as a constant value for all yis sensor values
#' @param variable.ubss logical, default is FALSE. If FALSE, ubss is used as constant random standard uncertainties for all yis sensor values. If TRUE ubss given in Mat and is used for each sensor value
#' @param perc.ubss numeric default value NULL. Use to compute ubss in case variable.ubss is TRUE as Mat$ubss = perc.ubss * Mat[["yis"]] 
#' @param Add.ubss logical, default is TRUE If TRUE ubss is added to  Mat$Rel.RSS. If FALSE ubss is not added to  Mat$Rel.RSS.
#' @param Fitted.RS Optional, logical, default is FALSE. If TRUE the square residuals (RSi) are fitted using a General Additive Model, provided that the null hypothesis of no correlation between xis and RSi is rejected when the probability is lower than 0.05, (p < 0.05)
#' @param Forced.Fitted.RS logical, default is FALSE. If TRUE even if the variance of residuals is constant, RS is Gam fitted.
#' @param Plot_Line (optional) logical, default is FALSE If TRUE the calibration line is added using par(new=TRUE) to an existing scatterplot
#' @param Verbose logical, default is FALSE. If TRUE messages are displayed during execution.
#' @details: 
#' The Homogeneity of variance of residuals is tested for the computation of RSi adding Ur in a new column of Mat
#' It is necessary to source function Cal_Line before
#' @example  Model <- U_orth_DF(Mat = na.omit(data.table(case   = 1:nrow(VMM_data), Date   = VMM_data[["date"]], xis    = VMM_data[[paste0(Fraction,"ref_",DataSet,Replicate)]], yis    = VMM_data[[paste0(Fraction,"can_",DataSet, Replicate)]],
#' ubss   = Reg.lin[[paste0(Fraction,".Intercept")]][which(Reg.lin$DataSet == DataSet)] + Reg.lin[[paste0(Fraction,".Slope")]][which(Reg.lin$DataSet == DataSet)] * VMM_data[[paste0(Fraction,"ref_",DataSet,Replicate)]])),
#' Versus = NULL, Regression = "Deming", 
#' variable.ubsRM = FALSE, ubsRM = if(DataSet == "PM25") 2.5 else 1.02, perc.ubsRM = 0.02,
#' ubss = NULL, variable.ubss = TRUE, perc.ubss = Reg.lin[[paste0(Fraction,".Slope")]][Reg.lin$DataSet == DataSet], Add.ubss = FALSE,
#' Fitted.RS = FALSE, Forced.Fitted.RS = FALSE, Verbose = TRUE)

#' @return a list with parameters: "mo","sdo", "mm","sdm", "b1", "ub1", "b0", "ub0", "RSS","rmse", "mbe", "Correlation", "nb", "RS.Fitted", "Regression", "Add.ubss", m2 (the linear calibration model), ft (a flextable with comparison of the possible Regression types) and a data.table called "Mat" with columns: "case", "Date", "xis", "yis","ubsRM", "RS", "Ur", "U", "Rel.bias", "Rel.RSS"
#' returning a list with slope (b and ub), intercept (a and ua), the sum of square of residuals (RSS),
#' the root means square of error (RMSE), the mean bias error (mbe), the coefficient of correlation (Correlation),
#' the number of valid measurements (nb) and Mat with (relative) expanded measurement uncertainty.
#' The list also include the parameters of computation: "RS.Fitted", "Regression" and "Add.ubss".
#' Negative Rel.RSS are set to 0.

#' @examples: empty
U_orth_DF <- function(Mat, Versus = NULL, Regression = "TLS", Tested.Models = c("OLS", "OLS.Weighing", "Quantile", "WLS_OLS", "WLS_ubss", "Deming", "TLS"),
                      variable.ubsRM = FALSE, ubsRM = NULL, perc.ubsRM = 0.02,
                      variable.ubss  = FALSE, ubss  = NULL, perc.ubss  = NULL, Add.ubss = TRUE,
                      Fitted.RS = FALSE, Forced.Fitted.RS = FALSE, ID = NULL,
                      Verbose = FALSE, Plot, Plot_Line = FALSE, Keep.Cols = FALSE) {
  
  #checking that Mat is not empty
  if (exists("Mat") && !is.null(Mat) && nrow(Mat) > 0) {
    
    # Setting Versus with xis if NULL
    if (is.null(Versus)) Versus <- "xis"
    stopifnot(Versus %in% names(Mat))
    
    # checking that at least x and y are given
    Missing.Cols <- setdiff(c("case", "Date", Versus, "yis","ubsRM", "ubss"), names(Mat))
    stopifnot(all(!c(Versus, "yis") %in% Missing.Cols))
    # Checking if ubsRM and ubss are provided in Mat
    ubsRM.Mat <- !"ubsRM" %in% Missing.Cols
    ubss.Mat  <- !"ubss"  %in% Missing.Cols
    
    # Checking if some columns needs be deleted
    Additional.Cols <- setdiff(names(Mat), c("case", "Date", Versus, "yis","ubsRM", "ubss"))
    if(!Keep.Cols && Verbose && length(Additional.Cols) >0){
      futile.logger::flog.warn(paste0("[U_orth_DF] Mat includes additional columns that are discarded: ", paste(Additional.Cols, collapse = ", ")))
      Mat[, (Additional.Cols) := NULL]}
    
    # Convert Mat to data.table if needed, order on versus, ordering on Versus
    if (!data.table::is.data.table(Mat)) Mat <- data.table(Mat)
    data.table::setkeyv(Mat, Versus)
    
    # Filtering for the rows with complete Versus and yis data only
    Mat <- Mat[is.finite(rowSums(Mat[, c(Versus,"yis"), with = FALSE]))]
    nb <- nrow(Mat)
    if (!nb > 5) return(futile.logger::flog.error("[U_orth_DF] Mat does not contains any complete rows with xis and yis"))
    
    # Setting ubsRM and ubss in Mat
    if(!ubsRM.Mat){
      if (!variable.ubsRM) {
        stopifnot(!is.null(ubsRM))
        data.table::set(Mat,  j = "ubsRM", value = rep(ubsRM, nb))
        if (Verbose) futile.logger::flog.info(paste0("[U_orth_DF] u(bs,RM) = ", ubsRM, " constant value."))
      } else {
        stopifnot(!is.null(perc.ubsRM))
        Mat[, ubsRM := perc.ubsRM * Mat[[Versus]]]
        if (Verbose) futile.logger::flog.info(paste0("[U_orth_DF] computing u(bs,RM) as ", 100 * perc.ubsRM," percents of reference data (Mat$xis)."))
      }
    } else if (Verbose) futile.logger::flog.info(paste0("[U_orth_DF] using u(bs,RM) provided in Mat."))
    
    if(!ubss.Mat){
      if (!variable.ubss) {
        stopifnot(!is.null(ubss) && !"ubss" %in% names(Mat))
        data.table::set(Mat,  j = "ubss", value = rep(ubss, nb))
        if (Verbose) futile.logger::flog.info(paste0("[U_orth_DF] u(bs,s) = ", ubss, " constant value."))
      } else {
        stopifnot(!is.null(perc.ubss))
        Mat[, ubss := perc.ubss * Mat$yis]
        if (Verbose) futile.logger::flog.info(paste0("[U_orth_DF] computing u(bs,s) as ", 100 * perc.ubss," percents of sensor data (Mat$xis)."))
      }
    } else if (Verbose) futile.logger::flog.info(paste0("[U_orth_DF] using u(bs,s) provided in Mat."))
    
    # Fitting Linear Models ###
    # Ordinary least square Linear Regression, OLS with outliers being discarded ###
    Formula <- as.formula(paste0("yis ~ ", Versus))
    OLS <- Cal_Line(x = Mat$xis, y = Mat$yis, Mod_type = "Linear", Weighted = FALSE, Plot_Line = Plot_Line, Verbose = Verbose)
    # Looking for and discarding influential points for OLS and any other model
    OLS.Outliers <- performance::check_outliers(OLS)
    if(length(which(OLS.Outliers)) > 0){
      Mat <- Mat[-which(OLS.Outliers)]
      OLS <- Cal_Line(x = Mat$xis, y = Mat$yis, Mod_type = "Linear", Weighted = FALSE, Plot_Line = Plot_Line, Verbose = Verbose)
      if(Verbose){
        print(OLS.Outliers)
        futile.logger::flog.info(paste0("[U_orth_DF] Rows of Mat: ", paste(which(OLS.Outliers), collapse = ", "),
                                        " discarded as influential using Cook's distance. OLs update without influential data"))}
    } else if(Verbose) futile.logger::flog.info(paste0("[U_orth_DF] no data Mat: discarded as influential using Cook's distance of OLS."))
    
    # checking assumptions of linear models
    OLS.Heteroskedasticity <- performance::check_heteroskedasticity(OLS)
    OLS.Models             <- performance::check_model(OLS)
    OLS.Autocorrelation    <- performance::check_autocorrelation(OLS)
    OLS.Diagnostic         <- rempsyc::nice_table(rempsyc::nice_assumptions(OLS), col.format.p = 2:4)
    # Printing message if requested
    if(Verbose){
      cat("Checking heteroskedascity, homogenity of variance of residuals for OLS model\n")
      print(OLS.Heteroskedasticity)
      
      cat("Checking autocorrelation of residuals for OLS model\n")
      print(OLS.Autocorrelation) # check_autocorrelation not existing for Weighted regression line
      
      cat(paste0("Plotting check of linearity for ", Regression, " model\n"))
      plot(OLS.Models)
      
      cat(paste0("Plotting table with test results of heteroskedascity, homogenity of variance and autocorrelation of residuals for ", Regression, " model\n"))
      OLS.Diagnostic
    }
    
    # Setting Models to fit
    if(is.null(Tested.Models)) Tested.Models <- c("OLS", "OLS.Weighing", "Quantile", "WLS_OLS", "WLS_ubss", "Deming", "TLS")
    
    # Ordinary Least square with  weighing according to scattering in 10 lags over the xis range
    if("OLS.Weighing" %in% Tested.Models){
      OLS.Weighing <- Cal_Line(x = Mat$xis, y = Mat$yis, Mod_type = "Linear", Weighted = TRUE, Auto.Lag = TRUE,  Plot_Line = Plot_Line, Verbose = Verbose)
    }
    
    # Quantile regression at the median, no weighing per lags
    if("OLS.Weighing" %in% Tested.Models){
      Quantile <- Cal_Line(x = Mat$xis, y = Mat$yis, Mod_type = "Linear.Robust", Probs = 0.5, Weighted = FALSE, Plot_Line = Plot_Line, Verbose = Verbose, f_coef1 = "%.1f", f_coef2 = "%.2f")
    }
    
    # Common parameters for Delta tools and TLS
    nb  <- nrow(Mat)
    mo  <- mean(Mat[[Versus]])
    mm  <- mean(Mat[["yis"]])
    sdo <- sd(Mat[[Versus]])
    sdm <- sd(Mat[["yis"]])
    
    # Fitting all other regressions: TLS (orthogonal regression), Deming, WLS
    # Fitting TLS with standard.errors for coefficients using 1000 bootstrap samples
    # The Delta of Deming (ratio of ubss/ubsRM for orthogonal regression is 1 (equal variance for x and y)
    if("TLS" %in% Tested.Models){
      TLS  <- Cal_Line(x = Mat$xis, y = Mat$yis, s_y = resid(OLS), Mod_type = "TLS", Weighted = FALSE, Plot_Line = Plot_Line, Verbose = Verbose)
    }
    
    # Fitting Deming regression, it takes care of constant or variable Errors on x and y
    if("Deming" %in% Tested.Models){
      
      if (!variable.ubsRM && !variable.ubss) {
        # Constant ubs
        Delta <- (ubss/ubsRM)^2
        if (Verbose) futile.logger::flog.warn("[U_orth_DF] \"Deming\" regression with constant u(bs,RM) and u(bs,s).")
        # if boot = TRUE and keep.boot = FALSE, MethComp::Deming returns a matrix with coefficients, se and confidence interval
        Deming.MethComp     <- MethComp::Deming(x = Mat[[Versus]], y = Mat[["yis"]], vr = Delta, boot = TRUE, keep.boot = FALSE)
        # if boot = TRUE and keep.boot = TRUE, MethComp::Deming returns a matrix with Coefficients each row being a subsample of x and y. Ideal to compute covariance between slope and intercept
        Deming.MethComp.COV <- MethComp::Deming(x = Mat[[Versus]], y = Mat[["yis"]], vr = Delta, boot = TRUE, keep.boot = TRUE)
        Deming <- list(
          coefficients =  Deming.MethComp[1:2, "Estimate"],
          x = Mat[[Versus]],
          y =  Mat[["yis"]],
          fitted.values = Deming.MethComp[1, "Estimate"] + Deming.MethComp[2, "Estimate"] * Mat[[Versus]],
          residuals = Mat[["yis"]] - Deming.MethComp[1, "Estimate"] + Deming.MethComp[2, "Estimate"] * Mat[[Versus]],
          rank = 2, 
          df.residuals = nrow(Mat) - 1 - 1,
          terms = terms(lm(yis~xis, data=Mat)),
          call = "MethComp::Deming(x = Mat[[Versus]], y = Mat[[\"yis\"]], vr = Delta, boot = TRUE, keep.boot = TRUE/FALSE)",
          Deming.MethComp = Deming.MethComp,
          Deming.MethComp.COV  = Deming.MethComp.COV
        )
        
      } else {
        
        if (Verbose) futile.logger::flog.warn("[U_orth_DF] \"Deming\" regression with variable u(bs,RM) and/or u(bs,s).")
        Deming <- Cal_Line(x = Mat$xis, s_x = Mat$ubsRM, y = Mat$yis, s_y = Mat$ubss, Mod_type = "Deming", Weighted = FALSE, Plot_Line = Plot_Line, Verbose = Verbose)
        
      }
    }
    
    # Fitting WLS regression, y weights are determined from the residuals using the residuals of the OLS model
    if("WLS_OLS" %in% Tested.Models){
      if(!is.null(perc.ubss) && FALSE){ # I added FALSE to be sure that the weights will be computed with the residuals of OLS which give better WLS fitting than with ubsss
        Mat[, Weights := nb * (Mat$ubss)^-2 / sum(Mat$ubss^-2)]
      } else Mat[, Weights := nb*resid(OLS)^-2/sum(resid(OLS)^-2)]
      #WLS <- lm(Formula, data = Mat, weights = Weights)
      WLS_OLS <- Cal_Line(x = Mat$xis, y = Mat$yis, s_y = resid(OLS), Mod_type = "Linear", Weighted = FALSE, Plot_Line = Plot_Line, Verbose = Verbose)
    }
    
    # weights with ubss
    if("WLS_ubss" %in% Tested.Models){
      WLS_ubss <- Cal_Line(x = Mat$xis, y = Mat$yis, s_y = Mat$ubss, Mod_type = "Linear", Weighted = FALSE, Plot_Line = Plot_Line, Verbose = Verbose)
    }
    
    # Creating a table with the different models
    Tested.Models <- Tested.Models[Tested.Models %in% ls()]
    if(length(Tested.Models) > 0){
      for(Model in Tested.Models){
        if (Model %in% c("OLS", "WLS_OLS", "WLS_ubss","OLS.Weighing")){
          st.errors <- sqrt(diag(vcov(get(Model))))
          b0  <- round(coef(get(Model))[1], 2)
          ub0 <- round(st.errors[1], 2)
          b1  <- round(coef(get(Model))[2], 3)
          ub1 <- round(st.errors[2], 3)
          covb0b1 <- round(vcov(get(Model))[1,2],4)
          R2 <-  round(broom::glance(get(Model))$r.squared,3)
          
          # # other computation of standard errors:
          # ub0 <- lmtest::coeftest(OLS, vcov. = car::hccm(OLS, type = "hc1"))[1, "Std. Error"]
          # ub1 <- lmtest::coeftest(OLS, vcov. = car::hccm(OLS, type = "hc1"))[2, "Std. Error"]
          
        } else if (Model == "Quantile"){
          # getting coefficients and standard error https://cran.r-project.org/web/packages/quantreg/vignettes/rq.pdf
          Summary <- summary(Quantile,se = "nid", cov = TRUE)
          b0  <- round(Summary$coefficients[,1][1], 2)
          ub0 <- round(Summary$coefficients[,2][1], 2)
          b1  <- round(Summary$coefficients[,1][2], 3)
          ub1 <- round(Summary$coefficients[,2][2], 3)
          R2 = round(R1_rq(Mat[[Versus]], Mat$yis, probs = 0.5),3)
          covb0b1 <- round(Summary$cov[1, 2], 4 ) # chatGPT asked
        } else if (Model == "Deming"){
          if (!variable.ubsRM && !variable.ubss) {
            b0  <- round(Deming$coefficients[1], 2)
            ub0 <- round(Deming$Deming.MethComp[1,2], 2)
            b1  <- round(Deming$coefficients[2], 3)
            ub1 <- round(Deming$Deming.MethComp[2,2], 3)
            covb0b1 <- round(cov(Deming$Deming.MethComp.COV[,c(1,2)])[1,2], 4)
          } else {
            b0  <- round(coef(Deming)[1], 2)
            ub0 <- round(sqrt(diag(Deming$variance)[1]), 2)
            b1  <- round(coef(Deming)[2], 3)
            ub1 <- round(sqrt(diag(Deming$variance)[2]), 3)
            covb0b1 <- round(sqrt(diag(Deming$variance)[2]), 4)}
          R2 = NA
        } else if (Model == "TLS"){
          b0  <- round(coef(TLS)[1], 2)
          ub0 <- round(TLS$fit.MethComp[1,2], 2)
          b1  <- round(coef(TLS)[2], 3)
          ub1 <- round(TLS$fit.MethComp[2,2], 3)
          # ub0 and ub1 are computed by 1000 bootstrap (as in deming::Deming) while in the GDE, equations are given (see below). The values ub0  and ub1 are different when computed by the two methods
          covb0b1 <- round(cov(TLS$fit.MethComp.COV)[1,2],4)
          
          # https://stats.stackexchange.com/questions/86453/coefficient-of-determination-of-a-orthogonal-regression/180173
          R2 <- round(summary(TLS)$r.squared,3)
        }
        if(exists("Lin.Reg")){
          Lin.Reg <- data.table::rbindlist(list(Lin.Reg, list(Model = Model, b0 = b0, ub0 = ub0, b1 = b1, ub1 = ub1, R2 = R2, covb0b1 = covb0b1)), use.names = T, fill =T)
        } else {
          Lin.Reg <- data.table::data.table(Model = Model, b0 = b0, ub0 = ub0, b1 = b1, ub1 = ub1, R2 = R2, covb0b1 = covb0b1)
        }
      }
    }
    
    # Selecting regression type for computing U and Ur
    if((Regression %in% Tested.Models)){
      m2 = get(Regression)
      b0      <- Lin.Reg[Model == Regression]$b0
      ub0     <- Lin.Reg[Model == Regression]$ub0
      b1      <- Lin.Reg[Model == Regression]$b1
      ub1     <- Lin.Reg[Model == Regression]$ub1
      covb0b1 <- Lin.Reg[Model == Regression]$covb0b1
    } else {
      return(
        futile.logger::flog.error("[U_orth_DF] unknown regression type. 
                                  Only \"OLS\", \"OLS.Weighing\", \"WLS\", , \"Quantile\", \"Deming\" (Delta is ubss^2/ubsRM^2) or \"TLS\" (Delta is 1) regressions can be used"))
    }
    
    # Squares of Residuals and bias (vector of values)
    Mat[, fitted    := b0 + b1 * Mat[[Versus]]]
    Mat[, bias      := (b0 + (b1 - 1) * Mat[[Versus]])] # Bias from identity line x
    Mat[, residuals := Mat[["yis"]] - fitted]
    Mat[, RS        := residuals^2]
    
    # Regression statistics for Target Diagram (see delta tool user guide)
    rmse  <- sqrt(sum(Mat[["residuals"]]^2) / (nb - 2 - 1)) # the degrees of freedom are n - k (number of coeffieint of the regression line: 2) - 1
    mbe   <- mean(Mat[["yis"]] - Mat[[Versus]])
    mae   <- mean(abs(Mat[["yis"]] - Mat[[Versus]]))
    Correlation <- cor(Mat[[Versus]],Mat[["yis"]])
    
    # testing for heterosKedasticity with Breusch Pagan test, the two packages gives the same p.value
    # https://www.r-bloggers.com/2016/01/how-to-detect-heteroscedasticity-and-rectify-it/
    # https://bookdown.org/ccolonescu/RPoE4/heteroskedasticity.html
    Breusch.Pagan     <- lmtest::bptest(formula = OLS, studentize = T)
    Breusch.Pagan     <- skedastic::breusch_pagan(mainlm = OLS, koenker = TRUE, statonly = FALSE)
    
    # fitting the residuals for RSS/(n-2) computation
    if (Verbose){
      futile.logger::flog.info("[U_orth_DF] Breusch-Pagan test: null hypothesis means constant variance of residuals along x axis (homoskedasticity). 
                                     The null hyposthesis is rejected if p-value of the Breusch-Pagan test < 0.05 (heterosKedasticity)")  
      futile.logger::flog.info(paste0("[U_orth_DF] Finally, p-value =  ", format(Breusch.Pagan$p.value, digits = 4)))
      futile.logger::flog.info(paste0("[U_orth_DF] Argument \"Fitted.RS\" in U_orth_DF(): ", Fitted.RS, ". If FALSE the square residuals are not fitted and constant RSS is computed."))
      futile.logger::flog.info(paste0("[U_orth_DF] Argument \"Forced.Fitted.RS\" in U_orth_DF(): ", Forced.Fitted.RS,
                                      ". If TRUE the square residuals are fitted, even if the variance of residuals along x axis is constant."))} 
    
    if (Fitted.RS && (Breusch.Pagan$p.value < 0.05 || Forced.Fitted.RS)) {
      if (Verbose) futile.logger::flog.info("[U_orth_DF] The variance of residuals is not constant or is set to be fitted. RSi are calculated after applying a General Additive Model fitting.")
      # Fitting with gam Vs Versus ("Xi")
      # if any y value is zero getting Warning: Error in eval: non-positive values not allowed for the 'gamma' family (we had 0.5 % of min(xis) to avoid this
      
      Formula <- as.formula(paste0("sqrt(RS) ~ s(", Versus, ")"))
      z <- mgcv::gam(Formula, data = Mat,family=Gamma(link=log) )
      
      # # see https://stats.stackexchange.com/questions/270124/how-to-choose-the-type-of-gam-parameters
      # z <- mgcv::gam(Formula, data = Mat, method = "REML", select = TRUE)
      
      Mat[, RS := fitted(z)^2]
      # Sum of squares of Residuals (one constant value)
      RSS     <- sum(Mat$RS)
      
      if (Verbose) print(summary(z))
      
    } else {
      
      if (Verbose) futile.logger::flog.info("[U_orth_DF] The variance of residuals is constant along x axis or Fitted.RS is set to FALSE. Constant RSS is calculated.")
      if (Verbose) futile.logger::flog.info("[U_orth_DF] RSS is calculated with equation for constant residuals.")
      # Sum of squares of Residuals (one constant value)
      RSS     <- sum(Mat$RS)
      if (Verbose) futile.logger::flog.info(paste0("[U_orth_DF] RSS is the square root of sum of squares of Residuals divided by n - 2 = ", format(sqrt(RSS/(nb-2)), digit = 3)))
      # No need to fit a line in this case
      Mat[, RS := rep(RSS/(nb-2), times = .N)]}
    
    # Plotting RS
    if (Verbose) {
      # See https://stackoverflow.com/questions/17093935/r-scatter-plot-symbol-color-represents-number-of-overlapping-points
      
      ## Use densCols() output to get density at each point
      x <- grDevices::densCols(Mat[[Versus]],sqrt(Mat$residuals^2), colramp=colorRampPalette(c("black", "white")))
      Mat$dens <- col2rgb(x)[1,] + 1L
      ## Map densities to colors
      cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", 
                                  "#FCFF00", "#FF9400", "#FF3100"))(256)
      
      Mat$col <- cols[Mat$dens]
      
      plot(sqrt(Mat$residuals^2) ~ get(Versus), data = Mat, type = "p", col = col, xlab = Versus, main = "Square of residuals versus reference values for RSS")
      lines(Mat[[Versus]],sqrt(Mat[["RS"]]), col = "red", xlab = Versus); grid()}
    
    # Checking if RSS^2 - Mat$ubsRM^2 + Mat$ubss^2 < 0 that results in an error using sqrt(RSS^2 - Mat$ubsRM^2) of the rs.RSS. Replacing with 0
    # and Calculating parameters for modified Target diagram Rel.bias and Rel.RSS
    if (Add.ubss) neg.RSS <- which(Mat$RS - Mat[["ubsRM"]]^2 + Mat[["ubss"]]^2 < 0) else neg.RSS <- which(Mat$RS - Mat[["ubsRM"]]^2 < 0)
    if (length(neg.RSS) > 0) {
      if (Verbose) futile.logger::flog.warn("[U_orth_DF] Some \"RS - ubsRM^2\" are negative and square roots cannot be calculated.")
      if (Verbose) futile.logger::flog.info("[U_orth_DF] ubsRM maybe too high and should be corrected.")
      if (Verbose) futile.logger::flog.info("[U_orth_DF] The \"RS - ubsRM^2\" that are negative will be set to 0 when computing uncertainties.")
      
      Mat[neg.RSS,  Rel.RSS := 0]
      Positives <- setdiff(1:nb,neg.RSS)
      
      if (length(Positives) > 0){
        if (Add.ubss){
          Mat[Positives, Rel.RSS := 2 * sqrt(Mat[Positives,ubss]^2 + Mat[Positives,RS] - Mat[Positives,ubsRM]^2) / Mat[Positives][[Versus]]]
        } else Mat[Positives, Rel.RSS := 2 * sqrt(Mat[Positives,RS] - Mat[Positives,ubsRM]^2) / Mat[Positives][[Versus]]]}
    }  else {
      # mat$RS are not changed and they are already calculated
      if (Verbose) futile.logger::flog.info("[U_orth_DF] All \"RSS/(nb - 2) or RSi - ubsRM^2\" are positives. ubsRM makes sence.")
      
      if (Add.ubss){
        Mat[, Rel.RSS := 2 * sqrt(Mat$ubss^2 + Mat$RS - Mat$ubsRM^2) / Mat[[Versus]]]
      } else Mat[, Rel.RSS := 2 * sqrt(Mat$RS - Mat$ubsRM^2) / Mat[[Versus]]]
    }
    Mat[, Rel.bias := 2 * (b0/Mat[[Versus]] + (b1 - 1))]
    
    #### Calculating uncertainty
    Mat[, Ur := sqrt(Mat$Rel.bias^2 + Mat$Rel.RSS^2) * 100]
    Mat[, U  := Mat$Ur / 100 * Mat[[Versus]]]
    
    # Indicators for ubsRM
    Mat[, Max.ubsRM := sqrt((Mat$Rel.RSS * Mat[[Versus]] / 2)^2 + Mat$ubsRM^2 + Mat$bias^2)]
    Mat[, Max.RSD   := Max.ubsRM / Mat[[Versus]]]
    
    # Printing
    if (Verbose) {
      cat("--------------------------------\n")
      cat(sprintf("mean of x   : %.1g +/- %.1g",mo,sdo),"\n")
      cat(sprintf("Intercept b0: %.4g +/- %.4g",mm,sdm), "\n")
      cat(sprintf("u of b1     : %.4g +/- %.4g",b1,ub1),"\n")
      cat(sprintf("u of b0     : %.4g +/- %.4g",b0,ub0), "\n")
      cat(sprintf("R2: %.4g",Correlation^2), "\n")
      if (Fitted.RS && (Breusch.Pagan$p.value < 0.05 || Forced.Fitted.RS)) {
        cat("The residuals are not constant. RS are fitted with a general Additive model (k=5) see in returned matrix. \n")
      } else {
        cat("The residuals are constant. RSS is calculated with equation for constant residuals:")
        cat(sprintf("RSS: %.4g ",Mat$RSS[1]), "\n")}
      cat(sprintf("RMSE : %.4g ",rmse), "\n")
      cat(sprintf("mbe  : %.4g ",mbe), "\n")
      cat(sprintf("n    : %.4g ",nb), "\n")
      
      # Printing the different regression models
      ft <- flextable::flextable(Lin.Reg)
      ft
    }
    calib <- list(mo = mo, sdo = sdo, mm = mm, sdm = sdm, b1 = b1, ub1 = ub1, b0 = b0, ub0 = ub0, RSS = RSS, rmse = rmse,
                  mb2 = mbe, Correlation = Correlation, nb = nb, Mat = Mat, Fitted.RS = Fitted.RS, Regression = Regression, Add.ubss = Add.ubss, m2 = m2,
                  OLS.Models = OLS.Models, Lin.Reg = Lin.Reg)
    if(Verbose){ # elements created only if Verbose = TRUE
      for(Element in c("OLS", "OLS.Outliers", "OLS.Diagnostic", "OLS.Weighing", "WLS_OLS", "WLS_ubss", "Quantile", "Deming", "TLS","Lin.Reg")) calib[[Element]] <- get(Element) }
  } else {
    cat("Mat is empty. Returning NAs.")
    calib <- list(mo = NA,sdo = NA, mm = NA,sdm = NA, b1 = NA, ub1 = NA, b0 = NA, ub0 = NA, RSS = NA,rmse = NA, mbe = NA, Correlation = NA, nb = NA,
                  Mat = NA, Regression = Regression, Add.ubss = Add.ubss, m2 = NA,
                  OLS.Models = NA, Lin.Reg = NA)
    if(Verbose){ # elements created only if Verbose = TRUE
      for(Element in c("OLS", "OLS.Diagnostic", "Lin.Reg")) calib[[Element]] <- NA}}
  
  # returning list of info
  return(calib)
}


U_orth_DF.2 <- function(Mat, Versus = NULL, Regression = "TLS", Tested.Models = c("OLS", "OLS.Weighing", "Quantile", "WLS_OLS", "WLS_ubss", "Deming", "TLS"),
                        variable.ubsRM = FALSE, ubsRM = NULL, perc.ubsRM = 0.02,
                        variable.ubss  = FALSE, ubss  = NULL, perc.ubss  = NULL, Add.ubss = TRUE,
                        Fitted.RS = FALSE, Forced.Fitted.RS = FALSE, ID = NULL,
                        Verbose = FALSE, Plot, Plot_Line = FALSE) {
  
  #checking that Mat is not empty
  if (exists("Mat") && !is.null(Mat) && nrow(Mat) > 0) {
    
    # Setting Versus with xis if NULL
    if (is.null(Versus)) Versus <- "xis"
    stopifnot(Versus %in% names(Mat))
    
    # checking that at least x and y are given
    Missing.Cols <- setdiff(c("case", "Date", Versus, "yis","ubsRM", "ubss"), names(Mat))
    stopifnot(all(!c(Versus, "yis") %in% Missing.Cols))
    # Checking if ubsRM and ubss are provided in Mat
    ubsRM.Mat <- !"ubsRM" %in% Missing.Cols
    ubss.Mat  <- !"ubss"  %in% Missing.Cols
    #colnames(Mat) <- c("case", "Date", Versus, "yis","ubsRM", "ubss")[1:length(colnames(Mat))]
    Additional.Cols <- setdiff(names(Mat), c("case", "Date", Versus, "yis","ubsRM", "ubss"))
    if(Verbose && length(Additional.Cols) >0){
      futile.logger::flog.warn(paste0("[U_orth_DF] Mat includes additional columns that are discarded: ", paste(Additional.Cols, collapse = ", ")))
      Mat[, (Additional.Cols) := NULL]}
    
    # Convert Mat to data.table if needed, order on versus, ordering on Versus
    if (!data.table::is.data.table(Mat)) Mat <- data.table(Mat)
    data.table::setkeyv(Mat, Versus)
    
    # Filtering for the rows with complete Versus and yis data only
    Mat <- Mat[is.finite(rowSums(Mat[, c(Versus,"yis"), with = FALSE]))]
    nb <- nrow(Mat)
    if (!nb > 5) return(futile.logger::flog.error("[U_orth_DF] Mat does not contains any complete rows with xis and yis"))
    
    # Setting ubsRM and ubss in Mat
    if(!ubsRM.Mat){
      if (!variable.ubsRM) {
        stopifnot(!is.null(ubsRM))
        data.table::set(Mat,  j = "ubsRM", value = rep(ubsRM, nb))
        if (Verbose) futile.logger::flog.info(paste0("[U_orth_DF] u(bs,RM) = ", ubsRM, " constant value."))
      } else {
        stopifnot(!is.null(perc.ubsRM))
        Mat[, ubsRM := perc.ubsRM * Mat[[Versus]]]
        if (Verbose) futile.logger::flog.info(paste0("[U_orth_DF] computing u(bs,RM) as ", 100 * perc.ubsRM," percents of reference data (Mat$xis)."))
      }
    } else if (Verbose) futile.logger::flog.info(paste0("[U_orth_DF] using u(bs,RM) provided in Mat."))
    
    if(!ubss.Mat){
      if (!variable.ubss) {
        stopifnot(!is.null(ubss) && !"ubss" %in% names(Mat))
        data.table::set(Mat,  j = "ubss", value = rep(ubss, nb))
        if (Verbose) futile.logger::flog.info(paste0("[U_orth_DF] u(bs,s) = ", ubss, " constant value."))
      } else {
        stopifnot(!is.null(perc.ubss))
        Mat[, ubss := perc.ubss * Mat$yis]
        if (Verbose) futile.logger::flog.info(paste0("[U_orth_DF] computing u(bs,s) as ", 100 * perc.ubss," percents of sensor data (Mat$xis)."))
      }
    } else if (Verbose) futile.logger::flog.info(paste0("[U_orth_DF] using u(bs,s) provided in Mat."))
    
    # Fitting Linear Models ###
    # Ordinary least square Linear Regression, OLS with outliers being discarded ###
    Formula <- as.formula(paste0("yis ~ ", Versus))
    OLS     <- Cal_Line(x = Mat$xis, y = Mat$yis, Mod_type = "Linear", Weighted = FALSE, Plot_Line = Plot_Line, Verbose = Verbose)
    # Looking for and discarding influential points
    OLS.Outliers <- performance::check_outliers(OLS)
    if(length(which(OLS.Outliers)) > 0){
      Mat <- Mat[-which(OLS.Outliers)]
      OLS <- Cal_Line(x = Mat$xis, y = Mat$yis, Mod_type = "Linear", Weighted = FALSE, Plot_Line = Plot_Line, Verbose = Verbose)
      if(Verbose){
        print(OLS.Outliers)
        futile.logger::flog.info(paste0("[U_orth_DF] Rows of Mat: ", paste(which(OLS.Outliers), collapse = ", "),
                                        " discarded as influential using Cook's distance. OLs update without influential data"))}
    } else if(Verbose) futile.logger::flog.info(paste0("[U_orth_DF] no data Mat: discarded as influential using Cook's distance of OLS."))
    
    # checking assumptions of linear models
    if(Verbose){
      cat("Checking heteroskedascity, homogenity of variance of residuals for OLS model\n")
      print(performance::check_heteroskedasticity(OLS))
      
      cat("Checking autocorrelation of residuals for OLS model\n")
      print(performance::check_autocorrelation(OLS)) # check_autocorrelation not existing for Weighted regression line
      
      cat(paste0("Plotting heteroskedascity, homogenity of variance of residuals for ", Regression, " model"))
      plot(performance::check_model(OLS))
      OLS.Diagnostic <- rempsyc::nice_table(rempsyc::nice_assumptions(OLS), col.format.p = 2:4)
      OLS.Diagnostic}
    
    # Ordinary Least square with  weighing according to scattering in 10 lags over the xis range
    OLS.Weighing <- Cal_Line(x = Mat$xis, y = Mat$yis, Mod_type = "Linear", Weighted = TRUE, Auto.Lag = TRUE,  Plot_Line = Plot_Line, Verbose = Verbose)
    
    # Quantile regression at the median, no weighing per lags
    Quantile <- Cal_Line(x = Mat$xis, y = Mat$yis, Mod_type = "Linear.Robust", Probs = 0.5, Weighted = FALSE, Plot_Line = Plot_Line, Verbose = Verbose, f_coef1 = "%.1f", f_coef2 = "%.2f")
    
    # Common parameters for Delta tools and TLS
    nb  <- nrow(Mat)
    mo  <- mean(Mat[[Versus]])
    mm  <- mean(Mat[["yis"]])
    sdo <- sd(Mat[[Versus]])
    sdm <- sd(Mat[["yis"]])
    
    # Fitting all other regressions: TLS (orthogonal regression), Deming, WLS
    # Fitting TLS with standard.errors for coefficients using 1000 bootstrap samples
    # The Delta of Deming (ratio of ubss/ubsRM for orthogonal regression is 1 (equal variance for x and y)
    TLS  <- Cal_Line(x = Mat$xis, y = Mat$yis, s_y = resid(OLS), Mod_type = "TLS", Weighted = FALSE, Plot_Line = Plot_Line, Verbose = Verbose)
    
    # Fitting Deming regression, it takes care of constant or variable Errors on x and y
    if (!variable.ubsRM && !variable.ubss) {
      
      Delta <- (ubss/ubsRM)^2
      if (Verbose) futile.logger::flog.warn("[U_orth_DF] \"Deming\" regression with constant u(bs,RM) and u(bs,s).")
      Deming  <- MethComp::Deming(x = Mat[[Versus]], y = Mat[["yis"]], vr = Delta, boot = TRUE, keep.boot = FALSE)
      if (Verbose) futile.logger::flog.warn("[U_orth_DF] \"Deming\" regression with constant u(bs,RM) and u(bs,s).")
    } else {
      if (Verbose) futile.logger::flog.warn("[U_orth_DF] \"Deming\" regression with variable u(bs,RM) and/or u(bs,s).")
    }
    Deming <- Cal_Line(x = Mat$xis, s_x = Mat$ubsRM, y = Mat$yis, s_y = Mat$ubss, Mod_type = "Deming", Weighted = FALSE, Plot_Line = Plot_Line, Verbose = Verbose)
    
    # Fitting WLS regression, y weights are determined from the residuals using the residuals of the OLS model
    if(!is.null(perc.ubss) && FALSE){ # I added FALSE to be sure that the weights will be computed with the residuals of OLS which give better WLS fitting than with ubsss
      Mat[, Weights := nb * (Mat$ubss)^-2 / sum(Mat$ubss^-2)]
    } else Mat[, Weights := nb*resid(OLS)^-2/sum(resid(OLS)^-2)]
    #WLS <- lm(Formula, data = Mat, weights = Weights)
    WLS_OLS <- Cal_Line(x = Mat$xis, y = Mat$yis, s_y = resid(OLS), Mod_type = "Linear", Weighted = FALSE, Plot_Line = Plot_Line, Verbose = Verbose)
    
    # weights with ubss
    WLS_ubss <- Cal_Line(x = Mat$xis, y = Mat$yis, s_y = Mat$ubss, Mod_type = "Linear", Weighted = FALSE, Plot_Line = Plot_Line, Verbose = Verbose)
    
    # Creating a table with the different models
    if(is.null(Tested.Models)) Tested.Models <- c("OLS", "OLS.Weighing", "Quantile", "WLS_OLS", "WLS_ubss", "Deming", "TLS")
    Tested.Models <- Tested.Models[Tested.Models %in% ls()]
    if(length(Tested.Models) > 0){
      for(Model in Tested.Models){
        if (Model %in% c("OLS", "WLS_OLS", "WLS_ubss","OLS.Weighing")){
          st.errors <- sqrt(diag(vcov(get(Model))))
          b0  <- round(coef(get(Model))[1], 2)
          ub0 <- round(st.errors[1], 2)
          b1  <- round(coef(get(Model))[2], 3)
          ub1 <- round(st.errors[2], 3)
          covb0b1 <- vcov(get(Model))[1,2]
          R2 <-  round(broom::glance(get(Model))$r.squared,3)
        } else if (Model == "Quantile"){
          # getting coefficients and standard error https://cran.r-project.org/web/packages/quantreg/vignettes/rq.pdf
          Summary <- summary(Quantile,se = "nid", cov = TRUE)
          b0  <- round(Summary$coefficients[,1][1], 2)
          ub0 <- round(Summary$coefficients[,2][1], 2)
          b1  <- round(Summary$coefficients[,1][2], 3)
          ub1 <- round(Summary$coefficients[,2][2], 3)
          R2 = round(R1_rq(Mat[[Versus]], Mat$yis, probs = 0.5),3)
          covb0b1 <- Summary$cov[1, 2] # chatGPT asked
        } else if (Model == "Deming"){
          if (!variable.ubsRM && !variable.ubss) {
            b0  <- round(Deming$coefficients[1], 2)
            ub0 <- round(sqrt(diag(Deming$variance)[1]), 2)
            b1  <- round(Deming$coefficients[2], 3)
            ub1 <- round(sqrt(Deming$variance[2,2]), 3)
            covb0b1 <- round(sqrt(diag(Deming$variance)[2]), 3)
          } else {
            b0  <- round(coef(Deming)[1], 2)
            ub0 <- round(sqrt(diag(Deming$variance)[1]), 2)
            b1  <- round(coef(Deming)[2], 3)
            ub1 <- round(sqrt(diag(Deming$variance)[2]), 3)
            covb0b1 <- round(sqrt(diag(Deming$variance)[2]), 3)}
          R2 = NA
        } else if (Model == "TLS"){
          b0  <- round(coef(TLS)[1], 2)
          ub0 <- round(TLS$fit.MethComp[1,2], 2)
          b1  <- round(coef(TLS)[2], 3)
          ub1 <- round(TLS$fit.MethComp[2,2], 3)
          # ub0 and ub1 are computed by 1000 bootstrap (as in deming::Deming) while in the GDE, equations are given (see below). The values ub0  and ub1 are different when computed by the two methods
          covb0b1 <- cov(TLS$fit.MethComp.COV)[1,2]
          
          # as in annex b of Guide for The Demonstration of Equivalence
          # Syy <- sum((Mat[["yis"]] - mm)^2)
          # Sxy <- sum((Mat[[Versus]] - mo) * (Mat[["yis"]] - mm))
          # Sxx <- sum((Mat[[Versus]] - mo)^2)
          # b1  <- (Syy - Sxx + sqrt((Syy- Sxx)^2 + 4*Sxy^2))/(2*Sxy)
          # b0  <- mm - b1 * mo
          # ub1 <- sqrt((Syy - (Sxy^2/Sxx))/((nb-2)*Sxx))
          # ub0 <- sqrt(ub1^2 * sum(Mat[[Versus]]^2)/nb)
          
          # https://stats.stackexchange.com/questions/86453/coefficient-of-determination-of-a-orthogonal-regression/180173
          # R2= 1- SSres/SStot
          # where SSres is the sum of squares of residuals, and SStot is the total sum of squares. To apply it in my case I considered:
          # SSres=bid((xi,yi);(x^,y^))B2 where d((xi,yi);(x^,y^)) is the distance of point (xi,yi) to the best fit line.
          # and SStot=bd((xi,yi);(xm,ym))B2 where xm is the mean of all the xi and ym is the mean of all the yi.
          SSres <- pracma::odregress(Mat[["xis"]], Mat[["yis"]])$ssq
          SStot <- sum((Mat[["yis"]] - mm)^2 + (Mat[[Versus]] - mo)^2)
          R2 <- round(1 - SSres/SStot,3)
        }
        if(exists("Lin.Reg")){
          Lin.Reg <- data.table::rbindlist(list(Lin.Reg, list(Model = Model, b0 = b0, ub0 = ub0, b1 = b1, ub1 = ub1, R2 = R2, covb0b1 = covb0b1)), use.names = T, fill =T)
        } else {
          Lin.Reg <- data.table::data.table(Model = Model, b0 = b0, ub0 = ub0, b1 = b1, ub1 = ub1, R2 = R2, covb0b1 = covb0b1)
        }
      }
    }
    
    # Selecting regression type for computing U and Ur
    m2 = get(Regression)
    b0  <- Lin.Reg[Model == Regression]$b0
    ub0 <- Lin.Reg[Model == Regression]$ub0
    b1  <- Lin.Reg[Model == Regression]$b1
    ub1 <- Lin.Reg[Model == Regression]$ub1
    if (Regression == "TLS") {
      # as in annex b of Guide for The Demonstration of Equivalence
      Syy <- sum((Mat[["yis"]] - mm)^2)
      Sxy <- sum((Mat[[Versus]] - mo) * (Mat[["yis"]] - mm))
      Sxx <- sum((Mat[[Versus]] - mo)^2)
      b1  <- (Syy - Sxx + sqrt((Syy- Sxx)^2 + 4*Sxy^2))/(2*Sxy)
      b0  <- mm - b1 * mo
      ub1 <- sqrt((Syy - (Sxy^2/Sxx))/((nb-2)*Sxx))
      ub0 <- sqrt(ub1^2 * sum(Mat[[Versus]]^2)/nb)
    } else if (Regression == "Deming"){
      if (!variable.ubsRM && !variable.ubss) {
        b0  <- m2[1] 
        b1  <- m2[2]
        ub0 <- m2[1,2]
        ub1 <- m2[2,2]
        browser()
        # other computation of standard errors:
        # https://bookdown.org/ccolonescu/RPoE4/heteroskedasticity.html
        ub0 <- lmtest::coeftest(OLS, vcov. = car::hccm(OLS, type = "hc1"))[1, "Std. Error"]
        ub1 <- lmtest::coeftest(OLS, vcov. = car::hccm(OLS, type = "hc1"))[2, "Std. Error"]
      } else {
        b0  <- coef(m2)[1] 
        b1  <- coef(m2)[2]
        ub1 <- sqrt(diag(m2$variance)[2])
        ub0 <- sqrt(diag(m2$variance)[1])
      }
    } else if (Regression == "Quantile"){
      
      # getting coefficients and standard error https://cran.r-project.org/web/packages/quantreg/vignettes/rq.pdf
      Summary <- summary(Quantile,se = "nid")
      b0  <- paste0(round(Summary$coefficients[,1][1], 2))
      ub0 <- paste0(round(Summary$coefficients[,2][1], 2))
      b1  <- paste0(round(Summary$coefficients[,1][2], 3))
      ub1 <- paste0(round(Summary$coefficients[,2][2], 3))
      R2 = R1_rq(Mat[[Versus]], Mat$yis, probs = 0.5)
      
    } else if (Regression %in% c("OLS", "OLS.Weighing","WLS_OLS","WLS_ubss")) {
      
      # Parameters of regression lines
      b0  <- coef(m2)[1] 
      b1  <- coef(m2)[2]
      st.errors <- sqrt(diag(vcov(m2)))
      ub0 <- st.errors[1]
      ub1 <- st.errors[2]
      
    } else return(futile.logger::flog.error("[U_orth_DF] unknown regression type. Only \"OLS\", \"OLS.Weighing\", \"WLS\", , \"Quantile\", \"Deming\" (Delta is ubss^2/ubsRM^2) or \"TLS\" (Delta is 1) regressions can be used"))
    
    # Squares of Residuals and bias (vector of values)
    Mat[, fitted    := b0 + b1 * Mat[[Versus]]]
    Mat[, bias      := (b0 + (b1 - 1) * Mat[[Versus]])] # Bias from identity line x
    Mat[, residuals := Mat[["yis"]] - fitted]
    Mat[, RS        := residuals^2]
    
    # Regression statistics for Target Diagram (see delta tool user guide)
    rmse  <- sqrt(sum(Mat[["residuals"]]^2) / (nb - 2 - 1)) # the degrees of freedom are n - k (number of coeffieint of the regression line: 2) - 1
    mbe   <- mean(Mat[["yis"]] - Mat[[Versus]])
    mae   <- mean(abs(Mat[["yis"]] - Mat[[Versus]]))
    CRMSE <- sqrt(mean(((Mat[["yis"]] - mm) - (Mat[[Versus]] - mo))^2))
    NMSD  <- (sd(Mat[["yis"]]) - sd(Mat[[Versus]])) / sd(Mat[[Versus]])
    Correlation <- cor(Mat[[Versus]],Mat[["yis"]])
    
    # testing for heterosKedasticity with Breusch Pagan test, the two packages gives the same p.value
    # https://www.r-bloggers.com/2016/01/how-to-detect-heteroscedasticity-and-rectify-it/
    Breusch.Pagan     <- lmtest::bptest(formula = OLS, studentize = T)
    Breusch.Pagan     <- skedastic::breusch_pagan(mainlm = OLS, koenker = TRUE, statonly = FALSE)
    
    # fitting the residuals for RSS/(n-2) computation
    if (Verbose){
      futile.logger::flog.info("[U_orth_DF] Breusch-Pagan test: null hypothesis means constant variance of residuals along x axis (homoskedasticity). 
                                     The null hyposthesis is rejected if p-value of the Breusch-Pagan test < 0.05 (heterosKedasticity)")  
      futile.logger::flog.info(paste0("[U_orth_DF] Finally, p-value =  ", format(Breusch.Pagan$p.value, digits = 4)))
      futile.logger::flog.info(paste0("[U_orth_DF] Argument \"Fitted.RS\" in U_orth_DF(): ", Fitted.RS, ". If FALSE the square residuals are not fitted and constant RSS is computed."))
      futile.logger::flog.info(paste0("[U_orth_DF] Argument \"Forced.Fitted.RS\" in U_orth_DF(): ", Forced.Fitted.RS,
                                      ". If TRUE the square residuals are fitted, even if the variance of residuals along x axis is constant."))} 
    if (Fitted.RS && (Breusch.Pagan$p.value < 0.05 || Forced.Fitted.RS)) {
      if (Verbose) futile.logger::flog.info("[U_orth_DF] The variance of residuals is not constant or is set to be fitted. RSi are calculated after applying a General Additive Model fitting.")
      # Fitting with gam Vs Versus ("Xi")
      # if any y value is zero getting Warning: Error in eval: non-positive values not allowed for the 'gamma' family (we had 0.5 % of min(xis) to avoid this
      
      Formula <- as.formula(paste0("sqrt(RS) ~ s(", Versus, ")"))
      z <- mgcv::gam(Formula, data = Mat,family=Gamma(link=log) )
      
      # # see https://stats.stackexchange.com/questions/270124/how-to-choose-the-type-of-gam-parameters
      # z <- mgcv::gam(Formula, data = Mat, method = "REML", select = TRUE)
      
      Mat[, RS := fitted(z)^2]
      # Sum of squares of Residuals (one constant value)
      RSS     <- sum(Mat$RS)
      
      if (Verbose) print(summary(z))
      
    } else {
      
      if (Verbose) futile.logger::flog.info("[U_orth_DF] The variance of residuals is constant along x axis or Fitted.RS is set to FALSE. Constant RSS is calculated.")
      if (Verbose) futile.logger::flog.info("[U_orth_DF] RSS is calculated with equation for constant residuals.")
      # Sum of squares of Residuals (one constant value)
      RSS     <- sum(Mat$RS)
      if (Verbose) futile.logger::flog.info(paste0("[U_orth_DF] RSS is the square root of sum of squares of Residuals divided by n - 2 = ", format(sqrt(RSS/(nb-2)), digit = 3)))
      # No need to fit a line in this case
      Mat[, RS := rep(RSS/(nb-2), times = .N)]}
    
    # Plotting RS
    if (Verbose) {
      # See https://stackoverflow.com/questions/17093935/r-scatter-plot-symbol-color-represents-number-of-overlapping-points
      
      ## Use densCols() output to get density at each point
      x <- grDevices::densCols(Mat[[Versus]],sqrt(Mat$residuals^2), colramp=colorRampPalette(c("black", "white")))
      Mat$dens <- col2rgb(x)[1,] + 1L
      ## Map densities to colors
      cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", 
                                  "#FCFF00", "#FF9400", "#FF3100"))(256)
      
      Mat$col <- cols[Mat$dens]
      
      plot(sqrt(Mat$residuals^2) ~ get(Versus), data = Mat, type = "p", col = col, xlab = Versus, main = "Square of residuals versus reference values for RSS")
      lines(Mat[[Versus]],sqrt(Mat[["RS"]]), col = "red", xlab = Versus); grid()}
    
    # Checking if RSS^2 - Mat$ubsRM^2 + Mat$ubss^2 < 0 that results in an error using sqrt(RSS^2 - Mat$ubsRM^2) of the rs.RSS. Replacing with 0
    # and Calculating parameters for modified Target diagram Rel.bias and Rel.RSS
    if (Add.ubss) neg.RSS <- which(Mat$RS - Mat[["ubsRM"]]^2 + Mat[["ubss"]]^2 < 0) else neg.RSS <- which(Mat$RS - Mat[["ubsRM"]]^2 < 0)
    if (length(neg.RSS) > 0) {
      if (Verbose) futile.logger::flog.warn("[U_orth_DF] Some \"RS - ubsRM^2\" are negative and square roots cannot be calculated.")
      if (Verbose) futile.logger::flog.info("[U_orth_DF] ubsRM maybe too high and should be corrected.")
      if (Verbose) futile.logger::flog.info("[U_orth_DF] The \"RS - ubsRM^2\" that are negative will be set to 0 when computing uncertainties.")
      
      Mat[neg.RSS,  Rel.RSS := 0]
      Positives <- setdiff(1:nb,neg.RSS)
      
      if (length(Positives) > 0){
        if (Add.ubss){
          Mat[Positives, Rel.RSS := 2 * sqrt(Mat[Positives,ubss]^2 + Mat[Positives,RS] - Mat[Positives,ubsRM]^2) / Mat[Positives][[Versus]]]
        } else Mat[Positives, Rel.RSS := 2 * sqrt(Mat[Positives,RS] - Mat[Positives,ubsRM]^2) / Mat[Positives][[Versus]]]}
    }  else {
      # mat$RS are not changed and they are already calculated
      if (Verbose) futile.logger::flog.info("[U_orth_DF] All \"RSS/(nb - 2) or RSi - ubsRM^2\" are positives. ubsRM makes sence.")
      
      if (Add.ubss){
        Mat[, Rel.RSS := 2 * sqrt(Mat$ubss^2 + Mat$RS - Mat$ubsRM^2) / Mat[[Versus]]]
      } else Mat[, Rel.RSS := 2 * sqrt(Mat$RS - Mat$ubsRM^2) / Mat[[Versus]]]
    }
    Mat[, Rel.bias := 2 * (b0/Mat[[Versus]] + (b1 - 1))]
    
    #### Calculating uncertainty
    Mat[, Ur := sqrt(Mat$Rel.bias^2 + Mat$Rel.RSS^2) * 100]
    Mat[, U  := Mat$Ur / 100 * Mat[[Versus]]]
    
    # Indicators for ubsRM
    Mat[, Max.ubsRM := sqrt((Mat$Rel.RSS * Mat[[Versus]] / 2)^2 + Mat$ubsRM^2 + Mat$bias^2)]
    Mat[, Max.RSD   := Max.ubsRM / Mat[[Versus]]]
    
    # Printing
    if (Verbose) {
      cat("--------------------------------\n")
      cat(sprintf("mean of x   : %.1g +/- %.1g",mo,sdo),"\n")
      cat(sprintf("Intercept b0: %.4g +/- %.4g",mm,sdm), "\n")
      cat(sprintf("u of b1     : %.4g +/- %.4g",b1,ub1),"\n")
      cat(sprintf("u of b0     : %.4g +/- %.4g",b0,ub0), "\n")
      cat(sprintf("R2: %.4g",Correlation^2), "\n")
      if (Fitted.RS && (Breusch.Pagan$p.value < 0.05 || Forced.Fitted.RS)) {
        cat("The residuals are not constant. RS are fitted with a general Additive model (k=5) see in returned matrix. \n")
      } else {
        cat("The residuals are constant. RSS is calculated with equation for constant residuals:")
        cat(sprintf("RSS: %.4g ",Mat$RSS[1]), "\n")}
      cat(sprintf("RMSE : %.4g ",rmse), "\n")
      cat(sprintf("mbe  : %.4g ",mbe), "\n")
      cat(sprintf("CRMSE: %.4g ",CRMSE), "\n")
      cat(sprintf("NMSD : %.4g ",NMSD), "\n")
      cat(sprintf("n    : %.4g ",nb), "\n")
      
      # Printing the different regression models
      ft <- flextable::flextable(Lin.Reg)
      ft
    }
    calib <- list(mo = mo, sdo = sdo, mm = mm, sdm = sdm, b1 = b1, ub1 = ub1, b0 = b0, ub0 = ub0, RSS = RSS, rmse = rmse,
                  mb2 = mbe, Correlation = Correlation, nb = nb, CRMSE = CRMSE, NMSD, Mat = Mat, Fitted.RS = Fitted.RS, Regression = Regression, Add.ubss = Add.ubss, m2 = m2,
                  Lin.Reg = Lin.Reg)
    if(Verbose){ # elements created only if Verbose = TRUE
      for(Element in c("OLS", "OLS.Outliers", "OLS.Diagnostic", "OLS.Weighing", "WLS_OLS", "WLS_ubss", "Quantile", "Deming", "TLS","Lin.Reg")) calib[[Element]] <- get(Element) }
  } else {
    cat("Mat is empty. Returning NAs.")
    calib <- list(mo = NA,sdo = NA, mm = NA,sdm = NA, b1 = NA, ub1 = NA, b0 = NA, ub0 = NA, RSS = NA,rmse = NA, mbe = NA, Correlation = NA, nb = NA, CRMSE = NA, 
                  NMSD = NA, Mat = NA, Regression = Regression, Add.ubss = Add.ubss, m2 = NA,
                  Lin.Reg = NA)
    if(Verbose){ # elements created only if Verbose = TRUE
      for(Element in c("OLS", "OLS.Diagnostic", "Lin.Reg")) calib[[Element]] <- NA}}
  
  # returning list of info
  return(calib)
}


### Meas_Function: Function Measurement Function x = f(y) once Calibration function (y = f(x) of sensor is established e.g with Cal_Line ====
#' This function estimates the x value using a calibration model (Model)
#' @param name  y Sensor data to be converted to concentration level using the reverse calibration function (Model)
#' @param Mod_type     : type of calibration function: Linear, Quadratic, Sigmoid
#' @param Model        : the calibration function
#' @param name.sensor character vector, default is NULL, optional name of sensor used to recognize relative humidity sensor
#' @param name.Model  character vector, default is NULL, optional, name of the file of calibration function. Use for Yatkin only to save Cal.RH.Inc and Cal.RH.Dec
Meas_Function <- function(y, Mod_type, Model, Date = NULL, name.sensor= NA_character_, name.Model = NULL, Verbose = FALSE) {
  if (Mod_type %in% c('Identity', 'Linear', 'Linear.Robust')) {
    return.cal <- (y - coef(Model)[1])/coef(Model)[2]
    if (name.sensor %in% "SHT31HE"){
      RH_95 <- which(return.cal > 95)
      if (length(RH_95) > 0) return.cal[RH_95] <- 95} 
    return(return.cal)
  } else if (Mod_type == 'TLS') {
    #browser()
    return.cal <- (y - Model$Coef[1])/Model$Coef[2]
    if (name.sensor %in% "SHT31HE"){
      RH_95 <- which(return.cal > 95)
      if (length(RH_95) > 0) return.cal[RH_95] <- 95} 
    return(return.cal)
  } else if (Mod_type == 'Linear.Robust.rqs') {
    DataXY <- data.table(y = y)
    DataXY[, quantile := cut(y, quantile(Model$Augment$y, probs = seq(0.1,0.9,0.1)))]
    levels(DataXY$quantile) <- 1:ncol(Model$Coef)
    # adding levels for y outside values of calibration
    Which.Low  <- which(y<quantile(Model$Augment$y, probs = 0.1))
    Which.High <- which(y>quantile(Model$Augment$y, probs = 0.9))
    if (length(Which.Low)  > 0) data.table::set(DataXY, i = Which.Low,  j = "quantile", value = 1)
    if (length(Which.High) > 0) data.table::set(DataXY, i = Which.High, j = "quantile", value = ncol(Model$Coef))
    DataXY[, slope := Model$Coef[2,DataXY$quantile]]
    DataXY[, intercept := Model$Coef[1,DataXY$quantile]]
    DataXY[, x := (DataXY$y - DataXY$intercept) / DataXY$slope]
    return(DataXY$x)
  } else if (Mod_type == 'Ridge') {
    # convert any column of Matrice that is not numeric (Date) to numeric
    if (!all(grepl(pattern = "numeric", x = sapply(lapply(Matrice, class), "[", 1)))) {
      Col.no.numeric <- grep(pattern = "numeric", x = sapply(lapply(Matrice, class), "[", 1), invert = TRUE)
      futile.logger::flog.warn(paste0("[Meas_Function] \"",paste(names(Matrice)[Col.no.numeric], collapse = ", "), "\" is(are) not numeric. Converting to numeric."))
      Matrice[,Col.no.numeric] <- sapply(Matrice[,Col.no.numeric],as.numeric)
    }
    # Class transformation
    if (class(Matrice) != "matrix") Matrice <- as.matrix(Matrice)
    if (class(Model$Coef) == "dgCMatrix") Model$Coeff <- Model$Coef[,1]
    # Inverse of calibration model
    M.Cov <- Matrice %*% Model$Coef[3:length(Model$Coef)]
    Estimated <- as.vector((y - (Model$Coef[1] + M.Cov ))/Model$Coef[2])
    return(Estimated)
  } else if (Mod_type == "gam") { # checking if the model was fitted from gam function
    return(predict(Model, newdata = data.frame(x = y[!is.na(y)], y = rep(numeric(0), length = length(y[!is.na(y)]))), type = "response"))
  }
}

# Function to plot uncertainty computed by GDR, 95th percentile of Delta, 2sqrt(sd(Delta)^2 + mean(Delta)^2) and computation according to GUM method
#' @param CM_Corrected (mandatory) list with 2 elements CM_Corrected and U_orth_DF as computed with reactive function CM_Corrected()
#' @param Name.CM (mandatory) vector of character vectors, names of column of CM_Corrected$CM_Corrected with data for candidate analysers
#' @param Model_Type (mandatory) character, it can be Raw, OLS, Deming or Weighted
#' @param LV.Interval description
#' @param min.N (mandatory) integer, default is 30. Minimum number of data within LV ± LV.Interval
#' @param SelectDQO mandatory list with elements LV and DQO
#' @param Unit (mandatory) character vector, unit of measurements
U.by.Model <- function(CM_Corrected, Name.CM, Model_Type, LV.Interval, min.N = 30, SelectDQO, unit.ref) {
  
  # if(Model_Type == "Raw"){
  #   Model_Type <- "Raw"
  #   Name.CM_DELTA <- paste0(Name.CM, "_DELTA")
  #   Name.CM_Corrected <- Name.CM
  # } else if(Model_Type == "OLS"){
  #   Model_Type <- "OLS"
  #   Name.CM_DELTA <- paste0(Name.CM, paste0("_",Model_Type,"_DELTA"))
  #   Name.CM_Corrected <- paste0(Name.CM, "_",Model_Type)
  # } 
  # 
  # Corrected <- melt(
  #   CM_Corrected$CM_Corrected[,.SD, .SDcols =c("date", "RM", "Campaign", Name.CM, Name.CM_DELTA, Name.CM_Corrected)],
  #   id.vars = c("date", "RM", "Campaign"),
  #   measure.vars = list(CM_DELTA = Name.CM_DELTA,
  #                       CM_Corr  = Name.CM_Corrected,
  #                       CM       = Name.CM),
  #   variable.name = "CM_Type",
  #   value.name = c("CM_DELTA", "CM_Corr", "CM")
  # )
  # # Discard incomplete rows of Corrected
  # Corrected <- Corrected[is.finite(rowSums((Corrected[,.SD,.SDcols = c("RM", "CM")])))]
  # 
  # # selected rows for uncertainty, alert message if not enough data
  # N.LV <- Corrected[RM >= (1 - as.numeric(LV.Interval)) * SelectDQO$LV & RM <= (1 + as.numeric(LV.Interval)) * SelectDQO$LV, which = T]
  # 
  # # Computing uncertainty
  # Corrected[N.LV, U.95th   := quantile(abs(Corrected[N.LV]$CM_DELTA), probs = 0.95, na.rm = T), by=.(Campaign)]
  # Corrected[N.LV, U.MBE_SD := 2 * sqrt(sd(Corrected[N.LV]$CM_DELTA, na.rm = T)^2 + mean(abs(Corrected[N.LV]$CM_DELTA), na.rm = T)^2)]
  
  # selected rows for uncertainty, alert message if not enough data
  N.LV <- CM_Corrected$U_orth_DF$Mat[xis >= (1 - as.numeric(LV.Interval)) * SelectDQO$LV & xis <= (1 + as.numeric(LV.Interval)) * SelectDQO$LV, which = T]
  if(length(N.LV) < min.N){
    my_message <- paste0("[GDE_App,Uncertainy] ERROR, missing data at LV, evaluation cannot proceed.")
    shinyalert::shinyalert(title = Title,
                           text = my_message,
                           closeOnEsc = TRUE,
                           closeOnClickOutside = TRUE,
                           html = FALSE,
                           type = "error",
                           showConfirmButton = TRUE,
                           showCancelButton  = FALSE,
                           confirmButtonText = "OK",
                           confirmButtonCol  = "#AEDEF4",
                           timer             = 5000,
                           imageUrl          = "",
                           animation         = FALSE)
    return()
  }
  CM_Corrected$U_orth_DF$Mat[N.LV, U.95th   := quantile(abs(yis - xis), probs = 0.95, na.rm = T), by=.(Campaign, SN)]
  CM_Corrected$U_orth_DF$Mat[N.LV, U.MBE_SD := 2 * sqrt(var(yis - xis, na.rm = T) + abs(mean(yis - xis, na.rm = T))^2), by=.(Campaign, SN)]
  #CM_Corrected$U_orth_DF$Mat[N.LV, MBE := mean(abs(yis - xis), na.rm = T), by=.(Campaign, SN)]
  #CM_Corrected$U_orth_DF$Mat[N.LV, SD := sd(yis - xis, na.rm = T), by=.(Campaign, SN)]
  
  # plotting without GUM estimation of uncertainty
  GGPLOT <- ggplot(data = CM_Corrected$U_orth_DF$Mat, aes(x = xis)) + 
    geom_point(aes(y = U.95th, color = "U.95th")) + 
    geom_point(aes(y = U.MBE_SD, color = "U.MBE_SD")) + 
    geom_line(data = CM_Corrected$U_orth_DF$Mat, aes(x = xis, y = U, color = "GDE")) + 
    
    geom_vline(xintercept = SelectDQO$LV) + 
    annotate("text",
             x = SelectDQO$LV + 0.04 * diff(range(CM_Corrected$U_orth_DF$Mat$x)),
             y = min(CM_Corrected$U_orth_DF$Mat[,.SD,.SDcols = c("U.MBE_SD", "U.95th", "U")], na.rm = T) + 
               0.04 * diff(range(CM_Corrected$U_orth_DF$Mat[,.SD,.SDcols = c("U.MBE_SD", "U.95th", "U")], na.rm = T)),
             label = "LV",
             color = "darkred",
             size = 4) + 
    geom_hline(yintercept = SelectDQO$DQO) + 
    annotate("text",
             x = 0.04 * diff(range(CM_Corrected$U_orth_DF$Mat$x)),
             y = SelectDQO$DQO + 0.04 * diff(range(CM_Corrected$U_orth_DF$Mat[,.SD,.SDcols = c("U.MBE_SD", "U.95th", "U")], na.rm = T)),
             label = "DQO",
             color = "darkred",
             size = 4) + 
    scale_color_manual(values = c("U.95th" = "red", "U.MBE_SD" = "orange", "GUM" = "blue", "GDE" = "green")) +
    labs(x = paste0("Mean reference data in ", unit.ref),
         y = "Expanded uncertianty of measurements in ", unit.ref) +
    facet_wrap(~ SN + Campaign) +
    theme_minimal()
  
  # GUM uncertainty
  if(Model_Type != "Raw"){
    
    browser()
    b0      <- CM_Corrected$U_orth_DF$Lin.Reg[Model == Model_Type]$b0
    ub0     <- CM_Corrected$U_orth_DF$Lin.Reg[Model == Model_Type]$ub0
    b1      <- CM_Corrected$U_orth_DF$Lin.Reg[Model == Model_Type]$b1
    ub1     <- CM_Corrected$U_orth_DF$Lin.Reg[Model == Model_Type]$ub1
    covb0b1 <- CM_Corrected$U_orth_DF$Lin.Reg[Model == Model_Type]$covb0b1
    
    CM_Corrected$U_orth_DF$Mat[, GUM := 2 * sqrt((-1/b1)^2 * (ub0^2 + RS) + ((CM_Corr - b0)/b1^2)^2 * ub1^2 - 2 * (CM_Corr - b0) / b1^3  * covb0b1)]
    
      GGPLOT <- GGPLOT + 
        geom_line(data = CM_Corrected$U_orth_DF$Mat, aes(x = xis, y = U, color = "GDE"))
  }
  return(GGPLOT)
}

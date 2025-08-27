predprob <- 
  function(pred, data4pred, Time, Statu, model, dataset, timepoints){
    resulti <- pred$distr[
      1:nrow(data4pred)
    ]$survival(timepoints) %>%
      t() %>%
      as.data.frame() %>%
      mutate(crank = pred$crank,
             time = data4pred[[Time]],
             status = data4pred[[Statu]],
             model = model,
             dataset = dataset)
    return(resulti)
  }

bootci_cindex <- function(model, data4pred){
  cindexs <- numeric(200)
  for (i in 1:200) {
    set.seed(i)
    samplei <- sample(
      1:nrow(data4pred), nrow(data4pred), replace = T
    )
    datai <- data4pred[samplei, ]
    cindexi <- as.numeric(
      model$predict_newdata(datai)$score(msr("surv.cindex"))
    )
    cindexs[i] <- cindexi
  }
  return(quantile(cindexs, c(0.025, 0.975)))
}

bootci_cindex2 <- function(model, data4pred, Time, Statu){
  cindexs <- numeric(200)
  for (i in 1:200) {
    set.seed(i)
    samplei <- sample(
      1:nrow(data4pred), nrow(data4pred), replace = T
    )
    datai <- data4pred[samplei, ]
    taski <- as_task_surv(
      datai, 
      time = Time,
      event = Statu, 
      type = "right"
    )
    predi <- stack_predict(
      trainedstack = model,
      newdata = datai,
      itask = taski,
      withraw = T
    )
    cindexi <- as.numeric(
      predi$score(msr("surv.cindex"))
    )
    cindexs[i] <- cindexi
  }
  return(quantile(cindexs, c(0.025, 0.975)))
}



timePR <- function (Time, Statu, marker, cause, times) {
  n <- length(Time)
  n_marker <- length(unique(marker))
  n_times <- length(times)
  if (n_times == 1) {
    times <- c(0, times)
    n_times <- 2
  }
  times <- times[order(times)]
  times_names <- paste("t=", times, sep = "")
  AUCPR <- rep(NA, n_times)
  names(AUCPR) <- times_names
  order_T <- order(Time)
  Time <- Time[order_T]
  Statu <- Statu[order_T]
  marker <- marker[order_T]
  weights <- pec::ipcw(
    Surv(failure_time, status) ~ 1, 
    data = data.frame(failure_time = Time, 
                      status = as.numeric(Statu != 0)), 
    method = "marginal", 
    times = times, 
    subjectTimes = Time, 
    subjectTimesLag = 1
  )
  order_marker <- order(-marker)
  Mat_data <- cbind(Time, Statu, marker)[order_marker, ]
  colnames(Mat_data) <- c("Time", "Statu", "marker")
  Weights_cases_all <- 1/(weights$IPCW.subjectTimes * n)
  Weights_cases_all <- Weights_cases_all[order_marker]
  Precision <- 
    matrix(NA, nrow = (n_marker + 1), ncol = n_times)
  TP <- matrix(NA, nrow = (n_marker + 1), ncol = n_times)
  colnames(Precision) <- times_names
  colnames(TP) <- times_names
  for (t in 1:n_times) {
    # t=2
    Cases <- 
      (Mat_data[, "Time"] < times[t] & Mat_data[, "Statu"] == cause)
    Controls_1 <- (Mat_data[, "Time"] > times[t])
    Weights_controls_1 <- 
      rep(1/(weights$IPCW.times[t] * n), times = n)
    Weights_controls_1 <- Weights_controls_1[order_marker]
    Weights_cases <- Weights_cases_all
    Weights_cases[!Cases] <- 0
    Weights_controls_1[!Controls_1] <- 0
    den_TP_t <- sum(Weights_cases)
    if (den_TP_t != 0) {
      TP_tbis <- c(0, cumsum(Weights_cases))/den_TP_t
      TP_t <- TP_tbis[!duplicated(marker[order_marker])]
      Precision_tbis <- 
        cumsum(Weights_cases)/(cumsum(Weights_cases) + cumsum(Weights_controls_1))
      Precision_t <- 
        c(1, Precision_tbis[!duplicated(marker[order_marker])])
    } 
    Precision_t[is.na(Precision_t)] <- 1
    Precision_t[TP_t == 0] <- 1
    AUCPR[t] <- mxjq_ia(TP_t, Precision_t)
    TP[, t] <- TP_t
    Precision[, t] <- Precision_t
  }
  prresult <- list(
    Recall = TP, 
    Precision = Precision, 
    AUC = AUCPR, 
    times = times, 
    weights = weights
  )
  return(prresult)
}

bootci_aupr <- function(data4pred, Time, Statu, marker, times){
  auprii <- matrix(nrow = 200, ncol = length(times))
  for (i in 1:200) {
    set.seed(i)
    samplei <- sample(
      1:nrow(data4pred), nrow(data4pred), replace = T
    )
    datai <- data4pred[samplei, ]
    aupri <- try(
      timePR(
        Time = datai[[Time]],
        Statu = datai[[Statu]],
        marker = marker[samplei], 
        cause = 1,
        times = times
      ),
      silent = T
    )
    if("try-error" %in% class(aupri)){
      aupri <- list()
      aupri$AUC <- NA
    }
    auprii[i,] <- aupri$AUC
  }
  auprci <- bind_rows(
    lapply(na.omit(data.frame(auprii)), 
           function(x)quantile(x, c(0.025, 0.975)))
  ) %>%
    as.data.frame()
  rownames(auprci) <- paste0("t=",times)
  return(auprci)
}

  
sadca <- function(predprob, data4pred, Time, Statu, 
                  model, dataset, timepoints) {
  dcaformula <- as.formula("Surv(time, status) ~ Model")
  dcaplot <- list()
  for (i in seq_along(timepoints)) {
    dcaplot[[i]] <- 
      data.frame(time = data4pred[[Time]],
                 status = data4pred[[Statu]],
                 Model = 1-predprob[[i]]) %>% 
      dcurves::dca(
        dcaformula, 
        data = .,
        time = timepoints[i], 
        label = list(Model = model)
      ) %>%
      plot(smooth = T) +
      scale_color_manual(values = c("black", "grey", "red")) +
      labs(title = paste(model, dataset, timepoints[i])) +
      theme(panel.grid = element_blank(), 
            legend.position = "inside",
            legend.justification = c(1,1),
            legend.background = element_blank(),
            legend.key = element_blank(), 
            text = element_text(family = "serif"))
  }
  names(dcaplot) <- paste(model, dataset, timepoints)
  return(dcaplot)
}

eval4sa <- function(
    predprob, data4pred, 
    Time, Statu, model, dataset, timepoints,
    calimethod, bw4nne, q4quantile, 
    groupno = 3, groupcut = NULL, groupname = c("L", "M", "H"), 
    color4times = c("red", "blue", "green")
  ) {
  
  if(!(is.null(groupno)) & is.null(groupcut)){
    groupcut <- 
      quantile(predprob$crank, seq(0, 1, length = groupno+1))
  }
  if(any(duplicated(groupcut))) {
    cat("预测风险集中，请自行设定风险分层界限")
    plot_risk <- NULL
  } else {
    predprob2 <- predprob %>%
      dplyr::mutate(risk = cut(crank, 
                               breaks = groupcut, 
                               labels = groupname,
                               include.lowest = T)) %>%
      dplyr::mutate(risk = as.factor(risk)) %>%
      as_tibble()
    riskfit <-  
      survfit(Surv(time, status) ~ risk, data = predprob2)
    plot_risk <- survminer::ggsurvplot(
      riskfit, 
      data = predprob2,
      pval=TRUE,
      pval.coord = c(0.1, 0.5),
      risk.table=TRUE,
      ggtheme = survminer::theme_survminer() +
        theme(text = element_text(family = "serif")),
      font.family = "serif"
    )
  }
  
  
  obj_roc <- timeROC::timeROC(
    T = data4pred[[Time]],
    delta = data4pred[[Statu]],
    marker = predprob[["crank"]],
    cause = 1,
    weighting = "marginal",
    times = timepoints,
    iid = TRUE
  )
  roc_auc_integrated <- 
    weighted.mean(obj_roc$AUC, 
                  obj_roc$weights$IPCW.times)
  roc_auc_integrated_l <- 
    weighted.mean(confint(obj_roc)$CB_AUC[,1],
                  obj_roc$weights$IPCW.times)
  roc_auc_integrated_u <- 
    weighted.mean(confint(obj_roc)$CB_AUC[,2], 
                  obj_roc$weights$IPCW.times)
  roc_auc <- data.frame(AUROC = obj_roc$AUC) %>%
    as.data.frame() %>%
    rownames_to_column("Time") %>%
    left_join(confint(obj_roc)$CB_AUC %>%
                as.data.frame() %>%
                rownames_to_column("Time"),
              by = "Time") %>%
    mutate(CI = paste0("(",sprintf("%0.3f", `2.5%`/100), "~",
                       sprintf("%0.3f", `97.5%`/100), ")")) %>%
    mutate(Label = paste0(Time, ", ROC=", sprintf("%0.3f", AUROC), CI)) %>%
    mutate(Model = model, Dataset = dataset) %>%
    select(Model, Dataset, Time, AUROC, CI, Label) %>%
    mutate(iAUROC = roc_auc_integrated,
           iAUROC_l = roc_auc_integrated_l/100,
           iAUROC_u = roc_auc_integrated_u/100)
  
  roc_tp <- obj_roc$TP %>%
    as.data.frame() %>%
    mutate(id = 1:n()) %>%
    pivot_longer(cols = -ncol(.), 
                 names_to = "Time", 
                 values_to = "Sensitivity")
  roc_fp <- obj_roc$FP %>%
    as.data.frame() %>%
    mutate(id = 1:n()) %>%
    pivot_longer(cols = -ncol(.), 
                 names_to = "Time", 
                 values_to = "FP")
  df_roc <- roc_tp %>%
    left_join(roc_fp, by = c("id", "Time")) %>%
    left_join(roc_auc, by = c("Time"))
  plot_roc <- df_roc %>%
    mutate(Label = forcats::as_factor(Label)) %>%
    ggplot(aes(x = FP, y = Sensitivity, 
               group = Label, color = Label)) +
    geom_line(linewidth = 1) +
    geom_abline(color = "grey") +
    scale_x_continuous(expand = c(0, 0), 
                       limits = c(0, 1),
                       breaks = seq(0, 1, by = 0.2)) +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0, 1),
                       breaks = seq(0, 1, by = 0.2)) +
    scale_color_manual(values = color4times) +
    labs(x = "1-Specificity", y = "Sensitivity", color = "") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.position = "inside",
          legend.justification = c(1,0),
          legend.background = element_blank(),
          legend.key = element_blank(), 
          text = element_text(family = "serif"))

  obj_pr <- timePR(
    Time = data4pred[[Time]],
    Statu = data4pred[[Statu]],
    marker = predprob[["crank"]],
    cause = 1,
    times = timepoints
  )
  pr_auc_integrated <- 
    weighted.mean(obj_pr$AUC, obj_pr$weights$IPCW.times)
  auprci <- bootci_aupr(
    data4pred = data4pred, 
    Time = Time,
    Statu = Statu,
    marker = predprob[["crank"]],
    times = timepoints
  )
  pr_auc_integrated_l <- 
    weighted.mean(auprci[,1], 
                  obj_pr$weights$IPCW.times)
  pr_auc_integrated_u <- 
    weighted.mean(auprci[,2], 
                  obj_pr$weights$IPCW.times)
  pr_auc <- data.frame(AUPR = obj_pr$AUC) %>%
    as.data.frame() %>%
    rownames_to_column("Time") %>%
    left_join(auprci %>%
                rownames_to_column("Time"),
              by = "Time") %>%
    mutate(CI = paste0("(",sprintf("%0.3f", `2.5%`), "~",
                       sprintf("%0.3f", `97.5%`), ")")) %>%
    mutate(Label = paste0(Time, ", AUPR=", sprintf("%0.3f", AUPR), CI)) %>%
    mutate(Model = model, Dataset = dataset) %>%
    select(Model, Dataset, Time, AUPR, CI, Label) %>%
    mutate(iAUPR = pr_auc_integrated,
           iAUPR_l = pr_auc_integrated_l,
           iAUPR_u = pr_auc_integrated_u)
  pr_recall <- obj_pr$Recall %>%
    as.data.frame() %>%
    mutate(id = 1:n()) %>%
    pivot_longer(cols = -ncol(.), 
                 names_to = "Time", 
                 values_to = "Recall")
  pr_precision <- obj_pr$Precision %>%
    as.data.frame() %>%
    mutate(id = 1:n()) %>%
    pivot_longer(cols = -ncol(.), 
                 names_to = "Time", 
                 values_to = "Precision")
  df_pr <- pr_recall %>%
    left_join(pr_precision, by = c("id", "Time")) %>%
    left_join(pr_auc, by = c("Time"))
  plot_pr <- df_pr %>%
    mutate(Label = forcats::as_factor(Label)) %>%
    ggplot(aes(x = Recall, y = Precision, 
               group = Label, color = Label)) +
    geom_line(linewidth = 1) +
    geom_abline(color = "grey", intercept = 1, slope = -1) +
    scale_x_continuous(expand = c(0, 0), 
                       limits = c(0, 1),
                       breaks = seq(0, 1, by = 0.2)) +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0, 1),
                       breaks = seq(0, 1, by = 0.2)) +
    scale_color_manual(values = color4times) +
    labs(x = "Recall", y = "Precision", color = "") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.position = "inside",
          legend.justification = c(0,0),
          legend.background = element_blank(),
          legend.key = element_blank(), 
          text = element_text(family = "serif"))
  
  obj_surv <- as.formula(
    paste0('Surv(', Time, ', ', Statu, ') ~ 1')
  )
  bsi <- list()
  df_calii <- list()
  for (i in seq_along(timepoints)) {
    datai <- data.frame(vi = 1-predprob[,i])
    colnames(datai) <- model
    set.seed(42)
    obj_score <- riskRegression::Score(
      as.list(datai),
      formula = obj_surv, 
      data = data4pred,
      metrics = c("brier"),
      summary = c("IPA"),
      plots = c("calibrate"),
      times = timepoints[i],
      conf.int = T
    )
    bsi[[i]] <- obj_score$Brier$score %>%
      as.data.frame() %>%
      filter(model != "Null model")
    obj_cali <- riskRegression::plotCalibration(
      obj_score,
      cens.method="local",
      times = timepoints[i],
      plot = F,
      method = calimethod,
      q = q4quantile
    )
    df_calii[[i]] <- obj_cali$plotFrames[[1]] %>%
      mutate(Time = paste0("t=", timepoints[i]))
  }
  bsitemp <- bind_rows(bsi)
  df_bs <- bsitemp %>%
    mutate(Time = paste0("t=", times)) %>%
    mutate(CI = paste0("(",sprintf("%0.3f", lower), "~",
                       sprintf("%0.3f", upper), ")")) %>%
    mutate(Label = paste0(Time, ", Brier=", sprintf("%0.3f", Brier), CI)) %>%
    mutate(Dataset = dataset) %>%
    select(Model = model, Dataset, Time, Brier, CI, Label)
  ibs <- weighted.mean(
    bsitemp$Brier,
    obj_roc$weights$IPCW.times
  )
  ibs_l <- weighted.mean(
    bsitemp$lower,
    obj_roc$weights$IPCW.times
  )
  ibs_u <- weighted.mean(
    bsitemp$upper,
    obj_roc$weights$IPCW.times
  )
  df_bs$iBS <- ibs
  df_bs$iBS_l <- ibs_l
  df_bs$iBS_u <- ibs_u
  df_cali <- bind_rows(df_calii) %>%
    left_join(df_bs, by = "Time")
  rownames(df_cali) <- NULL
  plot_calibration <- df_cali %>%
    mutate(Label = forcats::as_factor(Label)) %>%
    ggplot(aes(x = Pred, y = Obs, group = Label, color = Label)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3,
               alpha = ifelse(calimethod == "nne", 0, 1)) +
    geom_abline() +
    scale_x_continuous(expand = c(0, 0),
                       limits = c(0, 1),
                       breaks = seq(0, 1, by = 0.2)) +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0, 1),
                       breaks = seq(0, 1, by = 0.2)) +
    labs(x = "Predicted risk", 
         y = "Estimated actual risk", 
         color = "") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.position = "inside",
          legend.justification = c(1,0),
          legend.background = element_blank(),
          legend.key = element_blank(), 
          text = element_text(family = "serif"))
  
  plot_dca <- sadca(
    predprob = predprob,
    data4pred = data4pred,
    Time = Time,
    Statu = Statu,
    model = model,
    dataset = dataset,
    timepoints = timepoints
  )
  
  return(list(
    groupcut = groupcut,
    plot_risk = plot_risk,
    objroc = obj_roc,
    auroc = roc_auc,
    plotroc = plot_roc,
    dataroc = df_roc,
    aupr = pr_auc,
    plotpr = plot_pr,
    datapr = df_pr,
    brierscore = df_bs,
    plotcalibration = plot_calibration,
    datacalibration = df_cali,
    plotdca = plot_dca
  ))
}

viplot <- function(explainer, output_type = "risk"){
  if (output_type == "survival") {
    set.seed(42)
    vipdata <- survex::model_parts(
      explainer,
      type = "ratio",
      output_type = "survival",
      B = 5,
      N = 100
    )
    vipresult <- vipdata$result %>%
      rename_all(function(x){str_remove_all(x, "_")}) %>%
      filter(permutation == 0) %>%
      select(label, times, all_of(colnames(explainer$data)))
    rawplot <- plot(vipdata, max_vars = ncol(explainer$data)+1) +
      labs(subtitle = NULL) +
      scale_color_discrete(
        breaks = colnames(explainer$data),
        guide = guide_legend(title = NULL, ncol = 1)
      ) +
      theme(legend.position = "right",
            legend.text = element_text(size = 13),
            text = element_text(family = "serif"))
    diyplot <- vipresult %>%
      pivot_longer(cols = -c(1, 2)) %>%
      mutate(name = tidytext::reorder_within(name, value, times)) %>%
      ggplot(aes(x = value-1, y = name)) +
      geom_col(fill = "skyblue") +
      geom_vline(xintercept = 0, color = "black") +
      tidytext::scale_y_reordered() +
      labs(x = "Importance", y = "") +
      facet_wrap(~times,  scales = "free_y") +
      theme_minimal() +
      theme(text = element_text(family = "serif"),
            axis.text.y = element_text(size = 13),
            panel.grid.minor.x = element_blank())
  } else if (output_type == "risk") {
    set.seed(42)
    vipdata <- survex::model_parts(
      explainer,
      loss_function = survex::loss_one_minus_c_index,
      type = "ratio",
      output_type = "risk",
      B = 5,
      N = 100
    )
    vipresult <- as.data.frame(vipdata) %>%
      filter(variable %in% colnames(explainer$data)) %>%
      filter(permutation == 0) %>%
      select(-permutation)
    rawplot <- 
      plot(vipdata, 
           max_vars = ncol(explainer$data), 
           show_boxplots=F) +
      labs(subtitle = NULL) +
      theme(text = element_text(family = "serif"))
    diyplot <- vipresult %>%
      arrange(dropout_loss) %>%
      mutate(variable = forcats::as_factor(variable)) %>%
      ggplot(aes(x = dropout_loss-1, y = variable)) +
      geom_col(fill = "skyblue") +
      geom_vline(xintercept = 0, color = "black") +
      labs(x = "Importance", y = "") +
      theme_minimal() +
      theme(text = element_text(family = "serif"),
            axis.text.y = element_text(size = 13),
            panel.grid.minor.x = element_blank())
  }
  return(list(data = vipresult,
              rawplot = rawplot,
              diyplot = diyplot))
}

pdplot <- function(explainer, vars, output_type = "risk") {
  set.seed(42)
  partialdata <- survex::model_profile(
    explainer,
    variables = vars,
    type = "partial",
    output_type = output_type
  )
  rawplot <- plot(partialdata, facet_ncol = 2) +
    labs(subtitle = NULL) +
    patchwork::plot_annotation() &
    theme(text = element_text(family = "serif"))
  if (output_type == "risk") {
    pdpresult <- as.data.frame(partialdata$agr_profiles) %>%
      rename_all(function(x){str_remove_all(x, "_")}) %>%
      select(-ids)
    diyplot <- pdpresult %>%
      mutate(vname = forcats::as_factor(vname)) %>%
      ggplot(aes(x = x, y = yhat, color = vname, group = vname)) +
      geom_line(linewidth = 1, show.legend = F) +
      labs(x = "Feature Value", 
           y = "Average prediction",
           color = "Variable") +
      facet_wrap(~vname, scales = "free", ncol = 2) +
      theme_bw() +
      theme(panel.grid.minor = element_blank(),
            text = element_text(family = "serif", size = 15))
  } else {
    pdpresult <- partialdata$result %>%
      rename_all(function(x){str_remove_all(x, "_")}) %>%
      select(-ids)
    ylab <- ifelse(output_type == "survival",
                   "Survival Function Value",
                   "Chf Value")
    diyplot <- pdpresult %>%
      mutate(times = forcats::as_factor(times)) %>%
      ggplot(aes(x = x, y = yhat, color = times, group = times)) +
      geom_line(linewidth = 1) +
      labs(x = "Feature Value", 
           y = ylab,
           color = "Time") +
      facet_wrap(~vname, scales = "free", ncol = 2) +
      theme_bw() +
      theme(panel.grid.minor = element_blank(),
            text = element_text(family = "serif", size = 15))
  }
  return(list(data = pdpresult,
              rawplot = rawplot,
              diyplot = diyplot))
}

shap4one <- function(explainer, dataone, output_type) {
  if (output_type == "survival") {
    set.seed(42)
    shap41data <- survex::predict_parts(
      explainer, 
      new_observation = dataone,
      N = 100,
      output_type = "survival",
      type = "survshap"
    )
    datax <- dataone[rep(1,nrow(shap41data$result)),] %>%
      mutate(time = rownames(shap41data$result)) %>%
      select(time, everything()) %>%
      mutate_at(-1, as.character) %>%
      pivot_longer(-1, values_to = "featrue") %>%
      mutate(variable = paste(name, "=", featrue))
    contrib <- shap41data$result %>%
      rownames_to_column("time") %>%
      pivot_longer(cols = -1, values_to = "survshap") %>%
      left_join(datax, by = c("time", "name")) %>%
      select(time, variable, survshap)
    contrib01 <- mxjq_sct(contrib)
    rawplot <- plot(shap41data, max_vars = ncol(dataone)) +
      labs(subtitle = NULL) +
      scale_color_discrete(
        guide = guide_legend(title = NULL, ncol = 1)
      ) +
      theme(legend.position = "right",
            legend.text = element_text(size = 13),
            text = element_text(family = "serif"))
    diyplot <- bind_rows(contrib01) %>%
      mutate(time = forcats::as_factor(time)) %>%
      ggplot(aes(xmin = start, 
                 xmax = end, 
                 y = 1, 
                 label = shapvalue,
                 fill = as.factor(am),
                 forward = am)) +
      ggrepel::geom_text_repel(
        aes(x = xat, y = 1, label = variable),
        force_pull = 0, 
        nudge_y = 0.04
      ) +
      geom_segment(x = 0,   xend = 0, 
                   y = 0.95,yend = 1.1,
                   linetype = "dashed") +
      gggenes::geom_gene_arrow(
        arrow_body_height = unit(10, "mm"),
        arrowhead_height = unit(10, "mm"), 
        arrowhead_width = unit(3, "mm")
      ) +
      gggenes::geom_gene_label(height = grid::unit(5, "mm")) +
      facet_wrap(~ time, ncol = 1, scales = "free") +
      scale_y_continuous(expand = c(0,0),
                         limits = c(0.95, 1.08),
                         breaks = 1, labels = "") +
      labs(x = "SurvSHAP", y = "") +
      theme(
        legend.position = "none",
        panel.background = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.line.x = element_line(colour = "grey20", 
                                   linewidth = 0.5), 
        axis.ticks.x = element_line(colour = "grey20",
                                    linewidth = 0.5),
        strip.text = element_text(size = 15, hjust = 0),
        strip.background = element_blank()
      )
  } else if (output_type == "risk") {
    set.seed(42)
    shap41data <- survex::predict_parts(
      explainer, 
      new_observation = dataone,
      N = 100,
      B = 10,
      output_type = "risk",
      type = "shap",
      calculation_method = "kernelshap"
    )
    contrib <- shap41data %>%
      as.data.frame() %>%
      filter(B == 0) %>%
      select(variable, contribution)
    contrib01 <- mxjq_sc(contrib)
    rawplot <- plot(shap41data, 
         show_boxplots = F,
         max_vars = ncol(dataone)) +
      theme(legend.position = "none",
            text = element_text(family = "serif"))
    diyplot <- ggplot(contrib01, 
                      aes(xmin = start, 
                          xmax = end, 
                          y = 1, 
                          label = shapvalue,
                          fill = as.factor(am),
                          forward = am)) +
      ggrepel::geom_text_repel(
        aes(x = xat, y = 1, label = variable),
        force_pull = 0, 
        nudge_y = 0.04
      ) +
      geom_segment(x = 0,    xend = 0,
                   y = 0.95, yend = 1.1,
                   linetype = "dashed") +
      gggenes::geom_gene_arrow(
        arrow_body_height = unit(8, "mm"),
        arrowhead_height = unit(8, "mm"), 
        arrowhead_width = unit(3, "mm")
      ) +
      gggenes::geom_gene_label(
        height = grid::unit(5, "mm")
      ) +
      scale_y_continuous(expand = c(0,0),
                         limits = c(0.95, 1.08),
                         breaks = 1, labels = "") +
      labs(x = "Contribution", y = "") +
      gggenes::theme_genes() +
      theme(legend.position = "none")
  }
  return(list(data = contrib,
              rawplot = rawplot,
              diyplot = diyplot))
}

func4map <- function(i, iexplainer, idatax, idatay){
  set.seed(42)
  temp <- survex::predict_parts(
    iexplainer,
    new_observation = idatax[i,],
    y_true = idatay[i],
    N = 100
  )
  return(list(x = temp$variable_values,
              timeshap = temp$result,
              aggrshap = temp$aggregate))
}

sumshap <- function(explainer, datax, datay, sampleN=200){
  if(is.null(sampleN)){
    sumi <- 1:nrow(datax)
  } else if (sampleN >= nrow(datax)){
    sumi <- 1:nrow(datax)
  } else {
    set.seed(42)
    sumi <- sample(1:nrow(datax), sampleN)
  }
  temp <- furrr::future_map(
    sumi, 
    func4map, 
    iexplainer = explainer, 
    idatax = datax, 
    idatay = datay,
    .options = furrr::furrr_options(seed = 42),
    .progress = T
  )
  datax <- bind_rows(lapply(temp, "[[", "x"), .id = "id")
  timeshap <- bind_rows(
    lapply(temp, "[[", "timeshap"), .id = "id"
  ) %>%
    rownames_to_column("time") %>%
    mutate(time = str_extract(time, "^([^...]+)"))
  aggrshap <- 
    bind_rows(lapply(temp, "[[", "aggrshap"), .id = "id")
  return(list(datax = datax,
              timeshap = timeshap,
              aggrshap = aggrshap))
}

sumshap2 <- function(explainer, datax, datay, sampleN=200){
  if(is.null(sampleN)){
    sumi <- 1:nrow(datax)
  } else if (sampleN >= nrow(datax)){
    sumi <- 1:nrow(datax)
  } else {
    set.seed(42)
    sumi <- sample(1:nrow(datax), sampleN)
  }
  jdt <- progress::progress_bar$new(total = sampleN)
  temp <- list()
  for (i in seq_along(sumi)) {
    ii <- sumi[i]
    temp[[i]] <- func4map(ii, explainer, datax, datay)
    jdt$tick()
  }
  datax <- bind_rows(lapply(temp, "[[", "x"), .id = "id")
  timeshap <- bind_rows(
    lapply(temp, "[[", "timeshap"), .id = "id"
  ) %>%
    rownames_to_column("time") %>%
    mutate(time = str_extract(time, "^([^...]+)"))
  aggrshap <- 
    bind_rows(lapply(temp, "[[", "aggrshap"), .id = "id")
  return(list(datax = datax,
              timeshap = timeshap,
              aggrshap = aggrshap))
}

func4timevip <- function(sumshap){
  temp <- sumshap$timeshap %>%
    pivot_longer(-c(1,2), names_to = "x", values_to = "shap") %>%
    group_by(time, x) %>%
    summarise(importance = mean(abs(shap))) %>%
    mutate(time = as.numeric(str_sub(time, start = 3)))
  return(temp)
}

# shapvip <- function(sumshap){
#   library(patchwork)
#   aggrshapimp <- 
#     apply(sumshap$aggrshap[,-1], 2, function(x){mean(abs(x))})
#   aggrshapimp <- sort(aggrshapimp)
#   aggrvip <- data.frame(importance = aggrshapimp) %>%
#     rownames_to_column("x") %>%
#     mutate(x = forcats::as_factor(x))
#   timevip <- func4timevip(sumshap) %>%
#     mutate(x = factor(x, names(aggrshapimp)))
#   vip1 <- aggrvip %>%
#     ggplot(aes(x = importance, y = x, fill = x)) +
#     geom_col(show.legend = F) +
#     labs(x = "Average |aggregated SurvSHAP(t)|",
#          y = "") +
#     theme_minimal() +
#     theme(panel.grid.minor.x = element_blank())
#   vip2 <- timevip %>%
#     ggplot(aes(x = time, y = importance, color = x, group = x)) +
#     geom_line(linewidth = 1, show.legend = F) +
#     geom_point(size = 2, show.legend = F) +
#     labs(x = "Time", y = "Average |SurvSHAP(t)|", color = "") +
#     theme_minimal() +
#     theme(panel.grid.minor = element_blank())
#   vip1+vip2
# }

shapvip <- function(sumshap){
  library(patchwork)
  library(tibble)
  library(ggplot2)
  library(dplyr)
  
  aggrshapimp <- 
    apply(sumshap$aggrshap[,-1], 2, function(x){mean(abs(x))})
  aggrshapimp <- sort(aggrshapimp)
  
  n_features <- length(aggrshapimp)
  
  # 使用高对比度的彩虹色方案 - 增强颜色对比
color_vector <- rainbow(n_features, s = 0.8, v = 0.9, start = 0.1, end = 0.9)
color_vector <- c( "#000075",  "#808080", "#FFE119", "#4363D8", "#F58231",
                    "#911EB4", "#46F0F0", "#F032E6", "#BCF60C", "#FABEBE",
                    "#008080", "#E6BEFF", "#9A6324", "#FFFAC8", "#800000",
                    "#AAFFC3", "#808000", "#FFD8B1","#E6194B","#3CB44B")
  
  # 或者使用更鲜艳的色盲友好方案（二选一）
  # color_vector <- viridisLite::turbo(n_features, direction = -1)
  
  aggrvip <- data.frame(importance = aggrshapimp) %>%
    rownames_to_column("x") %>%
    mutate(x = factor(x, levels = names(aggrshapimp)))
  
  timevip <- func4timevip(sumshap) %>%
    mutate(x = factor(x, levels = names(aggrshapimp)))
  
  vip1 <- aggrvip %>%
    ggplot(aes(x = importance, y = x, fill = x)) +
    geom_col(show.legend = F) +
    scale_fill_manual(values = color_vector) +
    labs(x = "Average |aggregated SurvSHAP(t)|", y = "") +
    theme_minimal() +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank())
  
  vip2 <- timevip %>%
    ggplot(aes(x = time, y = importance, color = x, group = x)) +
    geom_line(linewidth = 1.2, show.legend = F) +  # 加粗线条
    geom_point(size = 2.5, show.legend = F) +       # 增大点大小
    scale_color_manual(values = color_vector) +
    labs(x = "Time", y = "Average |SurvSHAP(t)|", color = "") +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "grey90"))
  
  vip1 + vip2
}




aggrshap_bs <- function(sumshap){
  datax2 <- data.matrix(sumshap$datax) %>%
    as.data.frame() %>%
    pivot_longer(cols = -1, names_to = "x", values_to = "feature")
  aggrshap2 <- sumshap$aggrshap %>%
    mutate(id = as.numeric(id)) %>%
    pivot_longer(cols = -1, names_to = "x", values_to = "shap")
  aggrshapimp <- 
    apply(sumshap$aggrshap[,-1], 2, function(x){mean(abs(x))})
  aggrshapimp <- sort(aggrshapimp)
  datax2 %>%
    left_join(aggrshap2, by = c("id", "x")) %>%
    group_by(x) %>%
    mutate(feature2 = mxjq_r01(feature)) %>%
    ungroup() %>%
    mutate(x = factor(x, names(aggrshapimp))) %>%
    ggplot(aes(x = shap, y = x, color = feature2)) +
    ggbeeswarm::geom_quasirandom(width = 0.1) +
    geom_vline(xintercept = 0) +
    scale_color_gradient2(
      low = "red",
      mid = "darkmagenta",
      high = "dodgerblue",
      midpoint = 0.5,
      breaks = c(0, 1),
      labels = c("Low", "High"),
      guide = guide_colorbar(barwidth = 0.5,
                             barheight = 15,
                             title.position = "right",
                             title.hjust = 0.5)
    ) +
    labs(x = "Aggregated SurvSHAP(t)",
         y = "", 
         color = "Feature value") +
    theme_minimal() +
    theme(legend.title = element_text(angle = -90))
}


timeshap_bs <- function(sumshap){
  datax2 <- data.matrix(sumshap$datax) %>%
    as.data.frame() %>%
    pivot_longer(cols = -1, names_to = "x", values_to = "feature")
  timeshap2 <- sumshap$timeshap %>%
    mutate(id = as.numeric(id)) %>%
    pivot_longer(-c(1,2), names_to = "x", values_to = "shap")
  timevip <- func4timevip(sumshap)
  datax2 %>%
    full_join(timeshap2, by = c("id", "x")) %>%
    group_by(x, time) %>%
    mutate(feature2 = mxjq_r01(feature)) %>%
    ungroup() %>%
    mutate(time = as.numeric(str_sub(time, start = 3))) %>%
    left_join(timevip, by = c("time", "x")) %>%
    mutate(x = tidytext::reorder_within(x, importance, time),
           time = forcats::as_factor(time)) %>%
    ggplot(aes(x = shap, y = x, color = feature2)) +
    ggbeeswarm::geom_quasirandom(width = 0.1) +
    geom_vline(xintercept = 0) +
    scale_color_gradient2(
      low = "red",
      mid = "darkmagenta",
      high = "dodgerblue",
      midpoint = 0.5,
      breaks = c(0, 1),
      labels = c("Low", "High"),
      guide = guide_colorbar(barwidth = 0.5,
                             barheight = 15,
                             title.position = "right",
                             title.hjust = 0.5)
    ) +
    facet_wrap(~time, scales = "free") +
    tidytext::scale_y_reordered() +
    labs(x = "SurvSHAP",
         y = "", 
         color = "Feature value") +
    theme_minimal() +
    theme(legend.title = element_text(angle = -90))
}


aggrshap_dp <- function(sumshap, xi){
  datai <- sumshap$datax %>%
    mutate(id=as.numeric(id)) %>%
    select(id, feature = all_of(xi))
  aggrshap2 <- sumshap$aggrshap %>%
    mutate(id = as.numeric(id)) %>%
    pivot_longer(cols = -1, names_to = "x", values_to = "shap")
  if (is.factor(datai$feature)) {
    datai %>%
      left_join(filter(aggrshap2, x == xi), by = "id") %>%
      ggplot(aes(x = feature, y = shap)) +
      geom_boxplot(aes(fill = feature), width=0.25, show.legend = F) +
      # geom_point() +
      labs(x = xi, y = "Aggregated SurvSHAP(t)") +
      theme_minimal() 
  } else {
    datai %>%
      left_join(filter(aggrshap2, x == xi), by = "id") %>%
      ggplot(aes(x = feature, y = shap)) +
      geom_point() +
      geom_smooth(se = F) +
      labs(x = xi, y = "Aggregated SurvSHAP(t)") +
      theme_minimal() 
  }
}

timeshap_dp <- function(sumshap, xi){
  datai <- sumshap$datax %>%
    mutate(id=as.numeric(id)) %>%
    select(id, feature = all_of(xi))
  timeshap2 <- sumshap$timeshap %>%
    mutate(id = as.numeric(id)) %>%
    pivot_longer(-c(1,2), names_to = "x", values_to = "shap")
  if (is.factor(datai$feature)) {
    datai %>%
      left_join(filter(timeshap2, x == xi), by = "id") %>%
      mutate(time = forcats::as_factor(time)) %>%
      ggplot(aes(x = feature, y = shap)) +
      geom_boxplot(aes(fill = feature), width=0.2, show.legend = F) +
      # geom_point() +
      facet_wrap(~time) +
      labs(x = xi, y = "SurvSHAP") +
      theme_minimal() 
  } else {
    datai %>%
      left_join(filter(timeshap2, x == xi), by = "id") %>%
      mutate(time = forcats::as_factor(time)) %>%
      ggplot(aes(x = feature, y = shap)) +
      geom_point() +
      geom_smooth(se = F) +
      facet_wrap(~time) +
      labs(x = xi, y = "SurvSHAP") +
      theme_minimal()
  }
}

##############################################################

stack_train <- 
  function(base_learner, stack_learner, itask, iresample, withraw){
    
    itask_data <- itask$data() %>%
      as.data.frame() %>%
      mutate(row_ids = itask$row_ids)
    
    base_learner2 <- base_learner
    base_rr_result <- list()
    for (i in seq_along(base_learner)) {
      ibase <- base_learner[[i]]
      ibase2 <- base_learner[[i]]
      imodel <- names(base_learner)[i]
      
      set.seed(42)
      irr <- resample(
        task = itask,
        learner = ibase,
        resampling = iresample
      )
      irr_result <- as.data.table(irr$prediction()) %>%
        dtplyr::lazy_dt() %>%
        select(-distr) %>%
        mutate(model = paste0("crank.", imodel)) %>%
        as.data.frame()
      remove(irr)
      gc(verbose=F)
      
      base_rr_result[[i]] <- irr_result
      
      remove(irr_result)
      gc(verbose=F)
      
      ibase2$train(itask)
      base_learner2[[i]] <- ibase2
    }
    base_rr_result2 <- bind_rows(base_rr_result) %>%
      pivot_wider(names_from = model, values_from = crank) %>%
      select(-time, -status) %>%
      left_join(itask_data, by = "row_ids") %>%
      select(-row_ids)
    remove(base_rr_result)
    gc(verbose=F)
    
    if(!(withraw)){
      base_rr_result2 <- base_rr_result2 %>%
        select(-all_of(itask$feature_names))
    }
    
    itask_stack <- as_task_surv(
      base_rr_result2, 
      time = itask$target_names[1],
      event = itask$target_names[2], 
      type = "right"
    )
    remove(base_rr_result2)
    gc(verbose=F)
    
    set.seed(42)
    stack_learner$train(itask_stack)
    
    return(list(base_trained = base_learner2,
                stack_trained = stack_learner))
  }

stack_predict <- 
  function(trainedstack, newdata, itask, withraw){
    
    newdata$row_ids <- 1:nrow(newdata)
    base_learner <- trainedstack$base_trained
    stack_learner <- trainedstack$stack_trained
    
    base_pred_result <- list()
    for (i in seq_along(trainedstack$base_trained)) {
      ibase <- base_learner[[i]]
      imodel <- names(base_learner)[i]
      
      ibase_pred <- ibase$predict_newdata(newdata)
      ibase_pred_result <- as.data.table(ibase_pred) %>%
        dtplyr::lazy_dt() %>%
        select(-distr) %>%
        mutate(model = paste0("crank.", imodel)) %>%
        as.data.frame()
      remove(ibase_pred)
      gc(verbose=F)

      base_pred_result[[i]] <- ibase_pred_result
      remove(ibase_pred_result)
      gc(verbose=F)
    }
    
    base_pred_result2 <- bind_rows(base_pred_result) %>%
      pivot_wider(names_from = model, values_from = crank) %>%
      select(-time, -status) %>%
      left_join(newdata, by = "row_ids") %>%
      select(-row_ids)
    
    if(!(withraw)){
      base_pred_result2 <- base_pred_result2 %>%
        select(-all_of(itask$feature_names))
    }
    
    stack_pred <- stack_learner$predict_newdata(base_pred_result2)
    remove(base_pred_result2)
    gc(verbose=F)
    
    return(stack_pred)
  }

########################################

risk_pred <- function(model, newdata){
  model$predict_newdata(newdata)$crank
} 
surv_pred <- function(model, newdata, times){
  t(model$predict_newdata(newdata)$distr$survival(times))
} 
chf_pred <- function(model, newdata, times){
  t(model$predict_newdata(newdata)$distr$cumHazard(times))
} 

########################################

risk_pred2 <- function(model, newdata){
  model$predict_newdata(newdata)$crank
} 
surv_pred2 <- function(model, newdata, times){
  ers <- (exp(model$predict_newdata(newdata)$crank))
  km0 <- survfit(traindatay ~ 1)
  km01 <- 
    sapply(times, function(x){which.max(km0$time[km0$time<x])})
  km02 <- km0$surv[km01]
  si <- list()
  for (i in seq_along(km02)) {
    si[[i]] <- km02[i] ^ ers
  }
  names(si) <- times
  return(bind_cols(si))
} 

chf_pred2 <- function(model, newdata, times) {
  survex::survival_to_cumulative_hazard(
    surv_pred2(model, newdata, times)
  )
}

########################################

risk_pred3 <- function(model, newdata){
  ri <- predict(
    model,
    newdata %>% select(sort(colnames(newdata))),
    type = "risk"
  )
  return(ri)
}

surv_pred3 <- function(model, newdata, times){
  si <- predict(
    model,
    newdata %>% select(sort(colnames(newdata)))
  )
  sitimes <- as.numeric(colnames(si))
  timepointswhere <- 
    sapply(times, function(x){which.max(sitimes[sitimes<x])})
  si2 <- matrix(si[,timepointswhere], nrow = nrow(newdata))
  colnames(si2) <- times
  return(si2)
} 

chf_pred3 <- function(model, newdata, times) {
  survex::survival_to_cumulative_hazard(
    surv_pred3(model, newdata, times)
  )
}

#################################################

risk_pred4 <- function(model, newdata){
  if (nrow(newdata)==1) {
    newdata2 <- rbind(newdata, newdata)
    crank <- model$predict_newdata(newdata2)$crank[1]
  } else {
    crank <- model$predict_newdata(newdata)$crank
  }
  return(crank)
} 
surv_pred4 <- function(model, newdata, times){
  ers <- (exp(risk_pred4(model, newdata)))
  km0 <- survfit(traindatay ~ 1)
  km01 <- 
    sapply(times, function(x){which.max(km0$time[km0$time<x])})
  km02 <- km0$surv[km01]
  si <- list()
  for (i in seq_along(km02)) {
    si[[i]] <- km02[i] ^ ers
  }
  names(si) <- times
  return(bind_cols(si))
} 

chf_pred4 <- function(model, newdata, times) {
  survex::survival_to_cumulative_hazard(
    surv_pred4(model, newdata, times)
  )
}

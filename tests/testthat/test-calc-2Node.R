test_that("Test calc2Node function", {
  reference_tables <- retrieve_data(url_config$test_two_node_url)

  tolerance <- reference_tables$tolerance
  data_list <- reference_tables$data

  total_cases <- nrow(data_list)

  for (i in seq_len(total_cases)) {
    inputs <- as.list(data_list$inputs[i, ])
    outputs <- as.list(data_list$outputs[i, ])

    # Skip test case if 'execute_in_R' is set to FALSE
    if (!is.null(data_list$execute_in_R[i]) && !is.na(data_list$execute_in_R[i]) && data_list$execute_in_R[i] == FALSE) {
      skip(paste("Skipping test case", i, "due to 'execute_in_R' being FALSE"))
    }

    ta <- as.numeric(inputs$tdb)
    tr <- as.numeric(inputs$tr)
    vel <- as.numeric(inputs$v)
    rh <- as.numeric(inputs$rh)
    clo <- as.numeric(inputs$clo)
    met <- as.numeric(inputs$met)

    result <- calc2Node(
      ta = ta,
      tr = tr,
      vel = vel,
      rh = rh,
      clo = clo,
      met = met
    )

    check_tolerance(
      result$disc, as.numeric(outputs$disc), tolerance$disc,
      paste(
        "Failed at data row", i, ": disc tolerance check. Inputs:",
        "tdb =", ta, ", tr =", tr, ", v =", vel,
        ", rh =", rh, ", clo =", clo, ", met =", met,
        "Expected disc =", as.numeric(outputs$disc),
        "Actual disc =", result$disc
      )
    )

    check_tolerance(
      result$pmvg, as.numeric(outputs$pmv_gagge), tolerance$pmv_gagge,
      paste(
        "Failed at data row", i, ": pmv_gagge tolerance check. Inputs:",
        "tdb =", ta, ", tr =", tr, ", v =", vel,
        ", rh =", rh, ", clo =", clo, ", met =", met,
        "Expected pmv_gagge =", as.numeric(outputs$pmv_gagge),
        "Actual pmv_gagge =", result$pmvg
      )
    )

    check_tolerance(
      result$pmvstar, as.numeric(outputs$pmv_set), tolerance$pmv_set,
      paste(
        "Failed at data row", i, ": pmv_set tolerance check. Inputs:",
        "tdb =", ta, ", tr =", tr, ", v =", vel,
        ", rh =", rh, ", clo =", clo, ", met =", met,
        "Expected pmv_set =", as.numeric(outputs$pmv_set),
        "Actual pmv_set =", result$pmvstar
      )
    )

    check_tolerance(
      result$tcr, as.numeric(outputs$t_core), tolerance$t_core,
      paste(
        "Failed at data row", i, ": t_core tolerance check. Inputs:",
        "tdb =", ta, ", tr =", tr, ", v =", vel,
        ", rh =", rh, ", clo =", clo, ", met =", met,
        "Expected t_core =", as.numeric(outputs$t_core),
        "Actual t_core =", result$tcr
      )
    )

    check_tolerance(
      result$esk, as.numeric(outputs$e_skin), tolerance$e_skin,
      paste(
        "Failed at data row", i, ": e_skin tolerance check. Inputs:",
        "tdb =", ta, ", tr =", tr, ", v =", vel,
        ", rh =", rh, ", clo =", clo, ", met =", met,
        "Expected e_skin =", as.numeric(outputs$e_skin),
        "Actual e_skin =", result$esk
      )
    )

    check_tolerance(
      result$wet, as.numeric(outputs$w), 0.1,
      paste(
        "Failed at data row", i, ": w tolerance check. Inputs:",
        "tdb =", ta, ", tr =", tr, ", v =", vel,
        ", rh =", rh, ", clo =", clo, ", met =", met,
        "Expected w =", as.numeric(outputs$w),
        "Actual w =", result$wet
      )
    )

    check_tolerance(
      result$regsw, as.numeric(outputs$m_rsw), tolerance$m_rsw,
      paste(
        "Failed at data row", i, ": m_rsw tolerance check. Inputs:",
        "tdb =", ta, ", tr =", tr, ", v =", vel,
        ", rh =", rh, ", clo =", clo, ", met =", met,
        "Expected m_rsw =", as.numeric(outputs$m_rsw),
        "Actual m_rsw =", result$regsw
      )
    )
  }
})

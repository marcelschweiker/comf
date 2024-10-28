library(testthat)
source("../config.R")
source("../utils-test-tool.R")

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
      print(paste("Skipping test case", i, "due to 'execute_in_R' being FALSE"))
      next
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
    
    expected_disc <- as.numeric(outputs$disc)
    expected_pmv_gagge <- as.numeric(outputs$pmv_gagge)
    expected_pmv_set <- as.numeric(outputs$pmv_set)
    expected_tcore <- as.numeric(outputs$t_core)
    expected_esk <- as.numeric(outputs$e_skin)
    expected_w <- as.numeric(outputs$w)
    expected_m_rsw <- as.numeric(outputs$m_rsw)



    # Verify disc, using tolerance for comparison
    if (!is.null(expected_disc) && !is.na(expected_disc)) {
      expect_true(
        abs(result$disc - expected_disc) < tolerance$disc,
        info = paste("Failed at data row", i, ": disc tolerance check. Inputs:", 
                     "tdb =", ta, "tr =", tr, "v =", vel,
                     "rh =", rh, "clo =", clo, "met =", met,
                     "Expected disc =", expected_disc, 
                     "Actual disc =", result$disc)
      )
    }
    
    # Verify pmv_gagge, using tolerance for comparison
    if (!is.null(expected_pmv_gagge) && !is.na(expected_pmv_gagge)) {
      expect_true(
        abs(result$pmvg - expected_pmv_gagge) < tolerance$pmv_gagge, 
        info = paste("Failed at data row", i, ": pmv_gagge tolerance check. Inputs:", 
                     "tdb =", ta, "tr =", tr, "v =", vel,
                     "rh =", rh, "clo =", clo, "met =", met,
                     "Expected pmv_gagge =", expected_pmv_gagge, 
                     "tolerance pmv_gagge = ", tolerance$pmv_gagge,
                     "Actual pmv_gagge =", result$pmvg)
      )
    }
    
    # Verify pmv_set, using tolerance for comparison
    if (!is.null(expected_pmv_set) && !is.na(expected_pmv_set)) {
      expect_true(
        abs(result$pmvstar - expected_pmv_set) < tolerance$pmv_set,
        info = paste("Failed at data row", i, ": pmv_set tolerance check. Inputs:", 
                     "tdb =", ta, "tr =", tr, "v =", vel,
                     "rh =", rh, "clo =", clo, "met =", met,
                     "Expected pmv_set =", expected_pmv_set, 
                     "Actual pmv_set =", result$pmvstar)
      )
    }

    # Verify t_core, using tolerance t_core
    if (!is.null(expected_tcore)) {
      expect_true(
        abs(result$tcr - expected_tcore) < tolerance$t_core,
        info = paste("Failed at data row", i, ": t_core tolerance check. Inputs:", 
                     "tdb =", ta, "tr =", tr, "v =", vel,
                     "rh =", rh, "clo =", clo, "met =", met,
                     "Expected t_core =", expected_tcore, 
                     "Actual t_core =", result$tcr)
      )
    }
    
    # Verify e_skin, using tolerance e_skin
    if (!is.null(expected_esk)) {
      expect_true(
        abs(result$esk - expected_esk) < tolerance$e_skin,
        info = paste("Failed at data row", i, ": e_skin tolerance check. Inputs:", 
                     "tdb =", ta, "tr =", tr, "v =", vel,
                     "rh =", rh, "clo =", clo, "met =", met,
                     "Expected e_skin =", expected_esk, 
                     "Actual e_skin =", result$esk)
      )
    }
    
    # Verify w, using tolerance w
    if (!is.null(expected_w)) {
      expect_true(
        abs(result$wet - expected_w) < tolerance$w,
        info = paste("Failed at data row", i, ": w tolerance check. Inputs:", 
                     "tdb =", ta, "tr =", tr, "v =", vel,
                     "rh =", rh, "clo =", clo, "met =", met,
                     "Expected w =", expected_w, 
                     "Actual w =", result$wet)
      )
    }

    # Verify m_rsw, using tolerance m_rsw
    if (!is.null(expected_m_rsw)) {
      expect_true(
        abs(result$regsw - expected_m_rsw) < tolerance$m_rsw,
        info = paste("Failed at data row", i, ": m_rsw tolerance check. Inputs:", 
                     "tdb =", ta, "tr =", tr, "v =", vel,
                     "rh =", rh, "clo =", clo, "met =", met,
                     "Expected m_rsw =", expected_m_rsw, 
                     "Actual m_rsw =", result$regsw)
      )
    }
  }
})

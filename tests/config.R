# Define global URLs and any other configurations
unit_test_data_prefix <- "https://raw.githubusercontent.com/TwinGan/validation-data-comfort-models/release_v1.0/"

url_config <- list(
  test_adaptive_en_url = paste0(unit_test_data_prefix, "ts_adaptive_en.json"),
  test_solar_gain_url = paste0(unit_test_data_prefix, "ts_solar_gain.json"),
  test_wbgt_url = paste0(unit_test_data_prefix, "ts_wbgt.json"),
  test_ankle_draft_url = paste0(unit_test_data_prefix, "ts_ankle_draft.json"),
  test_vtg_url = paste0(unit_test_data_prefix, "ts_vertical_tmp_grad_ppd.json"),
  test_humidex_url = paste0(unit_test_data_prefix, "ts_humidex.json"),
  test_pmv_ppd_url = paste0(unit_test_data_prefix, "ts_pmv_ppd.json"),
  test_set_url = paste0(unit_test_data_prefix, "ts_set.json"),
  test_e_pmv_url = paste0(unit_test_data_prefix, "ts_e_pmv.json"),
  test_utci_url = paste0(unit_test_data_prefix, "ts_utci.json"),
  test_athb_url = paste0(unit_test_data_prefix, "ts_athb.json"),
  test_two_node_url = paste0(unit_test_data_prefix, "ts_two_nodes.json"),
  test_cooling_effect_url = paste0(unit_test_data_prefix, "ts_cooling_effect.json"),
  test_discomfort_index_url=paste0(unit_test_data_prefix, "ts_discomfort_index.json"),
  test_pet_steady_url = paste0(unit_test_data_prefix, "ts_pet_steady.json"),
  test_wind_chill_index_url = paste0(unit_test_data_prefix, "ts_wind_chill.json"),
  test_at_url = paste0(unit_test_data_prefix, "ts_at.json")
)

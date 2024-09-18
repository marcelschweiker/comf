# Define global URLs and any other configurations
unit_test_data_prefix <- "https://raw.githubusercontent.com/TwinGan/validation-data-comfort-models/release_v1.0/"

url_config <- list(
  test_adaptive_en_url = paste0(unit_test_data_prefix, "ts_adaptive_en.json"),
  test_solar_gain_url = paste0(unit_test_data_prefix, "ts_solar_gain.json"),
  test_wbgt_url = paste0(unit_test_data_prefix, "ts_wbgt.json"),
  test_ankle_draft_url = paste0(unit_test_data_prefix, "ts_ankle_draft.json"),
  test_vtg_url = paste0(unit_test_data_prefix, "ts_vertical_tmp_grad_ppd.json"),
  test_humidex_url = paste0(unit_test_data_prefix, "ts_humidex.json")
  test_heat_index_url = paste0(unit_test_data_prefix, "ts_heat_index.json")
  test_clo_tout_url = paste0(unit_test_data_prefix, "ts_clo_tout.json")
)

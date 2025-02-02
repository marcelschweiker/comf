# comfort_R
Repository for the R-package comf: Functions for Thermal Comfort Research

Current version of master branch: comf_0.1.12

Feel free to propose changes/additions or to get in contact...

# Contribute to the project

If you want to contribute to the project, you can do so by following these steps:

1. Fork the repository
2. Make changes to the forked repository
3. Reformate the code using the `styler` package using `styler::style_dir()`
4. Write tests for the changes
5. Submit a pull request

# Test Script Documentation

We are using the `testthat` package to run tests on the functions in the `comf` package.

To run the test locally follow these steps:

1. Clone the repository
2. Open the R console in your terminal using the command `R`
3. Run the following commands in the R console `devtools::test()` to run all the tests
4. To run a specific test, use the command `devtools::test_active_file("tests/testthat/test-calc-AD.R")`

The `helper-test-script.R` script is designed to leverage the `devtools::test()` function for running tests across various functions. These tests utilize test cases from the [validation-data-comfort-models](https://github.com/FedericoTartarini/validation-data-comfort-models) repository.


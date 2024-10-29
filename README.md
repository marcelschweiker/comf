# comfort_R
Repository for the R-package comf: Functions for Thermal Comfort Research

Current version of master branch: comf_0.1.12

Feel free to propose changes/additions or to get in contact...

# Test Script Documentation

## Overview
The `helper-test-script.R` script is designed to leverage the `devtools::test()` function for running tests across various functions. These tests utilize test cases from the [validation-data-comfort-models](https://github.com/FedericoTartarini/validation-data-comfort-models) repository.

## Test Execution
Tests are executed automatically by the script, which assesses each function systematically. The test results will be printed on the terminal in Markdown format and add to the readme file.

## Results
Below is the latest test result

========test result========
|Test                |Result |
|:-------------------|:------|
|Context             |PASSED |
|calc2Node           |FAILED |
|calcAD              |PASSED |
|calcAT              |PASSED |
|calcATHBstrandard   |FAILED |
|calcCE [1.1s]       |FAILED |
|calcCloTout         |PASSED |
|calcDiscomfortIndex |PASSED |
|calcePMV            |PASSED |
|calcHeatIndex       |PASSED |
|calcHumidity        |FAILED |
|calcPetSteady       |PASSED |
|calcPhs             |FAILED |
|calcpmvpdd          |FAILED |
|calcSET             |FAILED |
|calcSolarGain       |FAILED |
|calctAdapt15251     |FAILED |
|calctAdaptASHRAE    |FAILED |
|calcVTG             |PASSED |
|calcWbgt            |PASSED |
|calcWCI             |PASSED |
|calUtci [2.3s]      |FAILED |
========test result========


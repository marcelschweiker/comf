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
|calc2Node           |PASSED |
|calcAD              |PASSED |
|calcAT              |PASSED |
|calcATHBstrandard   |PASSED |
|calcCE              |PASSED |
|calcCloTout         |PASSED |
|calcDiscomfortIndex |PASSED |
|calcePMV            |PASSED |
|calcHeatIndex       |PASSED |
|calcHumidity        |PASSED |
|calcPetSteady       |PASSED |
|calcPhs             |PASSED |
|calcpmvpdd          |PASSED |
|calcSET             |PASSED |
|calcSolarGain       |PASSED |
|calctAdapt15251     |PASSED |
|calctAdaptASHRAE    |PASSED |
|calcUtci [2.2s]     |PASSED |
|calcVTG             |PASSED |
|calcWbgt            |PASSED |
|calcWCI             |PASSED |
========test result========


# PopulationSimulationR Package

## Overview

The **PopulationSimulationR** package is a powerful tool for simulating age and time-structured populations. It is designed primarily for epidemiological research, with a focus on estimating incidence, especially for chronic conditions like HIV. This package allows you to create and manipulate synthetic populations for various modeling and analysis purposes.

## Features

- **Population Modeling:** Simulate populations over time with age and time structure.
- **Customizable Parameters:** Specify key rate functions, including birth rate, PMTCT rate, incidence rate, base mortality rate, time step, and maximum age.
- **Simulation Control:** Use wrapper functions to control the simulation process and start simulations at different points.
- **Data Output:** Generate age and time-structured population data for analysis.
- **Prevalence Estimation:** Calculate prevalence by age and analyze the epidemic's status at specified dates.
- **Regional/Cluster Analysis:** Partition prevalence into regional or cluster-level prevalence for subpopulation analysis.

## Installation

To install the package, you can use the `devtools` package:

```R
# Install devtools if you haven't already
install.packages("devtools")

# Install PopulationSimulationR from GitHub
devtools::install_github("laurettemhlanga/PopulationSimulationR")
```

## Getting Started

Here's a quick example of how to use the package:

```R
# Load the PopulationSimulationR package
library(PopulationSimulationR)

# Specify your rate functions and parameters
# (birth rate, PMTCT rate, incidence rate, etc.)

# Run a simulation
population_data <- birth_cohort_simulation(rate_functions = your_rate_functions, ...)

# Analyze and visualize the simulated population data
# (e.g., prevalence by age, time series analysis)
```

## Documentation

For detailed usage instructions and function documentation, please refer to the package vignettes. You can access the vignettes using the following command after loading the package:

```R
# Load the PopulationSimulationR package
library(PopulationSimulationR)

# Access package vignettes
browseVignettes("PopulationSimulationR")
```

## Contributing

We welcome contributions from the community. If you encounter issues, have ideas for enhancements, or would like to contribute code, please check our [Contributing Guidelines](CONTRIBUTING.md) for more information.

## License

This package is distributed under the [MIT License](LICENSE.md).

## Acknowledgments

We would like to acknowledge the contributions of the research community that have made this package possible. Thank you for your support and feedback.

---

Please replace "YourGitHubUsername" in the installation section with your actual GitHub username, and customize the documentation and contributing sections to include specific information about your package. Additionally, consider adding examples and more detailed instructions as needed to assist users in effectively using your package.

# Interactive Bayesian Inference

An interactive R Shiny application that demonstrates Bayesian inference through two practical examples: bicycle ownership estimation and pre-election polling analysis.

## Overview

This app provides hands-on exploration of Bayesian statistical concepts by allowing users to:

- Generate simulated data with known parameters
- Observe how prior beliefs combine with observed data
- Compare different estimation approaches (frequentist vs. Bayesian)
- Visualize posterior distributions and credible intervals

## Examples

### 1. Bicycle Ownership in Ames, Iowa
Estimate the proportion of bicycle owners in a population using:

- **Data**: Bernoulli trials (yes/no bicycle ownership)
- **Prior**: Uniform distribution on [0,1]
- **Likelihood**: Binomial distribution
- **Posterior**: Beta distribution

**Key Features:**

- Interactive sliders for sample size and true probability
- Visual comparison of sample proportion vs. posterior mean
- Real-time updates of likelihood and posterior distributions

### 2. Pre-Election Polling
Analyze support for two candidates plus undecided voters using:

- **Data**: Multinomial distribution (3 categories)
- **Prior**: Non-informative Dirichlet distribution
- **Posterior**: Dirichlet distribution
- **Analysis**: Support difference estimation via simulation

**Key Features:**

- Interactive slides for poll results and sample sizes
- Monte Carlo simulation from posterior distribution
- Credible intervals and posterior probabilities
- Histogram visualization of support differences

## Installation

### Prerequisites

- R (version 3.6 or higher)
- RStudio (recommended)

### Required Packages
Install required R packages.
```r
install.packages(c(
  "shiny",
  "ggplot2", 
  "dplyr",
  "gtools",
  "RColorBrewer"
))
```

### Running the App

#### Option 1: Local Installation

1. Clone the GitHub repository [stephaniereinders/interactive-bayesian-inference](https://github.com/stephaniereinders/interactive-bayesian-inference).

2. Open R/RStudio and run:
```r
shiny::runApp()
```

#### Option 2: Direct from GitHub
```r
shiny::runGitHub("interactive-bayesian-inference", "stephaniereinders")
```

## Usage Guide

### Bicycle Ownership Example

1. **Set Parameters**: Use sliders to adjust:
   
   - Sample size (n): Number of people surveyed
   - True probability (θ): Actual proportion of bicycle owners

2. **View Results**:
   
   - **Simulated Data tab**: See the randomly generated survey responses
   - **Estimate Proportion tab**: Compare different estimation methods

3. **Key Insights**:
   
   - Observe how the posterior mean balances prior beliefs with data
   - See how sample size affects the precision of estimates
   - Compare Bayesian vs. frequentist approaches

### Pre-Election Polling Example

1. **Input Poll Results**:
   
   - Sample size: Total number of respondents
   - Support counts for each candidate
   - "No opinion" is calculated automatically

2. **Run Analysis**:
   
   - Adjust number of simulation draws
   - Click "Run Simulations" to generate results

3. **Interpret Results**:
   
   - Median difference in support
   - 95% credible interval
   - Probability statements about candidate preferences

## Educational Applications

This app is designed for:

- **Statistics Courses**: Demonstrate Bayesian vs. frequentist inference
- **Political Science**: Show uncertainty in polling data
- **Data Science Training**: Hands-on experience with Bayesian methods
- **Self-Learning**: Interactive exploration of statistical concepts

## Statistical Background

### Bicycle Ownership

- **Likelihood**: p(y|θ) = (n choose y) θ^y (1-θ)^(n-y)
- **Prior**: p(θ) = 1 (uniform on [0,1])
- **Posterior**: p(θ|y) ∝ θ^y (1-θ)^(n-y) (Beta distribution)

### Election Polling

- **Likelihood**: p(y|θ) ∝ θ₁^y₁ θ₂^y₂ θ₃^y₃
- **Prior**: Dirichlet(1,1,1) (non-informative)
- **Posterior**: Dirichlet(y₁+1, y₂+1, y₃+1)

## Technical Details

- **Framework**: R Shiny
- **Visualization**: ggplot2
- **Statistical Functions**: Base R stats, gtools for Dirichlet
- **Responsive Design**: Bootstrap-based UI
- **Mathematical Notation**: MathJax for LaTeX rendering

## Contributing

Contributions are welcome! Please feel free to:

- Report bugs or suggest features via Issues
- Submit pull requests for improvements
- Share educational use cases

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Citation

If you use this app in your teaching or research, please cite:
```
[Stephanie Reinders] (2024). Interactive Bayesian Inference. 
GitHub repository: https://github.com/stephaniereinders/interactive-bayesian-inference
```

## Contact

- **Author**: [Stephanie Reinders]
- **GitHub**: [@stephaniereinders](https://github.com/stephaniereinders)

---

*Built with ❤️ for statistics education*
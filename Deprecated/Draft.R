# Split the data into lines
lines <- unlist(strsplit(papers_2003, "\n"))

# Initialize empty lists to store each component
authors <- list()
titles <- list()
paper_numbers <- list()

# Iterate over each line to extract information
for(line in lines) {
  # Extract authors by capturing everything before the year
  author_match <- regmatches(line, regexpr("^(.+?)\\(\\d{4}\\):", line))
  # Replace ' and ' with '; '
  cleaned_authors <- gsub(" and ", "; ", gsub("\\s\\(\\d{4}\\):", "", author_match))
  authors <- c(authors, cleaned_authors)
  
  # Extract titles by capturing everything between ": " and ". Collaborative"
  title_match <- regmatches(line, regexpr("(?<=: ).+?(?=\\. Collaborative)", line, perl=TRUE))
  titles <- c(titles, title_match)
  
  # Extract paper numbers by capturing digits following "Discussion Paper"
  number_match <- regmatches(line, regexpr("(?<=Discussion Paper )\\d+", line, perl=TRUE))
  paper_numbers <- c(paper_numbers, number_match)
}

# Combine the lists into a dataframe
papers_2003 <- data.frame(
  author = unlist(authors),
  paper = unlist(titles),
  id = unlist(paper_numbers),
  stringsAsFactors = FALSE
)

papers_2003 <- papers_2003 %>%
  mutate(author = strsplit(as.character(author), ";\\s*")) %>%
  unnest(author) %>%
  mutate(has_comma = grepl(",", author)) %>%
  mutate(author = if_else(has_comma, author, paste(author, ",", sep = ""))) %>%
  separate(author, into = c("LastName", "FirstName"), sep = "\\s*,\\s*", extra = "merge", fill = "right") %>%
  unite("author", FirstName, LastName, sep = " ", remove = TRUE) %>%
  mutate(author = trimws(author)) %>%
  select(-has_comma) %>%
  mutate(year = 2003)


###### 2006
papers_2006 <- data.frame(
  author = c(
    "Aas, Kjersti; Czado, Claudia; Frigessi, Arnoldo; Bakken, Henrik",
    "Boulesteix, Anne-Laure; Strobl, Carolin",
    "Coolen, F. P. A.; Augustin, Thomas",
    "Czado, Claudia; Erhardt, Vinzenz; Min, Aleksey",
    "Czado, Claudia; Haug, Stephan",
    "Czado, Claudia; Haug, Stephan",
    "Czado, Claudia; Min, Aleksey",
    "Czado, Claudia; Pflüger, Carolin",
    "Czado, Claudia; Song, Peter X.-K.",
    "Dargatz, Christiane",
    "Fahrmeir, Ludwig; Kneib, Thomas",
    "Fahrmeir, Ludwig; Raach, Alexander",
    "Fahrmeir, Ludwig; Steinert, Sven",
    "Gschlößl, Susanne; Czado, Claudia",
    "Haug, Stephan; Czado, Claudia",
    "Heim, Susanne",
    "Held, Hermann; Kriegler, Elmar; Augustin, Thomas",
    "Hennerfeind, Andrea; Held, Leonhard",
    "Holzmann, Hajo; Min, Aleksey; Czado, Claudia",
    "Högn, Ralph; Czado, Claudia",
    "Höhle, Michael",
    "Höhle, Michael; Held, Leonhard",
    "Klüppelberg, Claudia; Kuhn, Gabriel",
    "Klüppelberg, Claudia; Kuhn, Gabriel; Peng, Liang",
    "Klüppelberg, Claudia; Kuhn, Gabriel; Peng, Liang",
    "Klüppelberg, Claudia; Peng, Liang",
    "Kneib, Thomas; Baumgartner, Bernhard; Steiner, Winfried J.",
    "Kneib, Thomas; Hennerfeind, Andrea",
    "Kneib, Thomas; Müller, Jörg; Hothorn, Torsten",
    "Krämer, N.; Boulesteix, Anne-Laure; Tutz, Gerhard",
    "Kukush, Alexander; Malenko, Andrii; Schneeweiß, Hans",
    "Kukush, Alexander; Malenko, Andrii; Schneeweiß, Hans; Shalabh",
    "Kukush, Alexander; Schneeweiß, Hans",
    "Küchenhoff, Helmut; Lederer, Wolfgang; Lesaffre, Emmanuel",
    "Leitenstorfer, Florian; Tutz, Gerhard",
    "Leitenstorfer, Florian; Tutz, Gerhard",
    "Müller, Gernot; Czado, Claudia",
    "Neuhaus, Anneke; Augustin, Thomas; Heumann, Christian; Daumer, Martin",
    "Reithinger, Florian; Jank, Wolfgang; Tutz, Gerhard; Shmueli, Galit",
    "Schmid, Matthias",
    "Schneeweiß, Hans; Komlos, John; Ahmad, A.",
    "Schneeweiß, Hans; Kukush, Alexander",
    "Schneeweiß, Hans; Shalabh",
    "Shalabh; Toutenburg, Helge; Heumann, Christian",
    "Shalabh; Toutenburg, Helge; Heumann, Christian",
    "Shalabh; Toutenburg, Helge; Heumann, Christian",
    "Strobl, Carolin; Boulesteix, Anne-Laure; Zeileis, Achim; Hothorn, Torsten",
    "Tutz, Gerhard; Ulbricht, Jan"
  ),
  paper = c(
    "Pair-copula constructions of multiple dependence",
    "Maximally selected chi-square statistics and umbrella orderings",
    "A nonparametric predictive alternative to the Imprecise Dirichlet Model: the case of a known number of categories",
    "Zero-inflated generalized Poisson models with regression effects on the mean, dispersion and zero-inflation level applied to patent outsourcing rates",
    "Quasi maximum likelihood estimation and prediction in the compound Poisson ECOGARCH(1,1) model",
    "A fractionally integrated ECOGARCH process",
    "Testing for zero-modification in count regression models",
    "Modeling dependencies between rating categories and their effects on prediction in a credit risk portfolio",
    "State space mixed models for longitudinal observations with binary and binomial responses",
    "A Diffusion Approximation for an Epidemic Model",
    "Propriety of Posteriors in Structured Additive Regression Models: Theory and Empirical Evidence",
    "A Bayesian semiparametric latent variable model for mixed responses",
    "A geoadditive Bayesian latent variable model for Poisson indicators",
    "Modelling count data with overdispersion and spatial effects",
    "An exponential continuous time GARCH process",
    "Space-Varying Coefficient Models for Diffusion Tensor Imaging using 3d Wavelets",
    "Bayesian Learning for a Class of Priors with Prescribed Marginals",
    "A Bayesian geoadditive relative survival analysis of registry data on breast cancer mortality",
    "Validating linear restrictions in linear regression models with general error structure",
    "Multiresolution Analysis of Long Time Series with Applications to Finance",
    "Poisson regression charts for the monitoring of surveillance time series",
    "Bayesian Estimation of the Size of a Population",
    "Copula Structure Analysis Based on Robust and Extreme Dependence Measures",
    "Estimating Tail Dependence of Elliptical Distributions",
    "Multivariate Tail Copula: Modeling and Estimation",
    "Empirical Likelihodd Methods for an AR(1) process with ARCH(1) errors",
    "Semiparametric Multinomial Logit Models for Analysing Consumer Choice Behaviour",
    "Bayesian Semiparametric Multi-State Models",
    "Spatial Smoothing Techniques for the Assessment of Habitat Suitability",
    "Penalized Partial Least Squares Based on B-Splines Transformations",
    "Optimality of the quasi-score estimator in a mean-variance model with applications to measurement error models",
    "Optimality of Quasi-Score in the multivariate mean-variance model with an application to the zero-inflated Poisson model with measurement errors",
    "Asymptotic optimality of the quasi-score estimator in a class of linear score estimators",
    "Asymptotic Variance Estimation for the Misclassification SIMEX",
    "Knot selection by boosting techniques",
    "Smoothing with Curvature Constraints based on Boosting Techniques",
    "Stochastic volatility models for ordinal valued time series with application to finance",
    "A Review on Joint Models in Biometrical Research",
    "Smoothing sparse and unevenly sampled curves using semiparametric mixed models: An application to online auctions",
    "The Effect of Single-Axis Sorting on the Estimation of a Linear Regression",
    "Symmetric and Asymmetric Rounding",
    "Comparing the efficiency of structural and functional methods in measurement error models",
    "On the Estimation of the Linear Relation when the Error Variances are known",
    "Performance of Double k-class Estimators for Coefficients in Linear Regression Models with Non Spherical Disturbances under Asymmetric Losses",
    "Mean Squared Error Matrix comparison of Least Squares and Stein-Rule Estimators for Regression Coefficients under Non-normal Disturbances",
    "Risk Performance Of Stein-Rule Estimators Over The Least Squares Estimators Of Regression Coefficients Under Quadratic Loss Structures",
    "Bias in Random Forest Variable Importance Measures: Illustrations, Sources and a Solution",
    "Penalized Regression with Correlation Based Penalty"
  ),
  id = c(
    "487", "476", "489", "482",
    "516", "484", "474",
    "511", "503", "517",
    "510", "471", "508",
    "475", "480", "505",
    "488", "515", "478",
    "497", "500", "499",
    "507", "470", "468",
    "469", "501", "502",
    "492", "485", "494",
    "498", "477", "473",
    "481", "467", "504",
    "506", "483", "472",
    "479", "491", "493",
    "509", "496", "495",
    "490", "486"
  )
)

#### Split authors into separate rows and replicate paper titles and IDs for each author
# papers_2006 <- papers_2006 %>%
#   mutate(author = strsplit(as.character(author), ";\\s*")) %>%
#   unnest(author) %>%
#   separate(author, into = c("LastName", "FirstName"), sep = "\\s*,\\s*", extra = "merge") %>%
#   unite("author", FirstName, LastName, sep = " ")%>%
#   mutate(year = 2006)

papers_2006 <- papers_2006 %>%
  mutate(author = strsplit(as.character(author), ";\\s*")) %>%
  unnest(author) %>%
  # Identify if the author name contains a comma
  mutate(has_comma = grepl(",", author)) %>%
  # Pre-process author names: append a comma for names with no comma to avoid separation
  mutate(author = if_else(has_comma, author, paste(author, ",", sep = ""))) %>%
  separate(author, into = c("LastName", "FirstName"), sep = "\\s*,\\s*", extra = "merge", fill = "right") %>%
  # Combine first and last name, handling cases with no first name
  unite("author", FirstName, LastName, sep = " ", remove = TRUE) %>%
  # Remove any trailing or leading whitespace that may have been introduced
  mutate(author = trimws(author)) %>%
  # Remove the temporary 'has_comma' column
  select(-has_comma) %>%
  mutate(year = 2006)

##### 2005
papers_2005 <- data.frame(
  # Create vectors to store author names, paper titles, and paper IDs
  author = c(
    "Boulesteix, Anne-Laure", "Boulesteix, Anne-Laure", "Boulesteix, Anne-Laure; Strimmer, Korbinian", 
    "Boulesteix, Anne-Laure; Strimmer, Korbinian", "Brezger, Andreas; Fahrmeir, Ludwig; Hennerfeind, Andrea", 
    "Brezger, Andreas; Lang, S.", "Brockwell, Peter J.; Chadraa, Erdenebaatar; Lindner, Alexander M.", 
    "Czado, Claudia; Heyn, Anette; Müller, Gernot", "Czado, Claudia; Heyn, Anette; Müller, Gernot", 
    "Czado, Claudia; Min, Aleksey", "Dargatz, Christiane; Georgescu, Vera; Held, Leonhard", 
    "Ebers, G. C.; Heigenhauser, L.; Daumer, Martin; Lederer, C.; Noseworthy, J. H.", "Eilers, Paul H. C.; Heim, Susanne; Marx, Brian D.", 
    "Einbeck, Jochen; Augustin, Thomas", "Fasen, V.; Klüppelberg, Claudia; Lindner, A.", "Gschlößl, Susanne; Czado, Claudia", 
    "Gschlößl, Susanne; Czado, Claudia", "Gschlößl, Susanne; Czado, Claudia", "Gschlößl, Susanne; Czado, Claudia", 
    "Hahn, Klaus; Prigarin, Sergej; Heim, Susanne; Hasan, Khader", "Hahn, Klaus; Prigarin, Sergej; Pütz, Benno; Hasan, Khader", 
    "Haug, Stephan; Czado, Claudia", "Haug, Stephan; Klüppelberg, Claudia; Lindner, A.; Zapp, M.", "Heigenhauser, L.; Confavreux, C.; Daumer, Martin; Ebers, G. C.; Kappos, L.; Lederer, C.; Neiß, A.; Polman, C.; Vukusic, S.", 
    "Heim, Susanne; Fahrmeir, Ludwig; Eilers, Paul H. C.; Marx, Brian D.", "Held, Leonhard; Hofmann, M.; Höhle, Michael; Schmid, Volker J.",
    "Held, U.; Heigenhauser, L.; Shang, C.; Kappos, L.; Polman, C.", "Hennerfeind, Andrea; Brezger, Andreas; Fahrmeir, Ludwig", 
    "Hennerfeind, Andrea; Brezger, Andreas; Fahrmeir, Ludwig", "Höhle, Michael; Riebler, A.", "Klüppelberg, Claudia; Lindner, A.", 
    "Klüppelberg, Claudia; Lindner, Alexander M.; Maller, R. A.", "Klüppelberg, Claudia; Lindner, Alexander M.; Maller, R. A.", 
    "Kneib, Thomas", "Kneib, Thomas; Fahrmeir, Ludwig", "Kuhn, Gabriel", "Kukush, Alexander; Schneeweiß, Hans; Shklyar, Sergiy", 
    "Leitenstorfer, Florian; Tutz, Gerhard", "Lindner, Alexander M.; Szimayer, Alexander", "Nilsson, D.; Höhle, Michael", 
    "Prigarin, Sergej; Hahn, Klaus", "Schmid, Matthias", "Schmid, Matthias; Schneeweiß, Hans", "Schmid, Matthias; Schneeweiß, Hans", 
    "Schmid, Matthias; Schneeweiß, Hans; Küchenhoff, Helmut", "Schmid, Matthias; Schneeweiß, Hans; Küchenhoff, Helmut", 
    "Schneeweiß, Hans", "Schneeweiß, Hans", "Schneeweiß, Hans; Augustin, Thomas", "Shalabh; Toutenburg, Helge", 
    "Shalabh; Toutenburg, Helge", "Shklyar, Sergiy; Schneeweiß, Hans; Kukush, Alexander", "Steiner, Winfried J.; Belitz, Christiane; Lang, Stefan", 
    "Strobl, Carolin", "Strobl, Carolin", "Strobl, Carolin; Boulesteix, Anne-Laure; Augustin, Thomas", "Tutz, Gerhard; Binder, Harald", 
    "Tutz, Gerhard; Leitenstorfer, Florian", "Tutz, Gerhard; Reithinger, Florian"
  ),
  
  paper = c(
    "Maximally selected chi-square statistics and binary splits of nominal variables", 
    "Maximally selected chi-square statistics for at least ordinal scaled variables", 
    "Partial Least Squares: A Versatile Tool for the Analysis of High-Dimensional Genomic Data", 
    "Predicting Transcription Factor Activities from Combined Analysis of Microarray and ChIP Data: A Partial Least Squares Approach", 
    "Adaptive Gaussian Markov Random Fields with Applications in Human Brain Mapping", 
    "Simultaneous probability statements for Bayesian P-splines", 
    "A Continuous Time GARCH Process of Higher Order", 
    "Modeling migraine severity with autoregressive ordered probit models", 
    "Modeling migraine severity with autoregressive ordered probit models", 
    "Consistency and asymptotic normality of the maximum likelihood estimator in a zero-inflated generalized Poisson regression", 
    "Stochastic modelling of the spatial spread of influenza in Germany", 
    "Multiple sclerosis, the measurement of disability and access to clinical trial data", 
    "Varying Coefficient Tensor Models for Brain Imaging", 
    "On weighted local fitting and its relation to the Horvitz-Thompson estimator", 
    "Extremal behavior of stochastic volatility models", 
    "Spatial modelling of claim frequency and claim size in insurance", 
    "Does a Gibbs sampler approach to spatial Poisson regression models outperform a single site MH sampler?", 
    "Introducing and evaluating a Gibbs sampler for spatial Poisson regression models", 
    "Modelling count data with overdispersion and spatial effects", 
    "Random noise in Diffusion Tensor Imaging, its Destructive Impact and Some Corrections", 
    "DTI denoising for data with low signal to noise ratios", 
    "Mixed effect model for absolute log returns of ultra high frequency data", 
    "Estimating the COGARCH(1,1) model - a first go", 
    "Treating Systematic Errors in Multiple Sclerosis Data", 
    "Space-Varying Coefficient Models for Brain Imaging", 
    "A two-component model for counts of infectious diseases", 
    "Predicting the On-Study Relapse Rate for Multiple Sclerosis Patients in Clinical Trials", 
    "Geoadditive Survival Models: A Supplement", 
    "Geoadditive survival models", 
    "The R-Package 'surveillance'", 
    "Extreme value theory for moving average processes with light-tailed innovations", 
    "Continuous time volatility modelling: COGARCH versus Ornstein-Uhlenbeck models", 
    "A Continuous Time GARCH Process Driven by a Lévy Process: Stationarity and Second Order Behaviour", 
    "Geoadditive hazard regression for interval censored survival times", 
    "Supplement to \"Structured additive regression for categorical space-time data: A mixed model approach\"", 
    "Tails of Credit Default Portfolios", 
    "Quasi Score is more efficient than Corrected Score in a general nonlinear measurement error model", 
    "Generalized Monotonic Regression Based on B-Splines with an Application to Air Pollution Data", 
    "A Limit Theorem for Copulas", 
    "Methods for evaluating Decision Problems with Limited Information", 
    "Stochastic Algorithms for White Matter Fiber Tracking and the Inference of Brain Connectivity from MR Diffusion Tensor Data", 
    "Estimation of a Linear Model under Microaggregation by Individual Ranking", 
    "Estimation of a Linear Regression under Microaggregation with the Response Variable as a Sorting Variable", 
    "The Effect of Microaggregation Procedures on the Estimation of Linear Models: A Simulation Study", 
    "Statistical Inference in a Simple Linear Model Under Microaggregation", 
    "Consistent Estimation of a Simple Linear Model Under Microaggregation", 
    "The polynomial and the Poisson measurement error models: some further results on quasi score and corrected score estimation", 
    "Abraham Wald", 
    "Some Recent Advances in Measurement Error Models and Methods", 
    "On the regression method of estimation of population mean from incomplete survey data through imputation", 
    "Consequences of Departure from Normality on the Properties of Calibration Estimators", 
    "Quasi Score is more efficient than Corrected Score in a polynomial measurement error model", 
    "Semiparametric Stepwise Regression to Estimate Sales Promotion Effects", 
    "Statistical Sources of Variable Selection Bias in Classification Tree Algorithms Based on the Gini Index", 
    "Variable Selection Bias in Classification Trees Based on Imprecise Probabilities", 
    "Unbiased split selection for classification trees based on the Gini Index", 
    "Boosting Ridge Regression", 
    "Generalized smooth monotonic regression", 
    "Flexible semiparametric mixed models"
  ),
  
  id = c(
    449, 407, 457, 411, 456, 437, 428, 463, 413, 423, 450, 429, 436, 465, 427, 461, 460, 434, 412, 459, 409, 440, 458, 435, 455, 424, 430, 454, 414, 422, 432, 426, 425, 447, 431, 410, 451, 444, 433, 421, 408, 453, 462, 443, 416, 415, 446, 439, 452, 442, 441, 445, 438, 420, 419, 464, 418, 417, 448
  )
)

papers_2005 <- papers_2005 %>%
  mutate(author = strsplit(as.character(author), ";\\s*")) %>%
  unnest(author) %>%
  # Identify if the author name contains a comma
  mutate(has_comma = grepl(",", author)) %>%
  # Pre-process author names: append a comma for names with no comma to avoid separation
  mutate(author = if_else(has_comma, author, paste(author, ",", sep = ""))) %>%
  separate(author, into = c("LastName", "FirstName"), sep = "\\s*,\\s*", extra = "merge", fill = "right") %>%
  # Combine first and last name, handling cases with no first name
  unite("author", FirstName, LastName, sep = " ", remove = TRUE) %>%
  # Remove any trailing or leading whitespace that may have been introduced
  mutate(author = trimws(author)) %>%
  # Remove the temporary 'has_comma' column
  select(-has_comma) %>%
  mutate(year = 2005)

##### 2004
papers_2004 <- data.frame(
  # Define the authors, papers, and ids
  author = c("Adejumo, A. O.; Heumann, Christian; Toutenburg, Helge",
             "Adejumo, A. O.; Heumann, Christian; Toutenburg, Helge",
             "Adejumo, A. O.; Heumann, Christian; Toutenburg, Helge",
             "Barndorff-Nielsen, Ole Eiler; Lindner, Alexander M.",
             "Barndorff-Nielsen, Ole Eiler; Stelzer, Robert",
             "Boulesteix, Anne-Laure",
             "Boulesteix, Anne-Laure",
             "Boulesteix, Anne-Laure; Tutz, Gerhard",
             "Czado, Claudia; Delwarde, A.; Denuit, M.",
             "Czado, Claudia; Kolbe, A.",
             "Czado, Claudia; Kolbe, A.",
             "Czado, Claudia; Prokopenko, S.",
             "Einbeck, Jochen; Tutz, Gerhard",
             "Hechenbichler, K.; Schliep, K.",
             "Held, Leonhard; Höhle, Michael; Hofmann, M.",
             "Held, Leonhard; Ranyimbo, Argwings Otiemo",
             "Helms, F.; Czado, Claudia; Gschlößl, Susanne",
             "Hofmann, M.; Höhle, Michael; Held, Leonhard",
             "Hsing, T.; Klüppelberg, Claudia; Kuhn, Gabriel",
             "Hsing, T.; Klüppelberg, Claudia; Kuhn, Gabriel",
             "Kneib, Thomas; Fahrmeir, Ludwig",
             "Kneib, Thomas; Fahrmeir, Ludwig",
             "Krause, Rüdiger; Tutz, Gerhard",
             "Krause, Rüdiger; Tutz, Gerhard",
             "Kukush, Alexander; Schneeweiß, Hans",
             "Linde, A. van der; Tutz, Gerhard",
             "Müller, M.",
             "Nittner, T.; Toutenburg, Helge",
             "Toutenburg, Helge; Heumann, Christian; Nittner, T.",
             "Tutz, Gerhard; Binder, Harald",
             "Tutz, Gerhard; Binder, Harald",
             "Zempléni, A."),
  
  paper = c("Modelling Negative Binomial as a substitute model to Poisson for raters agreement on ordinal scales with sparse data",
            "Conditional symmetry model as a better alternative to Symmetry Model for rater agreement measure",
            "A review of agreement measure as a subset of association measure between raters",
            "Some aspects of Lévy copulas",
            "Absolute Moments of Generalized Hyperbolic Distributions and Approximate Scaling of Normal Inverse Gaussian Lévy-Processes",
            "A note on between-group PCA",
            "PLS dimension reduction for classification of microarray data",
            "Identification of Interaction Patterns and Classification with Applications to Microarray Data",
            "Bayesian Poisson Log-Bilinear Mortality Projections",
            "Empirical Study of Intraday Option Price Changes using extended Count Regression Models",
            "Statistical Analysis of Absolute Transaction Price Changes of Options",
            "Modeling Transport Mode Decisions Using Hierarchical Binary Spatial Regression Models with Cluster Effects",
            "Modelling beyond Regression Functions: an Application of Multimodal Regression to Speed-Flow Data",
            "Weighted k-Nearest-Neighbor Techniques and Ordinal Classification",
            "A statistical framework for the analysis of multivariate infectious disease surveillance data",
            "Bayesian estimation of the false negative fraction in screening tests",
            "Calculation of LTC Premiums based on direct estimates of transition probabilities",
            "A stochastic model for multivariate surveillance of infectious diseases",
            "Modelling, Estimation and Visualization of Multivariate Dependence for Risk Management",
            "Dependence Estimation and Visualization in Multivariate Extremes with Applications to Financial Data",
            "A mixed model approach for structured hazard regression",
            "Structured additive regression for multicategorical space-time data: A mixed model approach",
            "Variable selection and discrimination in gene expression data by genetic algorithms",
            "Simultaneous selection of variables and smoothing parameters by genetic algorithms",
            "Relative Efficiency of Maximum Likelihood and Other Estimators in a Nonlinear Regression Model with Small Measurement Errors",
            "On association in regression: the coefficient of determination revisited",
            "Goodness-of-fit criteria for survival data",
            "Identifying Missing Data Mechanisms in (2 x 2)-Contingency Tables",
            "Statistische Methoden bei unvollständigen Daten",
            "Generalized additive modelling with implicit variable selection by likelihood based boosting",
            "Localized Regression",
            "Goodness-of-fit test in extreme value applications"),
  
  id = c(387, 386, 385, 388, 381, 397, 392, 369, 398, 403, 384, 406, 395, 399, 402, 376, 393, 394, 375, 374, 400, 377, 390, 389, 396, 391, 382, 373, 380, 401, 378, 383)
)

papers_2004 <- papers_2004 %>%
  mutate(author = strsplit(as.character(author), ";\\s*")) %>%
  unnest(author) %>%
  # Identify if the author name contains a comma
  mutate(has_comma = grepl(",", author)) %>%
  # Pre-process author names: append a comma for names with no comma to avoid separation
  mutate(author = if_else(has_comma, author, paste(author, ",", sep = ""))) %>%
  separate(author, into = c("LastName", "FirstName"), sep = "\\s*,\\s*", extra = "merge", fill = "right") %>%
  # Combine first and last name, handling cases with no first name
  unite("author", FirstName, LastName, sep = " ", remove = TRUE) %>%
  # Remove any trailing or leading whitespace that may have been introduced
  mutate(author = trimws(author)) %>%
  # Remove the temporary 'has_comma' column
  select(-has_comma) %>%
  mutate(year = 2004)

#### 2003
papers_2003 <- data.frame(
  papers <- c("Regression calibration for Cox regression under heteroscedastic measurement error - Determining risk factors of cardiovascular diseases from error-prone nutritional replication data",
              "Generating Survival Times to Simulate Cox Proportional Hazards Models",
              "Women, Work, and Motherhood: Changing Employment Penalties for Motherhood in West Germany after 1945 -- A Comparative Analysis of Cohorts Born in 1934-1971",
              "Stochastic modeling for the COMET-assay",
              "A Framework to Discover Emerging Patterns for Application in Microarray Data",
              "BayesX: Analysing Bayesian structured additive regression models",
              "Generalized structured additive regression based on Bayesian P-splines",
              "Monotonic regression based on Bayesian P-splines: an application to estimating price response functions from store-level scanner data",
              "The inception selection effect of diagnosis in a German long term care portfolio",
              "Bayesian P-Splines to investigate the impact of covariates on Multiple Sclerosis clinical course",
              "Local Principal Curves",
              "Bayesian mapping of brain regions using compound Markov random field priors",
              "Nonparametric Bayesian hazard rate models based on penalized splines",
              "Penalized additive regression for space-time data: a Bayesian perspective",
              "Structured count data regression",
              "An Elementary Rigorous Introduction to Exact Sampling",
              "Multiple Imputation von fehlenden Werten mit Daten über Unterernährung und Kindersterblichkeit",
              "Das Auszugsverhalten junger Menschen",
              "Analysis of the time to sustained progression in Multiple Sclerosis using generalised linear and additive models",
              "Spatial Smoothing for Diffusion Tensor Imaging with low Signal to Noise Ratios",
              "Diffusion Tensor Imaging: on the assessment of data quality - a preliminary bootstrap analysis",
              "Analysing the course of multiple sclerosis with segmented regression models",
              "Geoadditive survival models",
              "Efficient simulation of Bayesian logistic regression models",
              "Theoretical Foundations of Autoregressive Models for Time Series on Acyclic Directed Graphs",
              "Locally Adaptive Function Estimation for Binary Regression Models",
              "Modeling Probabilities of Patent Oppositions in a Bayesian Semiparametric Regression Framework",
              "Responder Identification in Clinical Trials with Censored Data",
              "Identification of Responders to Amiodarone: Subgroup Analysis of the EMIAT Study",
              "Analysis of the Determinants of Fertility Decline in the Czech Republic",
              "Stationarity and second order behaviour of discrete and continuous time GARCH(1,1) processes",
              "Alternatives to the MCMC method",
              "Additive Modelling with Penalized Regression Splines and Genetic Algorithms",
              "Effect of Berkson measurement error on parameter estimates in Cox regression models",
              "Bias of Maximum-Likelihood estimates in logistic and Cox regression models: A comparative simulation study",
              "A Study of the Austrian Labour Makret Dynamics Using a Model of Search Equilibrium",
              "Extremal behavior of finite EGARCH Processes",
              "Small Ball Asymptotics for the Stochastic Wave Equation",
              "Exact and Fast Numerical Algorithms for the Stochastic Wave Equation",
              "Exact numerical algorithms for linear stochastic wave equation and stochastic Klein-Gordon equation",
              "Regression Models for Ordinal Valued Time Series: Applications in High Frequency Finance and Medicine",
              "Implementation of complex interactions in a Cox regression framework",
              "Numerical solution of boundary value problems for stochastic differential equations on the basis of the Gibbs sampler",
              "Modelling time-varying effects in Cox model under order restrictions",
              "Bayesian modelling of space-time interactions on the Lexis diagram",
              "Estimating the endpoint of a uniform distribution under normal measurement errors with known error variance",
              "Bias of the Quasi Score Estimator of a Measurement Error Model Under Misspecification of the Regressor Distribution",
              "The Reduced Form of a Block Recursive Model",
              "Assessing Brain Activity through Spatial Bayesian Variable Selection",
              "Extension of CART using multiple splits under order restrictions",
              "Response smoothing estimators in binary regression",
              "Aggregating Classifiers With Ordinal Response Structure",
              "Ordinal regression modelling between proportional odds and non-proportional odds",
              "Risk Stratification in Post-MI Patients Based on Left Ventricular Ejection Fraction and Heart-Rate Turbulence",
              "Parsimonious Segmentation of Time Series' by Potts Models",
              "Unemployment Benefits and the Duration of Unemployment in East Germany",
              "The Speed of Leaving the Old Job: A Study on Job Changes and Exit into Unemployment during the East German Transition Process")
  
  ,authors <- c("Thomas Augustin, A. Döring, D. Rummel",
                "R. Bender, Thomas Augustin, Maria Blettner",
                "S. Bender, A. Kohlmann, S. Lang",
                "Anne-Laure Boulesteix, V. Hösel, V. Liebscher",
                "Anne-Laure Boulesteix, Gerhard Tutz",
                "Andreas Brezger, Thomas Kneib, S. Lang",
                "Andreas Brezger, S. Lang",
                "Andreas Brezger, Winfried J. Steiner",
                "Claudia Czado, Susanne Gschlößl",
                "C. Di Serio, C. Lamina",
                "Jochen Einbeck, Gerhard Tutz, L. Evers",
                "Ludwig Fahrmeir, Christoff Gössl, Andrea Hennerfeind",
                "Ludwig Fahrmeir, Andrea Hennerfeind",
                "Ludwig Fahrmeir, Thomas Kneib, S. Lang",
                "Ludwig Fahrmeir, L. Osuna",
                "F. Friedrich, Gerhard Winkler, O. Wittich, V. Liebscher",
                "H. Gartner, S. Scheid",
                "H. Gartner, T. Scholz",
                "U. Gehrmann, B. Hellriegel, A. Neiss, Ludwig Fahrmeir",
                "Klaus Hahn, Sergej Prigarin, Benno Pütz",
                "Susanne Heim, Klaus Hahn, Dorothee P. Auer",
                "B. Hellriegel, Martin Daumer, A. Neiß",
                "Andrea Hennerfeind, Andreas Brezger, Ludwig Fahrmeir",
                "C. Holmes, Leonhard Knorr-Held",
                "Ralph Högn, Claudia Czado",
                "A. Jerak, S. Lang",
                "A. Jerak, S. Wagner",
                "V. Kehl, Kurt Ulm",
                "V. Kehl, Kurt Ulm, G. Schmidt, Petra Barthel, M. Malik",
                "S. Klasen, A. Launov",
                "Claudia Klüppelberg, Alexander M. Lindner, R. A. Maller",
                "L. Knüsel",
                "Rüdiger Krause, Gerhard Tutz",
                "Helmut Küchenhoff, R. Bender, I. Langner, R. Lenz-Tönjes",
                "I. Langner, R. Bender, R. Lenz-Tönjes, Helmut Küchenhoff, Maria Blettner",
                "A. Launov",
                "A. Lindner, K. Meyer",
                "A. Martin",
                "A. Martin, Sergej Prigarin, Gerhard Winkler",
                "A. Martin, Sergej Prigarin, Gerhard Winkler",
                "Gernot Müller, Claudia Czado, Stefan Antes, Martin Rottenwallner",
                "M. Müller, Kurt Ulm",
                "Sergej Prigarin, Gerhard Winkler",
                "G. Salanti, Kurt Ulm",
                "Volker J. Schmid, Leonhard Held",
                "Hans Schneeweiß",
                "Hans Schneeweiß, Chi-Lun Cheng",
                "Hans Schneeweiß, Erich Otto Maschke, Manfred Pfannes",
                "Michael Smith, Benno Pütz, Dorothee P. Auer, Ludwig Fahrmeir",
                "R. Strobl, G. Salanti, Kurt Ulm",
                "Gerhard Tutz",
                "Gerhard Tutz, K. Hechenbichler",
                "Gerhard Tutz, T. Scholz",
                "Kurt Ulm, Petra Barthel, Linda Rolnitzky, Raphael Schneider, M. Malik, J. Thomas Bigger Jr., Georg Schmidt",
                "Gerhard Winkler, A. Kempe, V. Liebscher, O. Wittich",
                "J. Wolff",
                "J. Wolff, P. Trübswetter")
  
  ,ids <- c(345, 338, 309, 336, 313, 332, 321, 331, 357, 353, 320, 317, 361, 305, 334, 329, 322, 315, 354, 358, 356, 355, 333, 306, 326, 310, 323, 311, 366, 352, 337, 367, 312, 346, 362, 351, 347, 360, 349, 327, 335, 363, 328, 319, 314, 340, 339, 307, 316, 364, 318, 359, 304, 365, 348, 344, 343))

papers_2003 <- papers_2003 %>%
  mutate(author = strsplit(as.character(author), ";\\s*")) %>%
  unnest(author) %>%
  # Identify if the author name contains a comma
  mutate(has_comma = grepl(",", author)) %>%
  # Pre-process author names: append a comma for names with no comma to avoid separation
  mutate(author = if_else(has_comma, author, paste(author, ",", sep = ""))) %>%
  separate(author, into = c("LastName", "FirstName"), sep = "\\s*,\\s*", extra = "merge", fill = "right") %>%
  # Combine first and last name, handling cases with no first name
  unite("author", FirstName, LastName, sep = " ", remove = TRUE) %>%
  # Remove any trailing or leading whitespace that may have been introduced
  mutate(author = trimws(author)) %>%
  # Remove the temporary 'has_comma' column
  select(-has_comma) %>%
  mutate(year = 2003)

##### 2002
papers_2002 <- data.frame(
  # Create vectors to store authors, paper titles, and discussion paper IDs
  author = c("Adebayo, Samson B.; Fahrmeir, Ludwig",
             "Augustin, Thomas",
             "Augustin, Thomas",
             "Berger, U.; Fahrmeir, Ludwig; Klasen, S.",
             "Blauth, A.; Pigeot, Iris",
             "Böheim, R.",
             "Böheim, R.",
             "Czado, Claudia; Gschlößl, Susanne",
             "Czado, Claudia; Rudolph, F.",
             "Czado, Claudia; Sikora, I.",
             "Di Serio, C.; Vicard, P.",
             "Einbeck, Jochen",
             "Einbeck, Jochen; Diva Saldiva de André, C.; Singer, J.",
             "Fronk, Eva-Maria",
             "Hahn, Klaus; Rodenacker, K.; Auer, Dorothee P.",
             "Kandala, N. B.; Fahrmeir, Ludwig; Klasen, S.",
             "Kauermann, Göran; Berger, U.",
             "Kauermann, Göran; Küchenhoff, Helmut",
             "Kiselev, V. G.; Hahn, Klaus; Auer, Dorothee P.",
             "Klüppelberg, Claudia",
             "Klüppelberg, Claudia; Pergamenchtchikov, S.",
             "Kukush, Alexander; Schneeweiß, Hans; Wolf, R.",
             "Kukush, Alexander; Shklyar, Sergiy",
             "Lang, Stefan; Adebayo, Samson B.; Fahrmeir, Ludwig; Steiner, Winfried J.",
             "Müller, Gernot; Czado, Claudia",
             "Nittner, T.",
             "Nittner, T.",
             "Odejar, M. A. E.",
             "Payer, T.; Küchenhoff, Helmut",
             "Scheid, S.",
             "Schneeweiß, Hans; Cheng, Chi-Lun; Wolf, R.",
             "Shklyar, Sergiy; Schneeweiß, Hans",
             "Toutenburg, Helge; Heumann, Christian; Nittner, T.; Scheid, S.",
             "Tutz, Gerhard",
             "Tutz, Gerhard; Binder, Harald"),
  
  paper = c("Analyzing Child Mortality in Nigeria with Geoadditive Survival Models",
            "An exact corrected log-likelihood function for Cox's proportional hazards model under measurement error and some extensions",
            "Generalized basic probability assignments",
            "Dynamic Modelling of Child Mortality in Developing Countries: Application for Zambia",
            "Using Genetic Algorithms for Model Selection in Graphical Models",
            "Why are West African children underweight?",
            "The association between reported and calculated reservation wages",
            "Modeling of transition intensities and probabilities in a German long term care portfolio with known diagnosis",
            "Application of Survival Analysis Methods to Long Term Care Insurance",
            "Quantifying overdispersion effects in count regression data",
            "Graphical chain models for the analysis of complex genetic diseases: an application to hypertension",
            "Multivariate Local Fitting with General Basis Functions",
            "Local Smoothing with Robustness against Outlying Predictors",
            "Model Selection for Dags via RJMCMC for the Discrete and Mixed Case",
            "Intensity Segmentation of the Human Brain with Tissue dependent Homogenization",
            "Geo-additive models of Childhood Undernutrition in three Sub-Saharan African Countries",
            "A Smooth Test in Proportional Hazard Survival Models using Local Partial Likelihood Fitting",
            "Modelling Data from Inside of Earth: Local Smoothing of Mean and Dispersion Structure in Deep Drill Data",
            "Is the Brain Cortex a Fractal?",
            "Risk Management with Extreme Value Theory",
            "The Tail of the Stationary Distribution of a Random Coefficient AR(q) Model",
            "Comparing Different Estimators in a Nonlinear Measurement Error Model",
            "Comparison of three estimators in Poisson errors-in-variables model with one covariate",
            "Bayesian Geoadditive Seemingly Unrelated Regression",
            "Regression Models for Ordinal Valued Time Series with Application to High Frequency Financial Data",
            "Missing at Random (MAR) in Nonparametric Regression - A Simulation Experiment",
            "The Additive Model with Missing Values in the Independent Variable - Theory and Simulation",
            "Bayesian Analysis of Sample Selection and Endogenous Switching Regression Models with Random Coefficients Via MCMC Methods",
            "Modelling extreme wind speeds in the context of risk analysis for high speed trains",
            "A Selection Model for Bivariate Normal Data, with a Flexible Nonparametric Missing Model and a Focus on Variance Estimates",
            "On the bias of structural estimation methods in a polynomial regression with measurement error when the distribution of the latent covariate is a mixture of normals",
            "A comparison of asymptotic covariance matrices of three consistent estimators in the Poisson regression model with measurement errors",
            "Parametric and Nonparametric Regression with Missing X's - A Review",
            "Modelling of repeated ordered measurements by isotonic sequential regression",
            "Flexible Modelling of Discrete Failure Time Including Time-Varying Smooth Effects"),
  
  id = c(303, 277, 276, 299, 278, 274, 273, 302, 268, 289, 288, 292, 290, 271, 296, 287, 282, 269, 297, 270, 267, 244, 293, 300, 301, 284, 272, 291, 295, 285, 281, 283, 286, 298, 294)
)

papers_2002 <- papers_2002 %>%
  mutate(author = strsplit(as.character(author), ";\\s*")) %>%
  unnest(author) %>%
  # Identify if the author name contains a comma
  mutate(has_comma = grepl(",", author)) %>%
  # Pre-process author names: append a comma for names with no comma to avoid separation
  mutate(author = if_else(has_comma, author, paste(author, ",", sep = ""))) %>%
  separate(author, into = c("LastName", "FirstName"), sep = "\\s*,\\s*", extra = "merge", fill = "right") %>%
  # Combine first and last name, handling cases with no first name
  unite("author", FirstName, LastName, sep = " ", remove = TRUE) %>%
  # Remove any trailing or leading whitespace that may have been introduced
  mutate(author = trimws(author)) %>%
  # Remove the temporary 'has_comma' column
  select(-has_comma) %>%
  mutate(year = 2002)

##### 2001
papers_2001 <- data.frame(
  author = c(
    "Augustin, Thomas; Coolen, F. P. A.",
    "Augustin, Thomas; Pöhlmann, S.",
    "Augustin, Thomas; Schwarz, R.",
    "Cheng, Chi-Lun; Schneeweiß, Hans",
    "Czado, Claudia",
    "Czado, Claudia; Raftery, A. E.",
    "Czado, Claudia; Song, Peter X.-K.",
    "Einbeck, Jochen",
    "Einbeck, Jochen; Kauermann, Göran",
    "Kandala, N. B.; Lang, S.; Klasen, S.; Fahrmeir, Ludwig",
    "Kauermann, Göran",
    "Kauermann, Göran; Opsomer, J. D.",
    "Kauermann, Göran; Tutz, Gerhard",
    "Klüppelberg, Claudia; Maller, R. A.; Van De Vyver, M.; Wee, D.",
    "Klüppelberg, Claudia; Pergamenchtchikov, S.",
    "Klüppelberg, Claudia; Severin, M.",
    "Knorr-Held, Leonhard; Raßer, Günter; Becker, Nikolaus",
    "Kukush, Alexander; Schneeweiß, Hans; Wolf, R.",
    "Kukush, Alexander; Schneeweiß, Hans; Wolf, R.",
    "Lang, S.; Brezger, Andreas",
    "Lang, S.; Fahrmeir, Ludwig",
    "Lang, S.; Fronk, Eva-Maria; Fahrmeir, Ludwig",
    "Lang, S.; Kragler, P.; Haybach, G.; Fahrmeir, Ludwig",
    "Salanti, G.; Ulm, Kurt",
    "Salanti, G.; Ulm, Kurt",
    "Staubach, C.; Schmid, Volker J.; Ziller, M.; Knorr-Held, Leonhard",
    "Toutenburg, Helge; Shalabh",
    "Toutenburg, Helge; Shalabh",
    "Toutenburg, Helge; Shalabh",
    "Toutenburg, Helge; Shalabh",
    "Toutenburg, Helge; Srivastava, V. K.",
    "Tutz, Gerhard",
    "Tutz, Gerhard",
    "Wolff, J."
  ),
  
  paper = c(
    "Nonparametric predictive inference and interval probability",
    "On Robust Sequential Analysis - Kiefer-Weiss Optimal Testing under Interval Probability",
    "Cox's Proportional Hazards Model under Covariate Measurement Error - A Review and Comparison of Methods",
    "On the Polynomial Measurement Error Model",
    "Individual Migraine Risk Management using Binary State Space Mixed Models",
    "Choosing the Link Function and Accounting for Link Uncertainty in Generalized Linear Models using Bayes Factors",
    "State Space Mixed Models for Longitudinal Observations with Binary and Binomial Responses",
    "Local Fitting with General Basis Functions",
    "Online Monitoring with Local Smoothing Methods and Adaptive Ridging",
    "Semiparametric Analysis of the Socio-Demographic and Spatial Determinants of Undernutrition in Two African Countries",
    "Edge Preserving Smoothing by Local Mixture Modelling",
    "A fast method for implementing Generalized Cross-Validation in multi-dimensional nonparametric regression",
    "Vanishing of Risk Factors for the Success and Survival of Newly Founded Companies",
    "Testing for Reduction to Random Walk in Autoregressive Conditional Heteroskedasticity Models",
    "Renewal Theory for Functionals of a Markov Chain with Compact State Space",
    "Prediction of outstanding insurance claims",
    "Disease Mapping of Stage-specific Cancer Incidence Data",
    "Three Estimators for the Poisson Regression Model with Measurement Errors",
    "Comparison of three estimators in a polynomial regression with measurement errors",
    "Bayesian P-Splines",
    "Bayesian generalized additive mixed models. A simulation study",
    "Function estimation with locally adaptive dynamic models",
    "Bayesian space-time analysis of health insurance data",
    "Modelling Under Order Restrictions",
    "Multidimensional isotonic regression and estimation of the threshold value",
    "A Bayesian Model for Spatial Disease Prevalence Data",
    "Synthesizing the classical and inverse methods in linear calibration",
    "Use of prior information in the form of interval constraints for the improved estimation of linear regression models with some missing responses",
    "Estimation of linear models with missing data: The role of stochastic linear constraints",
    "A note on the comparison of minimax linear and mixed regression estimation of regression coefficients when prior estimates are available",
    "A Revisit to the Application of Weighted Mixed Regression Estimation in Linear Regression Models with Missing Data",
    "Generalized semiparametrically structured mixed models",
    "Generalized semiparametrically structured ordinal models",
    "The Hungarian Unemployment Insurance Benefit System and Incentives to Return to Work"
  ),
  
  id = c(257, 261, 260, 259, 235, 262, 232, 256, 248, 245, 255, 247, 242, 266, 264, 258, 249, 243, 233, 236, 230, 263, 237, 265, 234, 254, 252, 240, 239, 238, 241, 251, 250, 253)
)

papers_2001 <- papers_2001 %>%
  mutate(author = strsplit(as.character(author), ";\\s*")) %>%
  unnest(author) %>%
  # Identify if the author name contains a comma
  mutate(has_comma = grepl(",", author)) %>%
  # Pre-process author names: append a comma for names with no comma to avoid separation
  mutate(author = if_else(has_comma, author, paste(author, ",", sep = ""))) %>%
  separate(author, into = c("LastName", "FirstName"), sep = "\\s*,\\s*", extra = "merge", fill = "right") %>%
  # Combine first and last name, handling cases with no first name
  unite("author", FirstName, LastName, sep = " ", remove = TRUE) %>%
  # Remove any trailing or leading whitespace that may have been introduced
  mutate(author = trimws(author)) %>%
  # Remove the temporary 'has_comma' column
  select(-has_comma) %>%
  mutate(year = 2001)

##### 2000
papers_2000 <- data.frame(
  author = c(
    "Augustin, Thomas",
    "Berger, Ursula; Gerein, Pia; Ulm, Kurt; Schäfer, Juliane",
    "Blauth, A.; Pigeot, Iris",
    "Briegel, T.; Tresp, V.",
    "Didelez, V.; Pigeot, Iris; Dean, K.; Wister, A.",
    "Didelez, V.; Pigeot, Iris; Walter, P.",
    "Fahrmeir, Ludwig; Lang, S.",
    "Fahrmeir, Ludwig; Lang, S.; Wolff, J.; Bender, S.",
    "Fronk, Eva-Maria; Giudici, P.",
    "Galindo, C. D.; Kauermann, Göran; Liang, H.; Carroll, R. J.",
    "Gartner, H.",
    "Gartner, H.",
    "Gössl, Christoff; Auer, Dorothee P.; Fahrmeir, Ludwig",
    "Hahn, Klaus; Rodenacker, K.; Aurich, Volker; Auer, Dorothee P.",
    "Heumann, Christian",
    "Kauermann, Göran; Carroll, R. J.",
    "Kauermann, Göran; Opsomer, J. D.",
    "Kauermann, Göran; Tutz, Gerhard",
    "Klasen, S.",
    "Klasen, S.; Moradi, A.",
    "Klasen, S.; Woolard, I.",
    "Klasen, Stephan",
    "Knorr-Held, Leonhard; Best, N. G.",
    "Knorr-Held, Leonhard; Rue, H.",
    "Kukush, Alexander; Maschke, Erich Otto",
    "Kukush, Alexander; Schneeweiß, Hans",
    "Lang, S.; Brezger, Andreas",
    "Marx, Brian D.",
    "Pashova, Victoria; Ulm, Kurt",
    "Schneeweiß, Hans; Nittner, T.",
    "Seifert-Klauss, V.; Mueller, J. E.; Probst, R.; Wilker, J.; Höß, C.; Treumann, T.; Kastner, C.; Ulm, Kurt; Luppa, P.",
    "Storck, S.",
    "Storck, S.; Kastner, C.; Toutenburg, Helge",
    "Toutenburg, Helge; Fieger, A.",
    "Toutenburg, Helge; Srivastava, V. K.",
    "Toutenburg, Helge; Srivastava, V. K.",
    "Tutz, Gerhard; Scholz, T.",
    "Ulm, Kurt; Schmidt, Georg; Barthel, Petra; Schneider, Raphael; Pashova, Victoria; Rolnitzky, Linda; Bigger Jr., J. Thomas; Schömig, Albert",
    "Wolff, J.; Augustin, Thomas",
    "Ziegler, Andreas; Kastner, C.",
    "Ziegler, Andreas; Kastner, C.; Chang-Claude, J."
  ),
  
  paper = c(
    "Some Basic Results on the Extension of Quasi-Likelihood Based Measurement Error Correction to Multivariate and Flexible Structural Models",
    "On the use of Fractional Polynomials in Dynamic Cox Models",
    "GraphFitI - A computer program for graphical chain models",
    "Dynamic Neural Regression Models",
    "A comparative analysis of graphical interaction and logistic regression modelling: self-care and coping with a chronic illness in later life",
    "Modifications of the Bonferroni-Holm procedure for a multi-way ANOVA",
    "Bayesian Semiparametric Regression Analysis of Multicategorical Time-Space Data",
    "Semiparametric Bayesian Time-Space Analysis of Unemployment Duration",
    "Markov Chain Monte Carlo Model Selection for DAG Models",
    "Bootstrap Confidence Intervals For Local Likelihood, Local Estimating Equations And Varying Coefficient Models",
    "Die Ersetzung fehlender Werte: Ein Test alternativer Methoden mit Makrodaten",
    "Das Auszugsverhalten junger Menschen aus dem Elternhaus in Westdeutschland seit 1984",
    "Bayesian spatio-temporal inference in functional magnetic resonance imaging",
    "Segmentierung des Gehirns auf der Basis von MR-Daten",
    "Intention-to-treat with drop-out",
    "The Sandwich Variance Estimator: Efficiency Properties and Coverage Probability of Confidence Intervals",
    "Local Likelihood Estimation in Generalized Additive Models",
    "Semiparametric Modeling of Ordinal Data",
    "Malnourished and surviving in South Asia, better nourished and dying young in Africa: What can explain this puzzle?",
    "The Nutritional Status of Elites in India, Kenya, and Zambia: An appropriate guide for developing reference standards for undernutrition?",
    "Surviving Unemployment without State Support: Unemployment and Household Formation in South Africa",
    "Does Gender Inequality Reduce Growth and Development? Evidence from Cross-Country Regressions",
    "Shared component models for detecting joint and selective clustering of two diseases",
    "On block updating in Markov random field models for disease mapping. (REVISED, May 2001)",
    "The Efficiency of Adjusted Least Squares in the Linear Functional Relationship",
    "A Comparison of Asymptotic Covariance Matrices of Adjusted Least Squares and Structural Least Squares in Error Ridden Polynomial Regression",
    "BayesX - Software for Bayesian Inference based on Markov Chain Monte Carlo simulation techniques",
    "On Ill-Conditioned Generalized Estimating Equations and Toward Unified Biased Estimation",
    "Two Survival Tree Models for Myocardial Infarction Patients",
    "Estimating A Polynomial Regression With Measurement Errors In The Structural And In The Functional Case - A Comparison",
    "Bone Metabolism During the Perimenopausal Transition: a Prospective Study",
    "Pattern mixture models for multivariate normal data: a simulation study",
    "Longitudinal data with dropouts: a comparison of pattern mixture models with complete case analysis",
    "Using diagnostic measures to detect non-MCAR processes in linear regression models with missing covariates",
    "Estimation of Linear Regression Models with Missingness of Observations on Both the Explanatory and Study Variables-Part I: Theoretical Results",
    "Efficient Estimation of Population Mean Using Incomplete Survey Data on Study and Auxiliary Characteristics",
    "Semiparametric Modelling of Multicategorical Data",
    "A Statistical Model for Risk Stratification on the Basis of Left Ventricular Ejection Fraction and Heart-Rate Turbulence",
    "Heaping and its Consequences for Duration Analysis",
    "The Effect of Misspecified Response Probabilities on Parameter Estimates from Weighted Estimating Equations",
    "Analysis of Pregnancy and Other Factors on Detection of Human Papilloma Virus (HPV) Infection Using Weighted Estimating Equations for Follow-Up Data"
  ),
  
  id = c(196, 207, 224, 181, 186, 185, 202, 211, 221, 205, 216, 215, 192, 180, 199, 189, 190, 193, 214, 217, 213, 212, 183, 210, 208, 218, 187, 182, 195, 197, 194, 191, 188, 204, 184, 179, 209, 198, 203, 200, 201)
)

papers_2000 <- papers_2000 %>%
  mutate(author = strsplit(as.character(author), ";\\s*")) %>%
  unnest(author) %>%
  mutate(has_comma = grepl(",", author)) %>%
  mutate(author = if_else(has_comma, author, paste(author, ",", sep = ""))) %>%
  separate(author, into = c("LastName", "FirstName"), sep = "\\s*,\\s*", extra = "merge", fill = "right") %>%
  unite("author", FirstName, LastName, sep = " ", remove = TRUE) %>%
  mutate(author = trimws(author)) %>%
  select(-has_comma) %>%
  mutate(year = 2000)

##### 1999
papers_1999 <- data.frame(
  # Create vectors for authors, papers, and IDs
  author = c(
    "Augustin, Thomas", "Augustin, Thomas", "Caputo, Angelika", 
    "Didelez, V.", "Dreesman, J.; Tutz, Gerhard", 
    "Eberle, W.; Toutenburg, Helge", "Fahrmeir, Ludwig; Lang, S.", 
    "Friedl, H.; Kauermann, Göran", "Gieger, Christian", 
    "Giudici, P.; Knorr-Held, Leonhard; Rasser, G.", 
    "Gössl, Christoff; Küchenhoff, Helmut", 
    "Kastner, C.; Fieger, A.; Heumann, Christian", 
    "Kastner, C.; Ziegler, Andreas", "Kauermann, Göran", 
    "Kauermann, Göran", "Kauermann, Göran; Tutz, Gerhard", 
    "Klinger, Artur", "Knorr-Held, Leonhard", 
    "Knorr-Held, Leonhard; Becker, Nikolaus", 
    "Knorr-Held, Leonhard; Rainer, E.", 
    "Knorr-Held, Leonhard; Rasser, G.", "Nikele, M.; Fahrmeir, Ludwig", 
    "Pruscha, H.; Göttlein, A.", "Rose, Colin; Smith, Murray D.", 
    "Schneeweiß, Hans", "Shao, J.; Kübler, J.; Pigeot, Iris", 
    "Srivastava, V. K.; Toutenburg, Helge", 
    "Toutenburg, Helge; Fieger, A.; Schaffrin, B.", 
    "Toutenburg, Helge; Nittner, T.", 
    "Toutenburg, Helge; Shalabh", "Toutenburg, Helge; Shalabh", 
    "Toutenburg, Helge; Shalabh", "Toutenburg, Helge; Srivastava, V. K.", 
    "Trevisani, M.; Causin, R.; Montecchio, L.; Kastner, C.; Heumann, Christian", 
    "Tutz, Gerhard; Edlich, Silke; Bäumer, Christoph", 
    "Winkler, Gerhard; Liebscher, V.; Aurich, Volker", 
    "Ziegler, Andreas; Kastner, C.; Brunner, D.; Blettner, Maria"
  ),
  
  paper = c(
    "Neyman-Pearson Testing under Interval Probability by Globally Least Favorable Pairs: A Survey of Huber-Strassen Theory and Some Results on its Extension to General Interval Probability", 
    "Correcting for measurement error in parametric duration models by quasi-likelihood", 
    "Factorization of the Cumulative Distribution Function in Case of Conditional Independence. (REVISED, November 1999)", 
    "Local independence graphs for composable Markov processes", 
    "Nonstationary conditional models for spatial data based on varying coefficients", 
    "Handling of missing values in statistical software packages for windows", 
    "Bayesian Inference for Generalized Additive Mixed Models Based on Markov Random Field Priors", 
    "Standard Errors for EM Estimates in Variance Component Models", 
    "Marginal Regression Models with Varying Coefficients for Correlated Ordinal Data", 
    "Modelling categorical covariates in Bayesian disease mapping by partition structures", 
    "Bayesian analysis of logistic regression with an unknown change point", 
    "New features in MAREG 0.2.0", 
    "A Comparison of Jackknife Estimators of Variance for GEE2", 
    "On a Small Sample Adjustment for the Profile Score Function in Semiparametric Smoothing Models", 
    "Modeling longitudinal data with ordinal response by varying coefficients", 
    "Testing Generalized Linear and Semiparametric Models Against Smooth Alternatives", 
    "Inference in High Dimensional Generalized Linear Models based on Soft-Thresholding", 
    "Bayesian Modelling of Inseparable Space-Time Variation in Disease Risk", 
    "Bayesian Modelling of Spatial Heterogeneity in Disease Maps with Application to German Cancer Mortality Data", 
    "Prognosis of Lung Cancer Mortality in West Germany: A Case Study in Bayesian Prediction. (REVISED, January 2000)", 
    "Bayesian Detection of Clusters and Discontinuities in Disease Maps: Simulations. (REVISED, June 1999)", 
    "A latent variable probit model for multivariate ordered categorical responses", 
    "Regression Analysis for Forest Inventory Data with Time and Space Dependencies", 
    "Symbolic Maximum Likelihood Estimation with Mathematica", 
    "Resolving the Ellsberg Paradox by Assuming that People Evaluate Repetitive Sampling", 
    "Consistency of the Bootstrap Procedure in Individual Bioequivalence", 
    "On the First Order Regression Procedure of Estimation for Incomplete Regression Models", 
    "Approximate Confidence Regions for Minimax-Linear Estimators", 
    "The Classical Linear Regression Model with one Incomplete Binary Variable", 
    "Estimation of Regression Models with Equi-correlated Responses when some Observations on the Response Variable are Missing", 
    "Estimation of Regression Coefficients Subject to Exact Linear Restrictions when some Observations are Missing and Balanced Loss Function is Used", 
    "Improving the Estimation of Incomplete Regression Models through Pilot Investigations and Repeated Studies", 
    "Amputation versus Imputation of Missing Values through Ratio Method in Sample Surveys", 
    "Analysing the relationship between ectomycorrhizal infection and forest decline using marginal models", 
    "Comparison between local estimates for multi-categorical varying-coefficent models", 
    "Smoothers for Discontinuous Signals", 
    "Familial Associations of Lipid Profiles: A Generalised Estimating Equations Approach"
  ),
  
  id = c(172, 157, 161, 158, 150, 170, 134, 145, 177, 152, 148, 156, 167, 162, 144, 149, 165, 147, 176, 160, 142, 164, 173, 141, 153, 159, 175, 166, 178, 174, 163, 154, 155, 143, 151, 146, 168)
)

papers_1999 <- papers_1999 %>%
  mutate(author = strsplit(as.character(author), ";\\s*")) %>%
  unnest(author) %>%
  mutate(has_comma = grepl(",", author)) %>%
  mutate(author = if_else(has_comma, author, paste(author, ",", sep = ""))) %>%
  separate(author, into = c("LastName", "FirstName"), sep = "\\s*,\\s*", extra = "merge", fill = "right") %>%
  unite("author", FirstName, LastName, sep = " ", remove = TRUE) %>%
  mutate(author = trimws(author)) %>%
  select(-has_comma) %>%
  mutate(year = 1999)


##### 1998
papers_1998 <- data.frame(
  # Create vectors for authors, papers, and IDs
  author = c(
    "Bauer, Thomas; Million, A.; Rotte, Ralph; Zimmermann, Klaus F.", 
    "Biller, Clemens", 
    "Caputo, Angelika", "Caputo, Angelika", "Caputo, Angelika", 
    "Cheng, Chi-Lun; Schneeweiß, Hans; Thamerus, Markus", 
    "Didelez, V.", 
    "Fahrmeir, Ludwig", "Fahrmeir, Ludwig; Künstler, R.", 
    "Fahrmeir, Ludwig; Lang, S.", 
    "Fieger, A.; Kastner, C.; Heumann, Christian", 
    "Fronk, Eva-Maria; Fahrmeir, Ludwig", 
    "Gössl, Christoff; Auer, Dorothee P.; Fahrmeir, Ludwig", 
    "Hahn, Klaus; Waschulzik, T.", 
    "Heumann, Christian; Fieger, A.; Kastner, C.", 
    "Knorr-Held, Leonhard; Rasser, G.", 
    "Mayer, Jochen; Riphahn, Regina T.", 
    "Pigeot, Iris; Blauth, Angelika; Bry, François", 
    "Pruscha, H.", 
    "Schaffrin, B.; Toutenburg, Helge", 
    "Smith, Murray D.", "Smith, Murray D.; Moffatt, P. G.", 
    "Spiess, M.; Keller, F.", 
    "Toutenburg, Helge; Fieger, A.; Heumann, Christian", 
    "Toutenburg, Helge; Fieger, A.; Srivastava, V. K.", 
    "Toutenburg, Helge; Shalabh", "Toutenburg, Helge; Shalabh", 
    "Toutenburg, Helge; Shalabh", 
    "Toutenburg, Helge; Srivastava, V. K.", 
    "Toutenburg, Helge; Srivastava, V. K.", 
    "Toutenburg, Helge; Srivastava, V. K.", 
    "Toutenburg, Helge; Srivastava, V. K.; Schaffrin, B.", 
    "Ulm, Kurt; Dannegger, Felix; Becker, Ursula", 
    "Winkler, Gerhard", "Winkler, Gerhard", 
    "Winkler, Gerhard; Aurich, Volker; Hahn, Klaus; Martin, A.; Rodenacker, K.", 
    "Winkler, Gerhard; Hahn, Klaus; Aurich, Volker", 
    "Ziegler, Andreas; Kastner, C."
  ),
  
  paper = c(
    "Immigrant Labor and Workplace Safety", 
    "Adaptive Bayesian Regression Splines in Semiparametric Generalized Linear Models", 
    "Decomposition of ML Estimation in Graphical Models with Koehler Symanowski distributions", 
    "Graphical models with Koehler Symanowski distributions", 
    "Some properties of the family of Koehler Symanowski distributions", 
    "A Small Sample Estimator for a Polynomial Regression with Errors in the Variables", 
    "Maximum Likelihood and Semiparametric Estimation in Logistic Models with Incomplete Covariate Data", 
    "Recent Advances in Semiparametric Bayesian Function Estimation", 
    "Penalized likelihood smoothing in robust state space models", 
    "Bayesian Inference for Generalized Additive Regression based on Dynamic Models", 
    "WinMAREG Quick Start", 
    "Function estimation with locally adaptive dynamic models", 
    "Dynamic models in fMRI", 
    "On the Use of Local RBF Networks to Approximate Multivalued Functions and Relations", 
    "C++ Utilities zur Implementierung statistischer Verfahren unter Berücksichtigung fehlender Werte", 
    "Bayesian Detection of Clusters and Discontinuities in Disease Maps. (REVISED, February 1999)", 
    "Fertility Assimilation of Immigrants: A Varying Coefficient Count Data Model", 
    "Interactive analysis of high-dimensional association structures with graphical models", 
    "Semiparametric Point Process and Time Series Models for Series of Events", 
    "The Impact of Missing Values on the Reliability Measures in a Linear Model", 
    "On Dependency in Double-Hurdle Models", 
    "Fisher's Information on the Correlation Coefficient in Bivariate Models", 
    "A mixed approach and a distribution free multiple imputation technique for the estimation of multivariate probit models with missing values", 
    "Regression modelling with fixed effects - missing values and other problems", 
    "Weighted Modified First Order Regression Procedures for Estimation in Linear Models with Missing X-Observations", 
    "Improved Predictions in Linear Regression Models with Stochastic Linear Constraints", 
    "Use of minimum risk approach in the estimation of regression models with missing observation", 
    "Prediction of Response Values in Linear Regression Models from Replicated Experiments", 
    "Impact of Departure from Normality on the Efficiency of Estimating Regression Coefficients when Some Observations are Missing", 
    "Improving the Estimation of Coefficients in Linear Regression Models with Some Missing Observations on Some Explanatory Variables", 
    "Estimation of Ratio of Population Means in Survey Sampling When Some Observations are Missing", 
    "Efficiency properties of weighted mixed regression estimation", 
    "Tests for Trends in Binary Response", 
    "Moment Sets of Bell-Shaped Distributions: Extreme Points, Extremal Decomposition and Chebysheff Inequalities", 
    "An Adaptive Gradient Algorithm for Maximum Likelihood Estimation in Imaging. A Tutorial", 
    "Noise Reduction in Images: Some Recent Edge-Preserving Methods", 
    "A Brief Survey of Recent Edge-Preserving Smoothers", 
    "Solving Generalised Estimating Equations With Missing Data Using Pseudo Maximum Likelihood Estimation Is Equivalent to Complete Case Analysis"
  ),
  
  id = c(117, 133, 105, 104, 103, 113, 110, 137, 111, 134, 108, 135, 136, 116, 109, 107, 140, 131, 114, 125, 139, 126, 102, 123, 127, 124, 118, 112, 130, 129, 119, 122, 115, 121, 120, 138, 132, 128)
)

papers_1998 <- papers_1998 %>%
  mutate(author = strsplit(as.character(author), ";\\s*")) %>%
  unnest(author) %>%
  mutate(has_comma = grepl(",", author)) %>%
  mutate(author = if_else(has_comma, author, paste(author, ",", sep = ""))) %>%
  separate(author, into = c("LastName", "FirstName"), sep = "\\s*,\\s*", extra = "merge", fill = "right") %>%
  unite("author", FirstName, LastName, sep = " ", remove = TRUE) %>%
  mutate(author = trimws(author)) %>%
  select(-has_comma) %>%
  mutate(year = 1998)

##### 1997
papers_1997 <- data.frame(
  # Create vectors for authors, papers, and IDs
  author = c(
    "Aydemir, Sibel; Biller, Clemens", 
    "Aydemir, Sibel; Hornsteiner, U.", 
    "Bauer, Thomas; Rotte, Ralph", 
    "Bellmann, L.; Bender, S.; Hornsteiner, U.", 
    "Biller, Clemens", "Biller, Clemens", 
    "Caputo, Angelika; Heinicke, A.; Pigeot, Iris", 
    "Cheng, Chi-Lun; Schneeweiß, Hans", 
    "Dannegger, Felix", 
    "Daumer, Martin; Bauer, C.; Lederer, C.", 
    "Didelez, V.; Pigeot, Iris", 
    "Fahrmeir, Ludwig", 
    "Fahrmeir, Ludwig; Gieger, Christian; Heumann, Christian", 
    "Fahrmeir, Ludwig; Klinger, Artur", 
    "Fahrmeir, Ludwig; Knorr-Held, Leonhard", 
    "Fieger, A.", 
    "Fieger, A.", 
    "Fieger, A.; Heumann, Christian; Kastner, C.; Watzka, K.", 
    "Gieger, Christian", 
    "Haybach, G.; Küchenhoff, Helmut", 
    "Hornsteiner, U.; Hamerle, Alfred; Michels, P.", 
    "Kastner, C.; Ziegler, Andreas", 
    "Klinger, Artur", 
    "Knorr-Held, Leonhard", 
    "Küchenhoff, Helmut; Wellisch, U.", 
    "Luhm, A.; Pruscha, H.", 
    "Pan, Guohua; Santner, Thomas J.", 
    "Pigeot, Iris; Heinicke, A.; Caputo, Angelika; Brüderl, Josef", 
    "Pruscha, H.", 
    "Pruscha, H.; Ulm, Kurt; Schmidt, G.", 
    "Pruscha, H.; Wellisch, U.", 
    "Santner, Thomas J.", 
    "Schneeweiß, Hans", 
    "Spatz, R.; Hamerle, Alfred", 
    "Spiess, M.; Nagl, W.; Hamerle, Alfred", 
    "Stark, M.", 
    "Thamerus, Markus", 
    "Thamerus, Markus", 
    "Toutenburg, Helge; Srivastava, V. K.; Fieger, A.", 
    "Ziegler, Andreas; Blettner, Maria; Kastner, C.; Chang-Claude, J.", 
    "Ziegler, Andreas; Kastner, C."
  ),
  
  paper = c(
    "Kernel smoothing of Aalen's linear regression model", 
    "Analyse zeitveraenderlicher Kovariablen und rekurrenter Ereignisse am Beispiel einer Studie zur prophylaktischen Behandlung von Oesophagusvarizen", 
    "Prospect Theory Goes to War: Loss-Aversion and the Duration of Military Combat", 
    "Interfirm Job Mobility of Two Cohorts of Young German Men 1979 - 1990: An analysis of the (West-)German Employment Statistic Register Sample concerning multivariate failure times and unobserved heterogeneity", 
    "Discrete Duration Models combining Dynamic and Random Effects", 
    "Posterior mode estimation in dynamic generalized linear mixed models", 
    "A graphical chain model derived from a model selection strategy for the sociologists graduates study", 
    "Note on Two Estimators for the Polynomial Regression with Errors in the Variables", 
    "Tree stability diagnostics and some remedies against instability", 
    "Average Run Length and Mean Delay for Changepoint Detection: Robust Estimates for Threshold Alarms", 
    "Maximum Likelihood Estimation in Graphical Models with Missing Values", 
    "Discrete failure time models", 
    "An application of isotonic longitudinal marginal regression to monitoring the healing process", 
    "A nonparametric multiplicative hazard model for event history analysis (strongly modified and revised version of Discussion Paper 12)", 
    "Dynamic and semiparametric models", 
    "Modified First Order Regression, eine Simulationsstudie", 
    "C++ Klassen zur Linearen Regression bei fehlenden Kovariablen", 
    "Generische Bibliothek zur Linearen Algebra und zur Simulation in C++", 
    "Non- and semiparametric marginal regression models for ordinal response", 
    "Testing for a Breakpoint in Two-Phase Linear and Logistic Regression Models", 
    "Parametric versus Nonparametric Treatment of Unobserved Heterogeneity in Multivariate Failure Times", 
    "Cross-sectional Analysis of Longitudinal Data with Missing Values in the Dependent Variables: A Comparison of Weighted Estimating Equations with the Complete Case Analysis", 
    "Generalized Soft-Thresholding and Varying-coefficient Models", 
    "Dynamic Rating of Sports Teams", 
    "Asymptotics for generalized linear segmented regression models with an unknown breakpoint", 
    "Semi-parametric Inference for Regression Models Based on Marked Point Processes", 
    "Screening Procedures to Identify Robust Product or Process Designs Using Fractional Factorial Experiments", 
    "The professional career of sociologists: a graphical chain model reflecting early influences and associations", 
    "Semiparametric Estimation in Regression Models for Point Processes based on One Realization", 
    "Statistische Analyse des Einflusses von Herzrhythmusstörungen auf das Mortalitätsrisiko", 
    "Asymptotic behaviour of estimation equations with functional nuisance or working parameter", 
    "A Note on Teaching Binomial Confidence Intervals", 
    "Korrigierte Schaetzgleichungen fuer allgemeine Regressionsmodelle mit Fehlern in den Variablen", 
    "Ein Mehr-Zustands-Mehr-Episoden-Modell in diskreter Zeit zur Analyse klinischer Studien unter Beruecksichtigung unbeobachteter Heterogenitaet", 
    "Probit models: Regression parameter estimation using the ML principle despite misspecification of the correlation structure", 
    "Zu Vorhersage und Vorhersagewert fuer Ueberlebenszeitmodelle", 
    "Different Nonlinear Regression Models with Incorrectly Observed Covariates", 
    "Modelling Count Data with Heteroscedastic Measurement Error in the Covariates", 
    "Shrinkage Estimation of Incomplete Regression Models by Yates Procedure", 
    "Identifying Influential Families Using Regression Diagnostics For Generalized Estimating Equations", 
    "A Minimum Distance Estimation Approach to Estimate the Recombination Fraction from a Marker Locus in Robust Linkage Analysis for Quantitative Traits"
  ),
  
  id = c(101, 81, 97, 94, 88, 70, 73, 96, 72, 95, 75, 91, 89, 12, 76, 62, 61, 63, 71, 77, 80, 64, 59, 98, 83, 78, 92, 74, 66, 56, 79, 87, 93, 82, 67, 84, 68, 58, 69, 60, 65)
)

papers_1997 <- papers_1997 %>%
  mutate(author = strsplit(as.character(author), ";\\s*")) %>%
  unnest(author) %>%
  mutate(has_comma = grepl(",", author)) %>%
  mutate(author = if_else(has_comma, author, paste(author, ",", sep = ""))) %>%
  separate(author, into = c("LastName", "FirstName"), sep = "\\s*,\\s*", extra = "merge", fill = "right") %>%
  unite("author", FirstName, LastName, sep = " ", remove = TRUE) %>%
  mutate(author = trimws(author)) %>%
  select(-has_comma) %>%
  mutate(year = 1997)


##### 1996
papers_1996 <- data.frame(
  author = c("Aydemir, Sibel; Aydemir, Ülker; Dirschedl, Peter",
             "Aydemir, Sibel; Aydemir, Ülker; Dirschedl, Peter",
             "Biller, Clemens; Fahrmeir, Ludwig",
             "Czado, Claudia",
             "Czado, Claudia; Munk, Axel",
             "Daumer, Martin; Falk, M.; Beyer, U.",
             "Fahrmeir, Ludwig; Knorr-Held, Leonhard",
             "Fieger, A.; Heumann, Christian; Kastner, C.",
             "Geil, Peter; Rotte, Ralph",
             "Hamerle, Alfred; Moller, M.",
             "Heumann, Christian",
             "Hornsteiner, U.; Hamerle, Alfred",
             "Johansson, P.; Brännäs, K.",
             "Knorr-Held, Leonhard",
             "Küchenhoff, Helmut",
             "Pan, Guohua; Santner, Thomas J.",
             "Pruscha, H.",
             "Rieder, H.",
             "Santner, Thomas J.; Pan, Guohua",
             "Schuster, G.",
             "Schuster, G.",
             "Schöpp, A.; Toutenburg, Helge",
             "Spiess, M.; Hamerle, Alfred",
             "Spiess, M.; Hamerle, Alfred",
             "Stark, M.; Wagenpfeil, Stefan; Nekarda, H.",
             "Thamerus, Markus",
             "Toutenburg, Helge; Srivastava, V. K.; Fieger, A.",
             "Ulm, Kurt; Dannegger, Felix; Spanier, M.",
             "Vach, W.; Illi, S.",
             "Veall, Michael R.; Zimmermann, Klaus F.",
             "Wagenpfeil, Stefan",
             "Windmeijer, F. A. G.; Santos Silva, J. M. C.",
             "Winkler, B.",
             "Ziegler, Andreas; Kastner, C.; Grömping, U.; Blettner, Maria"
  ),
  paper = c("Survivalanalysen mit Berücksichtigung der zeitlichen Kovariablenentwicklung in klinischen Studien",
            "Das lineare Regressionsmodell von Aalen zur Analyse von Überlebenszeiten unter Berücksichtigung zeitveränderlicher Kovariablen",
            "Bayesian spline-type smoothing in generalized regression models",
            "Multivariate Probit Analysis of Binary Time Series Data with Missing Responses",
            "Noncanonical Links in Generalized Linear Models - When is the Effort Justified?",
            "On-line monitoring using Multi-Process Kalman Filtering",
            "Dynamic discrete-time duration models. (REVISED)",
            "MAREG and WinMAREG",
            "International Interventionism 1970-1989: A Count Data Approach",
            "Semiparametric EM-estimation of censored linear regression models for durations",
            "Marginal regression modeling of correlated multicategorical response: A likelihood approach",
            "A Combined GEE/Buckley-James Method for Estimating an Accelerated Failure Time Model of Multivariate Failure Times",
            "A Household Model for Work Absence",
            "Conditional Prior Proposals in Dynamic Models",
            "An exact algorithm for estimating breakpoints in segmented generalized linear models",
            "Selection and Screening Procedures to Determine Optimal Product Designs. (REVISED, April 1997)",
            "Residual and forecast methods in time series models with covariates",
            "Estimation of Mortalities",
            "The Use of Subset Selection in Combined Array Experiments to Determine Optimal Product or Process Designs. (REVISED, June 1997)",
            "ML-Estimation from Binomial Data with Misclassifications. A Comparison: Internal Validation versus Repeated Measurements",
            "ML-Estimation in a Case-Control Study with Measurement Error in the Risk Factor. A Comparison: External Validation versus Repeated Measurements",
            "Das symmetrische konditionale Regressionsmodell - alternative Parametrisierung bei korrelierten binären Responsevariablen",
            "Estimation of multivariate probit models: A mixed generalized estimating/pseudo-score equations approach and some finite sample results",
            "On the properties of GEE estimators in the presence of invariant covariates",
            "Estimating Time-Varying Effects of Prognostic Factors for Stomach Cancer Patients within a Dynamic Grouped Cox Model",
            "Fitting a Finite Mixture Distribution to a Variable Subject to Heteroscedastic Measurement Error",
            "Estimation of Parameters in Multiple Regression With Missing X-Observations using Modified First Order Regression Procedure",
            "Bericht über die Auswertung der Daten der Studie 'Chronische Bronchitis'",
            "Biased Estimation of Adjusted Odds Ratios From Incomplete Covariate Data Due to Violation of the Missing at Random Assumption",
            "Pseudo-R2 Measures for Some Common Limited Dependent Variable Models",
            "Dynamische Modelle zur Ereignisanalyse",
            "Estimation of count data models with endogenous regressors; an application to demand for health care",
            "Bootstrapping Goodness of Fit Statistics in Loglinear Poisson Models",
            "The Generalized Estimating Equations in the Past Ten Years: An Overview and A Biomedical Application"
  ),
  id = c(44, 25, 31, 23, 22, 54, 14, 45, 21, 15, 19, 47, 41, 36, 27, 52, 33, 28, 51, 50, 43, 37, 46, 13, 29, 48, 38, 39, 17, 18, 32, 20, 53, 24)
)

papers_1996 <- papers_1996 %>%
  mutate(author = strsplit(as.character(author), ";\\s*")) %>%
  unnest(author) %>%
  mutate(has_comma = grepl(",", author)) %>%
  mutate(author = if_else(has_comma, author, paste(author, ",", sep = ""))) %>%
  separate(author, into = c("LastName", "FirstName"), sep = "\\s*,\\s*", extra = "merge", fill = "right") %>%
  unite("author", FirstName, LastName, sep = " ", remove = TRUE) %>%
  mutate(author = trimws(author)) %>%
  select(-has_comma) %>%
  mutate(year = 1996)

#### 1995
papers_1995 <- data.frame(
  author = c("Dannegger, Felix; Klinger, Artur; Ulm, Kurt",
             "Fahrmeir, Ludwig; Gieger, Christian; Klinger, Artur",
             "Fahrmeir, Ludwig; Klinger, Artur",
             "Fahrmeir, Ludwig; Pritscher, L.",
             "Fahrmeir, Ludwig; Wagenpfeil, Stefan",
             "Fahrmeir, Ludwig; Wagenpfeil, Stefan",
             "Knorr-Held, Leonhard",
             "Knorr-Held, Leonhard",
             "Küchenhoff, Helmut; Thamerus, Markus",
             "Pruscha, Helmut",
             "Spiess, M.; Hamerle, Alfred",
             "Wagenpfeil, Stefan"),
  paper = c("Identification of Prognostic Factors with Censored Data",
            "Additive, Dynamic and Multiplicative Regression",
            "A nonparametric multiplicative hazard model for event history analysis",
            "Regression analysis of forest damage by marginal models for correlated ordinal responses",
            "Smoothing Hazard Functions and Time-Varying Effects in Discrete Duration and Competing Risks Models",
            "Penalized likelihood estimation and iterative kalman smoothing for non-gaussian dynamic regression models",
            "Markov Chain Monte Carlo Simulation in Dynamic Generalized Linear Mixed Models",
            "Dynamic Cumulative Probit Models for Ordinal Panel-Data; a Bayesian Analysis by Gibbs Sampling",
            "Extreme value analysis of Munich airpollution data",
            "Some Forecast Methods in Regression Models for Categorical Time Series",
            "Regression Models with Correlated Binary Response Variables: A Comparison of Different Methods in Finite Samples",
            "Hyperparameter Estimation in Exponential Family State Space Models"),
  id = c(11, 1, 12, 9, 7, 5, 8, 2, 4, 3, 10, 6)
)

papers_1995 <- papers_1995 %>%
  mutate(author = strsplit(as.character(author), ";\\s*")) %>%
  unnest(author) %>%
  mutate(has_comma = grepl(",", author)) %>%
  mutate(author = if_else(has_comma, author, paste(author, ",", sep = ""))) %>%
  separate(author, into = c("LastName", "FirstName"), sep = "\\s*,\\s*", extra = "merge", fill = "right") %>%
  unite("author", FirstName, LastName, sep = " ", remove = TRUE) %>%
  mutate(author = trimws(author)) %>%
  select(-has_comma) %>%
  mutate(year = 1995)

author <- c("Stefan Lang", "S. Lang", "Jane Doe", "J. Doe", "Alice Wonderland", "A. Wonderland")
df <- data.frame(author)
df <- author_id %>%
  mutate(LastName = sapply(strsplit(as.character(author), " "), `[`, 2),
         FirstName = sapply(strsplit(as.character(author), " "), `[`, 1))
# Function to detect abbreviated names
is_abbrev_match <- function(fullname, abbrev) {
  lname_full <- tail(strsplit(fullname, " ")[[1]], 1)
  fname_full <- head(strsplit(fullname, " ")[[1]], 1)
  
  lname_abbrev <- tail(strsplit(abbrev, " ")[[1]], 1)
  fname_abbrev <- head(strsplit(abbrev, " ")[[1]], 1)
  
  # Check if last names match and first name is an abbreviation
  if (lname_full == lname_abbrev && grepl(paste0("^", substr(fname_full, 1, 1), "\\."), fname_abbrev)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Apply the function across all combinations of author names to find matches
df$Matched = FALSE
for(i in 1:nrow(df)) {
  for(j in 1:nrow(df)) {
    if(i != j && is_abbrev_match(df$author[i], df$author[j])) {
      df$Matched[i] <- TRUE
    }
  }
}

df <- data.frame(author = c("S. Lang", "Stefan Lang", "J. Doe", "John Doe", "A. Smith", "Alice Smith"),
                 id = 1:6)

author_id$normalized_author <- sapply(author_id$author, normalize_name)
duplicates <- author_id[duplicated(author_id$normalized_author) | duplicated(author_id$normalized_author, fromLast = TRUE), ]

add_numbering <- function(data) {
  process_element <- function(element) {
    # Find a match in the name_map
    match <- author_id$author == element
    if (any(match)) {
      # If a match is found, return the corresponding full name and numbering
      return(paste(element, ' (', author_id$author_id[match], ')', sep=''))
    } else {
      # If no match is found, return the original value
      return(element)
    }
  }

  if (is.list(data) | is.vector(data)) {
    sapply(data, process_element)
  } else if (is.data.frame(data)) {
    for (i in 1:ncol(data)) {
      data[[i]] <- sapply(data[[i]], process_element)
    }
  }
}

#### Betweeness centrality
betweenness_centrality <- betweenness(graph, normalized = TRUE)

betweenness_centrality_df <- data.frame(centrality = betweenness_centrality)
betweenness_centrality_df <- betweenness_centrality_df %>%
  mutate(name = row.names(betweenness_centrality_df)) %>%
  select(name, centrality)
betweenness_centrality_df$name <- sapply(betweenness_centrality_df$name, add_numbering)


# clusters_leiden <- cluster_leiden(graph, objective_function = "modularity") # 29 groups
# 
# clusters_louvain <- cluster_louvain(graph) # 29 groups
# community_louvain <- data.frame(Name = character(), Group = integer())
# for (i in seq_along(communities(clusters_louvain))) {
#   temp_df <- data.frame(Name = communities(clusters_louvain)[[i]], Group = i)
#   
#   community_louvain <- rbind(community_louvain, temp_df)
# }
# community_louvain$Name <- sapply(community_louvain$Name, add_numbering)
# summary_louvain <- community_louvain %>%
#   group_by(Group) %>%
#   dplyr::summarise(n())

# Network with different group coloring
# pdf("C:/Users/Ngoc Phu Nguyen/Documents/GitHub/Bachelorarbeit/Deskriptive Analyse/Coauthor-Network/network-louvain.pdf", width=20, height=20)
# colors <- rainbow(max(membership(clusters_louvain)))
# set.seed(networkseed)
# coords <- layout_with_fr(graph = graph, niter = 2000)
# plot(graph,
#      vertex.size = 5,
#      vertex.color = colors[membership(clusters_louvain)],
#      vertex.label = rownames(vertices),
#      vertex.frame.color = "gray90",
#      vertex.label.color = "black",
#      edge.color = "SkyBlue2",
#      layout = coords,
#      main = sprintf("Complete collaboration network (%s)",
#                     max(levels(coauthors_pairs$year))))
# legend("topleft",
#        legend = sort(unique(edgelist$width)),
#        lwd = sort(unique(edgelist$width)),
#        col = "SkyBlue2",
#        bty = "n")
# par(op1)
# dev.off()

# pdf("C:/Users/Ngoc Phu Nguyen/Documents/GitHub/Bachelorarbeit/Deskriptive Analyse/Coauthor-Network/network-leiden.pdf", width=20, height=20)
# colors <- rainbow(max(membership(clusters_leiden)))
# set.seed(networkseed)
# coords <- layout_with_fr(graph = graph, niter = 2000)
# plot(graph,
#      vertex.size = 5,
#      vertex.color = colors[membership(clusters_leiden)],
#      vertex.label = rownames(vertices),
#      vertex.frame.color = "gray90",
#      vertex.label.color = "black",
#      edge.color = "SkyBlue2",
#      layout = coords,
#      main = sprintf("Complete collaboration network (%s)",
#                     max(levels(coauthors_pairs$year))))
# legend("topleft",
#        legend = sort(unique(edgelist$width)),
#        lwd = sort(unique(edgelist$width)),
#        col = "SkyBlue2",
#        bty = "n")
# par(op1)
# dev.off()

#### Eigen centrality
eigen_centrality <- eigen_centrality(graph)$vector

eigen_centrality_df <- data.frame(centrality = eigen_centrality)
eigen_centrality_df <- eigen_centrality_df %>%
  mutate(name = row.names(eigen_centrality_df)) %>%
  select(name, centrality)
eigen_centrality_df$name <- sapply(eigen_centrality_df$name, add_numbering)


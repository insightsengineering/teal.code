
# teal.code R Package Development Guide

## Package Overview

`teal.code` is an R package for storing code and its execution
environment in objects called `qenv`. It supports reproducible code
execution in teal Shiny applications. Developers can inspect and modify
`qenv` objects, providing full control over the stored code and
environment. These objects also capture warnings and errors
automatically.

## Development Context

The teal framework uses Shiny to create reproducible environments for
data analysis. Within this framework, `teal.code` manages code and its
execution environment. Related packages include:

- `teal.reporter`: Integrates reports with reproducible code in teal
  applications. Its reporter functions accept `qenv` objects as inputs.
- `teal`: Provides the core architecture for teal Shiny applications.
  Teal modules should use `qenv` objects to ensure that code changes are
  recorded and reproducible.
- `teal.data`: Stores all data objects, data relations and
  transformations of data used in teal applications. The `teal.data`
  object is based on the `qenv` and therefore used for code
  reproducibility.

For an introduction to `qenv` and its related functions and methods, see
@vignettes/qenv.Rmd.

### Workflows

- When creating a `qenv`, favor readability over concise code.
- For simple `qenv` objects, favor `within()`, as it supports more
  readable code.
- When creating a `qenv` that uses functions with side effects, such as
  `runif()`, `library` or `options` always use `eval_code()`. Use the
  appropriate comment to link side effects to objects
- For a complex `qenv`, consider dividing the work into smaller `qenv`
  objects that encapsulate distinct logical steps.
- In the line of the previous comment, `qenv` can be combined with
  method `c.qenv`. Be careful that they cannot have elements with the
  same name and about the combined order of code execution.
- For a long or complex `qenv`, choose between `eval_code()` and
  `within()` based on:
  - the number of external variables added to the `qenv`; a larger
    number favors `eval_code()`;
  - how often the same external variable is used; greater repetition
    favors `within()`.
- When creating custom functions within a teal app server, it is better
  to do define them inside a qenv. If defined outside the function might
  fail.
- When fixing bugs or adding features, avoid changing the core
  principles of `qenv`. Check whether an issue already exists. If the
  expected behavior is unclear, request that an issue be created before
  making changes that could have unexpected consequences.

### Debugging `qenv`

When debugging teal apps, it is very common that the error happens
within the execution of a `qenv`. It is important to know how to debug
`qenv`. Firstly, by default when there is an error within a qenv its
automatically handled, and a `qenv.error` class object will be returned.
That will prevent the teal app to immediately crash and will display an
useful error message to the user. Once identified a problematic `qenv`,
the guidelines to debug it are: - As the `qenv` does not print messages
and warnings to the R console, so explore them with functions
`get_messages` and `get_warnings`. - Access individual elements of the
`qenv` with `get_var[[qenv, "my_var"]]` to inspect internal values. -
Use function `get_code` to extract specifically how an element is
created. That will help in identifying errors.

This package is part of the teal framework. The following configuration
applies to all packages within the teal framework.

## Package Structure and Organization

### Key Directories

Follow the standard R package structure with teal-specific conventions:

``` text
package_name/
├── .gitlab-ci.yml    # CI/CD workflows (if package hosted in Gitlab)
├── .github           # CI/CD workflows (if package hosted in GitHub)
├── R/                # R source code
├── tests/testthat/   # Unit tests using testthat
├── vignettes/        # Long-form documentation
├── inst/             # Package assets
├── AGENTS.md         # Development guide for AI agents (this file)
├── DESCRIPTION       # Package metadata
├── NAMESPACE         # Exports and imports automa
├── NEWS.md           # Change log
├── README.md         # Package overview
├── _pkgdown.yml      # Documentation website config
├── .lintr            # Linting configuration
└── .Rbuildignore     # Build exclusions
```

### Naming Conventions

- **Function names**: Use `snake_case` consistently
- **Class names**: Use `PascalCase` (e.g., `TealAppDriver`)
- **Module functions**: Prefix UI functions with `ui_` and server
  functions with `srv_`
- **Internal functions**: Use descriptive names without export

### File Organization

- **One main function per file** when the function is substantial
- **Group related utilities** in shared files (e.g., `utils.R`,
  `validations.R`)
- **Module files**: Use pattern `tm_<name>.R` for teal modules
- **Helper functions**: Prefix with the main function they support

## Code Style and Standards

### Code Quality

- **Run `pre-commit` hooks**: Always run `pre-commit run --all-files`
  before committing, Fix any issues it reports - the error messages are
  informative and will guide you. It automatically checks code style,
  documentation and other quality issues. If pre-commit is not
  available, run the checks manually. Lint the R code manually as well
  if not called by pre-commit.
- **Follow `tidyverse` style**: General R code style follows the
  `tidyverse` style guide.
- **Documentation**: All exported functions must have `roxygen2`
  documentation with `@returns` and `@examples` fields.
- **Formatting** rules are configured in the `.lintr` file.

## Dependencies and Imports

### Dependency Management

- **Minimize dependencies**: Only add dependencies that provide
  significant value
- **Version constraints**: Specify minimum versions for critical
  dependencies
- **Ecosystem coherence**: Prefer packages already used within teal
  ecosystem

### Import Best Practices

Avoid importing package functions via roxygen2 (`#' @import pkg`)tags in
favor of explicit namespacing for clarity when appropriate. When needed
prefer specific imports over full package imports.

### Code Style for Modules

- **Use `tidyverse` style**: Write clear, readable code using `dplyr`,
  `ggplot2` patterns
- **Use `magrittr` pipes in reproducible execution**: For code executed
  for `teal_data`/`qenv` data objects with `eval_code()` and `within()`
- **Use crane and gtsummary**: For statistical tables and summaries
- **Error handling**: Implement proper validation using `checkmate` and
  `shiny::validate(teal::need_input(...))`

## Testing Framework

### Testing Philosophy

- **Test public functions only**: Internal utilities should be tested
  through public interfaces
- **Precise, focused tests**: Each test should verify one specific
  behavior
- **High coverage**: Maintain at least 80% test coverage as measured by
  `covr`
- **Integration over units**: Test realistic usage patterns
- **Test Dependencies**.: Add
  `testthat::skip_if_not_installed(package_name)` only for dependencies
  in `Suggests` or related to tests cases

### Shiny Module Testing

- **Server functions**: Test with `shiny::testServer()`
- **UI functions**: Test basic usage with regular testing (class checks,
  error generation, snapshots, regexp search). Test UI scenarios and
  interactions with `teal:::TealAppDriver` (based on
  `shinytest2::AppDriver`) for integration testing
- **Reactive behavior**: Test reactive chains and side effects

### Test Organization and Naming

- **One test file per R file**: `test-module_example.R` for
  `module_example.R`
- **Descriptive test names**: Clearly describe what is being tested
- **End to end test names**: `test-shinytest2-module_example.R` for
  `module_example.R`
- **Logical grouping**: Group related tests using `describe()` and
  individual tests with `it()` when beneficial
- **Test data**: Create minimal test datasets, avoid external
  dependencies

## Documentation and Communication

### Package Documentation

- **`README.md`**: Clear overview, installation, basic usage examples
- **Vignettes**: Comprehensive guides for complex functionality
- **Function documentation**: All exported functions must have
  `roxygen2` documentation
- **`NEWS.md`**: Detailed changelog of features, bugs and miscellanea
  changes affecting the users

### Package Version Management

Do not change versions on your own. There is a CI/CD workflow that
manages the versions automatically on the `main` branch.

## CI/CD and Development Workflow

### Gitlab Workflows (if package hosted in Gitlab)

`.gitlab-ci.yml` reuses CI/CD tasks, such as running all unit tests,
`R CMD check`, code quality checks, style checks and website generation.

### GitHub Workflows (if package hosted in GitHub)

Use r.pkg.template workflows for consistency:

- `check.yaml`: R CMD check, unit tests, coverage
- `docs.yaml`: Documentation building and deployment
- `audit.yaml`: Security and dependency auditing
- `pkgdown.yaml`: Website generation

## Quality Assurance

### Code Quality Metrics

- **Test Coverage**: ≥80% line coverage
- **Linting**: No lint violations using configured `.lintr`
- **Documentation**: 100% of exports documented

### Code Review Process

- **Pull Request Reviews**: All changes require human review and
  approval
- **Automated Checks**: CI must pass before merging
- **Breaking Changes**: Require special consideration and communication
- **Documentation Updates**: Must accompany functional changes

### Performance Considerations

- **Shiny Reactivity**: Minimize unnecessary reactive computations
- **Data Processing**: Use efficient data manipulation patterns
- **Memory Usage**: Consider memory implications for large datasets
- **Loading Time**: Optimize package loading and module initialization

## Maintenance Guidelines

- **Long-term Support**: Maintain backward compatibility when possible
- **Dependencies**: Minimal and justified dependencies only
- **Deprecation**: Use `lifecycle` package for function deprecation

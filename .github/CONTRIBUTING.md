# Contributing to shinypayload

We love your input! We want to make contributing to `shinypayload` as easy and transparent as possible, whether it's:

- Reporting a bug
- Discussing the current state of the code
- Submitting a fix
- Proposing new features
- Becoming a maintainer

## 🚀 Quick Start

1. **Fork** the repository
2. **Clone** your fork: `git clone https://github.com/yourusername/shinypayload.git`
3. **Install** development dependencies: `devtools::load_all()`
4. **Create** a feature branch: `git checkout -b feature/amazing-feature`
5. **Make** your changes
6. **Test** your changes: `devtools::test()`
7. **Check** the package: `devtools::check()`
8. **Commit** your changes: `git commit -m 'Add amazing feature'`
9. **Push** to your branch: `git push origin feature/amazing-feature`
10. **Open** a Pull Request

## 🐛 Bug Reports

We use GitHub issues to track public bugs. Report a bug by [opening a new issue](https://github.com/PawanRamaMali/shinypayload/issues/new).

**Great Bug Reports** tend to have:

- A quick summary and/or background
- Steps to reproduce
  - Be specific!
  - Give sample code if you can
- What you expected would happen
- What actually happens
- Notes (possibly including why you think this might be happening, or stuff you tried that didn't work)

## 💡 Feature Requests

We welcome feature requests! Please:

1. **Check existing issues** to avoid duplicates
2. **Describe the problem** you're trying to solve
3. **Describe the solution** you'd like to see
4. **Consider alternatives** you've thought about
5. **Provide examples** of how it would be used

## 🔧 Development Setup

```r
# Install development dependencies
install.packages(c("devtools", "testthat", "roxygen2", "styler", "lintr"))

# Load the package
devtools::load_all()

# Run tests
devtools::test()

# Check package
devtools::check()

# Generate documentation
devtools::document()
```

## 📝 Code Style

We use automated code styling. Before submitting:

```r
# Style your code
styler::style_pkg()

# Check for linting issues (optional)
lintr::lint_package()
```

### Key Principles

- **Be consistent** with existing code
- **Use meaningful names** for functions and variables
- **Write clear documentation** with roxygen2
- **Include tests** for new functionality
- **Follow tidyverse style** where applicable

## 🧪 Testing

We maintain high test coverage. When adding features:

1. **Write tests first** (TDD approach preferred)
2. **Test edge cases** and error conditions
3. **Use descriptive test names**
4. **Group related tests** in contexts

```r
# Run all tests
devtools::test()

# Run specific test file
devtools::test(filter = "payload")

# Test with coverage
covr::package_coverage()
```

## 📚 Documentation

### Function Documentation

Use roxygen2 for all exported functions:

```r
#' Brief description
#'
#' Longer description if needed
#'
#' @param param_name Description of parameter
#' @return Description of return value
#' @export
#' @examples
#' \dontrun{
#' example_usage()
#' }
my_function <- function(param_name) {
  # implementation
}
```

### Examples

- Include **realistic examples**
- Use `\dontrun{}` for examples requiring external services
- Ensure examples **actually work**
- Show **common use cases**

## 🚦 Pull Request Process

1. **Update documentation** for any new/changed functionality
2. **Add tests** that prove your fix/feature works
3. **Ensure CI passes** - all tests, checks, and style requirements
4. **Update NEWS.md** if your change is user-facing
5. **Be patient** - maintainers will review when possible

### PR Checklist

- [ ] Code follows the existing style
- [ ] Tests added for new functionality
- [ ] All tests pass locally
- [ ] Documentation updated
- [ ] NEWS.md updated (if applicable)
- [ ] CI checks pass

## 🏷️ Versioning

We use [Semantic Versioning](http://semver.org/). For the versions available, see the [tags on this repository](https://github.com/PawanRamaMali/shinypayload/tags).

## 📄 License

By contributing, you agree that your contributions will be licensed under the MIT License.

## 🤝 Code of Conduct

This project follows the [Contributor Covenant Code of Conduct](https://www.contributor-covenant.org/version/2/1/code_of_conduct/). By participating, you are expected to uphold this code.

### Our Standards

- **Be welcoming** to newcomers
- **Be respectful** of differing viewpoints
- **Accept constructive criticism** gracefully
- **Focus on community benefit**
- **Show empathy** towards community members

## 📞 Questions?

Don't hesitate to ask! You can:

- Open an issue for **bug reports** or **feature requests**
- Start a [discussion](https://github.com/PawanRamaMali/shinypayload/discussions) for **questions**
- Email the maintainer: **prm@outlook.in**

## 🙏 Recognition

Contributors will be recognized in:

- **README.md** acknowledgments
- **NEWS.md** release notes  
- **DESCRIPTION** file (for significant contributions)

Thank you for making `shinypayload` better! 🎉
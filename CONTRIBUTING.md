Contributing
-----

### Requirements

```r
install.packages(c('httptest', 'testthat', 'mockthat', 'getPass', 'rstudioapi', 'mockery', 'lintr', 'styler', 'devtools', 'roxygen2'))
```

### Development

This project follows the [tidyverse style
guide](https://style.tidyverse.org/documentation.html) and uses
[roxygen2](https://github.com/r-lib/roxygen2) for generating documentation.

External functions need an `@export` tag in the Roxygen2 docstring. They must
also be added to `_pkgdown.yml`.

After adding a new function, you must:
```r
library('roxygen2')
roxygenise()
```

#### Markdown

Roxygen has [markdown
support](https://roxygen2.r-lib.org/articles/markdown.html). To enable it,
include the `@md` tag in the docstring, i.e.:
```r
#' @param properties list Properties 
#'
#' Accepted properties:
#' - item type Description of item
#' - itemList list List with properties:
#'   - property character
#'   - property character
#' @md
```

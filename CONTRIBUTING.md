## General guidelines

This will provide you with contribution guidelines. You can contribute in two ways:

1. Raising a bug report or making a feature request in an issue
2. Suggesting a change by submitting a pull request. Specific suggestions are described below.

### Issues

* Use the template provided when you create an issue. Follow this template as closely as possible (including the output from `devtools::session_info()`)
* It is ok to submit a stump issue (title only) to ensure something is recorded as long as you subsequently complete it. Issues will not be actioned if they are not complete.


### Pull requests

* You must submit code that follows a [common code style](http://adv-r.had.co.nz/Style.html). PRs that do not follow this style will not be accepted. You may use existing code as inspiration if that is the way you learn.
* Files containing classes should follow the convention `<name>-class.R` where `name` is the class name. Use hyphens to separate words in a file name.
* Use the template that is provided to you when you create a PR.
* New code should be accompanied by [test cases](http://r-pkgs.had.co.nz/tests.html). If it does not, then you must explain why in the PR and create an issue (assigned to yourself) to subsequently submit the test casess.
* New code should be [documented](http://r-pkgs.had.co.nz/man.html) when the object to exported (available to the user). Make sure that you roxygenise the package before submitting the PR if you have added or updated any roxygen blocks by invoking `devtools::document()` and that you use the same version of 
roxygen that is documented in the `DESCRIPTION` file.
* Add an item to the [`NEWS` file](http://r-pkgs.had.co.nz/release.html#important-files) that reflects the FIX, CHANGE or NEW functionality. Include any issue numbers that are relevant (e.g. `(#XX)`) where `XX` is the issue number and your Github handle if you are submitting changes (e.g. `@yourname`).
* Add your name to the [authors list](http://r-pkgs.had.co.nz/description.html#author) in the `DESCRIPTION` file if you aren't already represented therein
* Only the package maintainer should merge PRs into the master branch. The template PR request includes a ping to the package maintainer.
* Run `devtools::check()` before submitting the PR.


### Workflow

* Adopt the [Github Flow](https://guides.github.com/introduction/flow/) branch based workflow. 
* Branches must almost always be created from master and you will be responsible for resolving any future conflicts with master prior to your PR being accepted. 
* Be nice to other contributors.


## Specific guidelines

Here are a few suggestions for how you can contribute to this package.

### Calendars

A number calendars are pre-canned as part of this package. However, we could always do with more. To do this, you will need to:

1. Implement a constructor function such as `<LocationCode>Calendar() <- Calendar("<LocationCode>", "<Olson>/<TimeZone>")`. This function will create an instance of the corresponding `Calendar` subclass. See `?Calendar` for further information.
2. Implement a method for `is_good()` that dispatches on the Calendar subclass that you have implemented.

### Day basis conventions

You can implement new day basis conventions and their corresponding year fraction calculators by:

1. Defining a string to denote the corresponding day basis convention (e.g. "act/365")
2. Add this string to the list of valid day basis conventions in `is_valid_day_basis()`
3. Implement a vectorised function that calculates the years between two dates (e.g. `thirty_360()`)
4. Call this function from `year_frac()` making sure that you preserve its vectorised nature.

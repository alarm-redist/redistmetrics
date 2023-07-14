# redistmetrics 1.0.4
* Fix rare edge case in `prep_perims()` where a shape that is Queen's-contiguous to one other shape and contiguous to nothing else causes a loud failure when creating the perimeter output.

# redistmetrics 1.0.3
* Minor improvements to compactness functions by removing costly unions where collections are sufficient.
* Adds support for multi-threaded VI calculations.
* Uses new logo to match the updated `redist` logo.
* Implements an _experimental_ basic *fuzzy* splits function for non-nesting geographies. 

# redistmetrics 1.0.2
* Makes `ncores` argument work for `prep_polsby()`.
* Speed improvements for compactness functions in serial.
* C++ header-only interface for some functions
* Reverses `part_tau_gap()` sign so positive is pro-Republican bias.
* Improves usability of `by_plan()` by allowing for measurements to be repeated. Instead of reducing the vector length only if every entry is repeated the same number of times, it does it by the greatest common divisor.
* Corrects documentation indicating that some inputs need to have `sf` geometry when it is not used.
* Relicense under MIT License
* Adds `admin_splits_total()` to count total unique district-admins.

# redistmetrics 1.0.1

* Reverses `part_bias()` sign so positive is pro-Republican bias.
* Reverses `part_egap()` and `part_egap_ep()` sign so positive is pro-Republican bias.
* Fixes bug where `splits_admin()` plans input needed to be sequentially numbered (#10).
* Allows planarizing warnings to be silenced when `epsg = FALSE` (#11).
* Makes `ncores` argument work for compactness functions.

# redistmetrics 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.

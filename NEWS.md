# redistmetrics 1.0.2
* Makes `ncores` argument work for `prep_polsby()`.
* Speed improvements for compactness functions in serial.
* Rcpp functions no longer export RNG when results are deterministic.
* Reverses `part_tau_gap()` sign so positive is pro-Republican bias.
* Improves usability of `by_plan()` by allowing for measurements to be repeated. Instead of reducing the vector length only if every entry is repeated the same number of times, it does it by the greatest common divisor.
* Corrects documentation indicating that some inputs need to have `sf` geometry when it is not used.

# redistmetrics 1.0.1

* Reverses `part_bias()` sign so positive is pro-Republican bias.
* Reverses `part_egap()` and `part_egap_ep()` sign so positive is pro-Republican bias.
* Fixes bug where `splits_admin()` plans input needed to be sequentially numbered (#10).
* Allows planarizing warnings to be silenced when `epsg = FALSE` (#11).
* Makes `ncores` argument work for compactness functions.

# redistmetrics 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.

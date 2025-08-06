# redistmetrics 1.0.10

* Adds support for Partisan Dislocation (Deford, Eubank, and Rodden 2022) via `part_dislocation()`.

# redistmetrics 1.0.9

* Revises a minor bug in `seg_dissim()` where having a district with 0 population would yield NAs
  for the dissimilarity index (#20)
* Adds support for Dilution Asymmetry (Gordon and Yntiso 2024) via `part_dil_asym()`.

# redistmetrics 1.0.8

* Adds support for counting splits municipalities (sub-admin) units via `splits_sub_count()`.
* `splits_count()` and `splits_sub_count()` now include rownames in the output.

# redistmetrics 1.0.7

* Updates compilation flags for use of `RcppThread` on Linux.

# redistmetrics 1.0.6

* Resolves minor CRAN submission issues for use of `revdep` to check reverse dependencies.

# redistmetrics 1.0.5

* Updates to C++17.
* Resolves minor C++ warnings (#14).

# redistmetrics 1.0.4
* Add generic function to tally a varible by plan
* Massively speed up calculation of county/admin/muni splits as well as plan reindexing
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

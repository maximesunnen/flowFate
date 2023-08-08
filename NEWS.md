# flowFate (development version)

# flowFate 0.1.3
- added channel parameter to the `plot_myosin_splittedPeaks( )` function so that the user's (!) channel name is taken as the x argument of aes(). This unfortunately still defaulted to our own machine's channel name...
- the `getData_splitPeaks()` function `params` argument now also takes into account the user's (!) MyHC channel name
- implemented the addition of csv comments. "Metadata" (channel name, flowFate version, bin sizes) is now included in the csv file.

# flowFate 0.1.2
- replaced the demo data
- `golem` added to DESCRIPTION imports

# flowFate 0.1.1

- opted for a less big demo file (< 5MB)
- changed the read-in of this demo file

# flowFate 0.1.0

- added the option to submit single, merged FCS files as well as a folder with individual FCS files
- Added a `pkgdown` website
- added a favicon using [hexmake](https://connect.thinkr.fr/hexmake/)

# flowFate 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.

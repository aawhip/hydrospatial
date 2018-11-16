# hydrospatial
An R-package for spatial and temporal analysis of floodplain inundation characteristics and habitat suitability from raster data

## Plan for package structure
### Organization
Each function will be a separate file

### Conventions
On function names/filenames, have generic classes (lowercase) in front (e.g., "read", "calc","plot"), then caps for what it's doing - see USGS packages for this convention

### Questions
How best to group functions if each is a separate file, use a common class name within the filename? The three main categories I think would be "predictrasters", "hsa" (hydrospatialanalysis), and plot/summary/visualization.

Need to figure out how to do the scenario comparison - it certainly needs to be a wrap around the basic functions so that people don't have to have a comparison setup

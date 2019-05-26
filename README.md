# hydrospatial
An R-package for spatial and temporal analysis of floodplain inundation characteristics and habitat suitability from raster data

## Plan for package structure
### Organization
Don't put all functions in one file or each funciton in its own separate file

### Conventions
On function names/filenames, have generic classes (lowercase) in front (e.g., "read", "calc","plot"), then underscore, then shortname (lowercase w caps to distinguish between words), see USGS packages for this convention

### Questions
How best to group functions if each is a separate file, use a common class name within the filename? The four main categories I think would be "predictrast", "hsa" (hydrospatialanalysis), "utility", and "vis" (plot/summary/visualization).

Need to figure out how to do the scenario comparison - it certainly needs to be a wrap around the basic functions so that people don't have to have a comparison setup. Have an if statement check?

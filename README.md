dpkg: Data Packages for R
================

An R package to read, write, and edit [Data Package](https://specs.frictionlessdata.io/data-package/) data and metadata. Unlike other existing R packages [dpmr](https://github.com/christophergandrud/dpmr) and [datapkg](https://github.com/ropenscilabs/datapkg), dpkg can be used to build and document Data Packages entirely within R. Please note that this is a work in progress and function naming and functionality may drift based on feedback from the community.

This package is not on CRAN. To install in R, use [devtools](https://github.com/hadley/devtools):

    devtools::install_github("ezwelty/dpkg")

#### Quick introduction

To build a data package, assemble the data and add metadata to the various elements:

``` r
data <- data.frame(
  id = 1L %>% set_field(title = "Identifier"),
  value = 1.1,
  added = Sys.Date()
)
# Data Resource (list of Fields)
dr <- data %>%
  set_resource(
    name = "data",
    path = "data/data.csv"
  )
# Data Package (list of Resources)
dp <- list(dr) %>%
  set_package(
    name = "data-package"
  )
```

You can preview the package metadata:

``` r
get_package(dp) %>% str()
```

    ## List of 2
    ##  $ name     : chr "data-package"
    ##  $ resources:List of 1
    ##   ..$ :List of 3
    ##   .. ..$ name  : chr "data"
    ##   .. ..$ path  : chr "data/data.csv"
    ##   .. ..$ schema:List of 1
    ##   .. .. ..$ fields:List of 3
    ##   .. .. .. ..$ :List of 3
    ##   .. .. .. .. ..$ name : chr "id"
    ##   .. .. .. .. ..$ type : chr "integer"
    ##   .. .. .. .. ..$ title: chr "Identifier"
    ##   .. .. .. ..$ :List of 2
    ##   .. .. .. .. ..$ name: chr "value"
    ##   .. .. .. .. ..$ type: chr "number"
    ##   .. .. .. ..$ :List of 3
    ##   .. .. .. .. ..$ name  : chr "added"
    ##   .. .. .. .. ..$ type  : chr "date"
    ##   .. .. .. .. ..$ format: chr "%Y-%m-%d"

Write the package to file:

``` r
dir <- tempdir()
write_package(dp, path = dir)
```

And read the package back in:

``` r
read_package(dir)
```

    ## $data
    ##   id value      added
    ## 1  1   1.1 2017-07-12
    ## 
    ## attr(,"dpkg_package")
    ## attr(,"dpkg_package")$name
    ## [1] "data-package"

Build a package
---------------

In `dpkg`, the contents of a data *package* is stored as a list of one or more data *resources* (each a list) of one or more *fields* (each typically an atomic vector). For example:

``` r
dp <- list(
  dr = data.frame(
    id = 1L,
    value = 1.1,
    added = Sys.Date()
  )
)
```

Package, resource, and field ("data objects") metadata can be set or updated using the `set_*` functions (`set_package`, `set_resource`, `set_field`), which come in a `<-` flavor:

``` r
set_field(dp$dr$id) <- field(title = "Unique identifier", constraints = constraints(unique = TRUE))
```

and a pipe-friendly flavor:

``` r
dp$dr$id %<>% set_field(title = "Identifier", constraints = NULL)
```

As seen above with the use of `field` and `constraints`, a suite of helper functions are available to assist in the building of metadata:

-   Data objects: `package`, `resource`, `field`
-   Meta objects: `schema`, `foreignKey`, `constraints`, `license`, `source`, `contributor`

Preview a package
-----------------

To preview a package, metadata can be retrieved from data objects using the `get_*` functions (`get_package`, `get_resource`, `get_field`). Missing properties are filled with their default values:

-   Fields
    -   `name`: The name of the object in a list (resource).
    -   `type`: The type corresponding to the object class (or "string" if not supported).
    -   `format`: The default format for that type.
    -   `unit`: Units set by [units](https://github.com/edzer/units) deparsed to product power form.
-   Resources
    -   `name`: The name of the object in a list (package).
    -   `schema$fields`: Field metadata from the elements of the object.
-   Packages
    -   `resources`: Resource metadata from the elements of the object.

``` r
get_field(dp$dr$id) %>% str()
```

    ## List of 2
    ##  $ type : chr "integer"
    ##  $ title: chr "Identifier"

``` r
get_resource(dp$dr) %>% str()
```

    ## List of 2
    ##  $ schema:List of 1
    ##   ..$ fields:List of 3
    ##   .. ..$ :List of 3
    ##   .. .. ..$ name : chr "id"
    ##   .. .. ..$ type : chr "integer"
    ##   .. .. ..$ title: chr "Identifier"
    ##   .. ..$ :List of 2
    ##   .. .. ..$ name: chr "value"
    ##   .. .. ..$ type: chr "number"
    ##   .. ..$ :List of 3
    ##   .. .. ..$ name  : chr "added"
    ##   .. .. ..$ type  : chr "date"
    ##   .. .. ..$ format: chr "%Y-%m-%d"
    ##  $ data  :'data.frame':  1 obs. of  3 variables:
    ##   ..$ id   : int 1
    ##   ..$ value: num 1.1
    ##   ..$ added: chr "2017-07-12"

``` r
get_package(dp) %>% str()
```

    ## List of 1
    ##  $ resources:List of 1
    ##   ..$ :List of 3
    ##   .. ..$ name  : chr "dr"
    ##   .. ..$ schema:List of 1
    ##   .. .. ..$ fields:List of 3
    ##   .. .. .. ..$ :List of 3
    ##   .. .. .. .. ..$ name : chr "id"
    ##   .. .. .. .. ..$ type : chr "integer"
    ##   .. .. .. .. ..$ title: chr "Identifier"
    ##   .. .. .. ..$ :List of 2
    ##   .. .. .. .. ..$ name: chr "value"
    ##   .. .. .. .. ..$ type: chr "number"
    ##   .. .. .. ..$ :List of 3
    ##   .. .. .. .. ..$ name  : chr "added"
    ##   .. .. .. .. ..$ type  : chr "date"
    ##   .. .. .. .. ..$ format: chr "%Y-%m-%d"
    ##   .. ..$ data  :'data.frame':    1 obs. of  3 variables:
    ##   .. .. ..$ id   : int 1
    ##   .. .. ..$ value: num 1.1
    ##   .. .. ..$ added: chr "2017-07-12"

Write a package
---------------

`write_package` writes package data and metadata to disk using the following rules for each resource:

-   `format`: If missing, checks `path` file extension and `mediatype`. Only "csv" ("text/csv") and "json" ("application/json") are supported.
-   `path`: If not set, the data is saved in the metadata (`datapackage.json`) as either an inline JSON object (`format:` "json" or missing) or a CSV string (`format:` "csv"). For writing, `path` must be a single, local, relative path.

Resource as an inline JSON object:

``` r
set_resource(dp$dr) <- package(format = "json", path = NULL)
get_resource(dp$dr)$data
```

    ##   id value      added
    ## 1  1   1.1 2017-07-12

``` r
write_package(dp, path = tmpdir)
list.files(tmpdir)
```

    ## [1] "datapackage.json"

Resource as an inline CSV string:

``` r
set_resource(dp$dr) <- package(format = "csv", path = NULL)
get_resource(dp$dr)$data
```

    ## [1] "id,value,added\n1,1.1,2017-07-12"

``` r
write_package(dp, path = tmpdir)
list.files(tmpdir)
```

    ## [1] "datapackage.json"

Resource as a JSON file:

``` r
set_resource(dp$dr) <- package(format = "json", path = "data/data.json")
get_resource(dp$dr)$data
```

    ## NULL

``` r
write_package(dp, path = tmpdir)
list.files(tmpdir, recursive = TRUE)
```

    ## [1] "data/data.json"   "datapackage.json"

Resource as a CSV file:

``` r
set_resource(dp$dr) <- package(format = "csv", path = "data/data.csv")
get_resource(dp$dr)$data
```

    ## NULL

``` r
write_package(dp, path = tmpdir)
list.files(tmpdir, recursive = TRUE)
```

    ## [1] "data/data.csv"    "datapackage.json"

Read a package
--------------

`read_package` reads package data and metadata into the same structure described above, but unlike `write_package`, it supports both local and remote paths. The `resources` argument can be used to read only a subset of the package resources.

``` r
dp <- read_package(
  "https://raw.githubusercontent.com/columbia-glacier/optical-surveys-1985/master",
  resources = c("station", "velocity")
)
get_package(dp) %>% str()
```

    ## List of 7
    ##  $ name        : chr "optical-surveys-1985"
    ##  $ title       : chr "Optical Surveys (1985)"
    ##  $ description : chr "Velocity of three reflectors 1.3, 2.8, and 4.6 km from the terminus and meteorological observations from a station on nearby He"| __truncated__
    ##  $ version     : chr "0.1.0"
    ##  $ sources     :List of 1
    ##   ..$ :List of 2
    ##   .. ..$ title: chr "Original data, scripts, and documentation"
    ##   .. ..$ path : chr "sources/"
    ##  $ contributors:List of 1
    ##   ..$ :List of 3
    ##   .. ..$ title: chr "Ethan Welty"
    ##   .. ..$ email: chr "ethan.welty@gmail.com"
    ##   .. ..$ role : chr "author"
    ##  $ resources   :List of 2
    ##   ..$ :List of 4
    ##   .. ..$ name  : chr "station"
    ##   .. ..$ path  : chr "data/station.csv"
    ##   .. ..$ title : chr "Station Metadata"
    ##   .. ..$ schema:List of 1
    ##   .. .. ..$ fields:List of 2
    ##   .. .. .. ..$ :List of 4
    ##   .. .. .. .. ..$ name       : chr "lat"
    ##   .. .. .. .. ..$ type       : chr "number"
    ##   .. .. .. .. ..$ description: chr "Latitude (WGS84, EPSG:4326)."
    ##   .. .. .. .. ..$ unit       : chr "°"
    ##   .. .. .. ..$ :List of 4
    ##   .. .. .. .. ..$ name       : chr "lng"
    ##   .. .. .. .. ..$ type       : chr "number"
    ##   .. .. .. .. ..$ description: chr "Longitude (WGS84, EPSG:4326)."
    ##   .. .. .. .. ..$ unit       : chr "°"
    ##   ..$ :List of 4
    ##   .. ..$ name  : chr "velocity"
    ##   .. ..$ path  : chr "data/velocity.csv"
    ##   .. ..$ title : chr "Marker Velocity"
    ##   .. ..$ schema:List of 1
    ##   .. .. ..$ fields:List of 4
    ##   .. .. .. ..$ :List of 3
    ##   .. .. .. .. ..$ name       : chr "marker"
    ##   .. .. .. .. ..$ type       : chr "integer"
    ##   .. .. .. .. ..$ description: chr "Marker identifier (1: 1.3 km, 2: 2.8km, and 3: 4.6 km from the terminus)."
    ##   .. .. .. ..$ :List of 3
    ##   .. .. .. .. ..$ name       : chr "sequence"
    ##   .. .. .. .. ..$ type       : chr "integer"
    ##   .. .. .. .. ..$ description: chr "Sequence number from figure tracing. Observations are 'continuous' between times of the same sequence."
    ##   .. .. .. ..$ :List of 4
    ##   .. .. .. .. ..$ name       : chr "t"
    ##   .. .. .. .. ..$ type       : chr "datetime"
    ##   .. .. .. .. ..$ format     : chr "%Y-%m-%dT%H:%M:%SZ"
    ##   .. .. .. .. ..$ description: chr "Date and time (UTC)."
    ##   .. .. .. ..$ :List of 4
    ##   .. .. .. .. ..$ name       : chr "value"
    ##   .. .. .. .. ..$ type       : chr "number"
    ##   .. .. .. .. ..$ description: chr "Velocity"
    ##   .. .. .. .. ..$ unit       : chr "m d-1"

``` r
dp$station
```

    ##          lat         lng
    ## 1 60.98891 ° -147.0357 °

``` r
head(dp$velocity)
```

    ##   marker sequence                   t        value
    ## 1      1        2 1985-08-06 14:43:20 9.289464 m/d
    ## 2      1        1 1985-08-06 14:45:22 9.218484 m/d
    ## 3      1        4 1985-08-06 14:51:20 9.511275 m/d
    ## 4      1        3 1985-08-06 15:07:43 9.440296 m/d
    ## 5      1        6 1985-08-06 15:27:08 9.635490 m/d
    ## 6      1        5 1985-08-06 15:28:58 9.571165 m/d

`read_package_github` accepts a shorthand GitHub repository address.

``` r
dp <- read_package_github("columbia-glacier/optical-surveys-1985", "station")
```

TODO
----

### Fields

Only types `string`, `number`, `integer`, `boolean`, `date`, and `datetime` are implemented (see [table-schema/field-descriptors](https://specs.frictionlessdata.io/table-schema/#field-descriptors)). Add support for the remaining types:

-   \[ \] \[object\](<https://specs.frictionlessdata.io/table-schema/#object>)
-   \[ \] \[array\](<https://specs.frictionlessdata.io/table-schema/#array>)
-   \[ \] \[time\](<https://specs.frictionlessdata.io/table-schema/#time>) (via package [hms](https://github.com/tidyverse/hms))
-   \[ \] \[year\](<https://specs.frictionlessdata.io/table-schema/#year>) (already supported via `type = date` and `format = "%Y"`)
-   \[ \] \[yearmonth\](<https://specs.frictionlessdata.io/table-schema/#yearmonth>) (already supported via `type = date` and `format = "%Y-%m"`)
-   \[ \] \[duration\](<https://specs.frictionlessdata.io/table-schema/#duration>) (already supported via `type = numeric` and `unit`)
-   \[ \] \[geopoint\](<https://specs.frictionlessdata.io/table-schema/#geopoint>)
-   \[ \] \[geojson\](<https://specs.frictionlessdata.io/table-schema/#geojson>)

Additionally:

-   \[ \] Validate field values against [`constraints`](https://specs.frictionlessdata.io/table-schema/#constraints) property
-   \[ \] For [string](https://specs.frictionlessdata.io/table-schema/#string), validate values against `format` property
-   \[ \] For [number](https://specs.frictionlessdata.io/table-schema/#number), support `currency` property - *What does this property contain?*

### Resources & Packages

-   \[ \] Validate packages against the default or custom [`profile`](https://specs.frictionlessdata.io/profiles/) - *The published schemas don't match the latest specifications?!*
-   \[ \] Read/write resources with a `path` like "data/data.csv.gz" to/from compressed files
-   \[ \] Write resource schemas with a `path` to a JSON file
-   \[ \] Read/write GeoJSON and TopoJSON to/from spatial objects

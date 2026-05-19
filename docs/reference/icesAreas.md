# ICES fishing areas

Food and Agriculture Organization Major Fishing Area 27 (i.e. ICES
region) fishing area polygons. The polygons are not cut with land as
opposed to the areas distributed on the ICES website (see
`BioticExplorerServer:::prepareICESareas`). This makes the polygons
smaller and consequent calculations quicker. Use this dataset instead of
the function.

## Usage

``` r
icesAreas
```

## Format

[`st_polygon`](https://r-spatial.github.io/sf/reference/st.html) in
decimal degrees (EPSG:4326).

## Source

[International Council for the Exploration of the
Sea](https://gis.ices.dk/sf/index.html)

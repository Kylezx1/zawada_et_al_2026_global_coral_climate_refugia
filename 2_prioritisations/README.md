# 50-Reefs-Prioritisations
Prioritisations stage of the 50 reefs + project - Macquarie University and The Wildlife Conservation Society
Coral Conservation Portfolio — Region Metadata & File Guide
===========================================================
This document explains what files exist in this region, what they contain, how
their variables are defined, and how each pipeline uses them. It is meant to be
placed inside each REGION folder so collaborators can quickly understand the data
products and their relationships.

Two Key Directories Per Region
------------------------------
1) Primary region directory (the region root, e.g. `<PARENT_FOLDER>/<REGION>`)
   - Holds canonical inputs/sidecars from Pipeline 1 (P1) and raw metrics from P2.
   - Typical items: grid polygons (projected), CRS sidecars, and P2 metrics RDS.

2) Selection directory (e.g. `<PARENT_FOLDER>/<REGION>/Selection/`)
   - Holds all selection and reporting artifacts from P3 and P4.
   - Typical items: P3 optimization results (RDS), P4 analysis grid, figures,
     tables, interactive map, and region/global summaries.


Pipeline-by-Pipeline Overview
-----------------------------

P1 — Grid & CRS (found in the PRIMARY region directory)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Purpose: build the analysis grid and permanently record the projected, planar CRS.

Key files:
- grid_polygons_laea.rds  : sf POLYGON/MULTIPOLYGON in the saved projected CRS (meters).
- grid_crs.rds            : CRS sidecar; may include wkt / input / epsg.
- grid_crs_wkt.txt        : Text WKT fallback.

Typical fields in grid_polygons_laea.rds:
- grid_id   (chr) : Stable unique id for each grid cell (5 km² nominal).
- ECOREGION (chr) : Marine ecoregion name for the cell.
- geometry  (sfc) : Cell polygon in the projected CRS.

Notes:
- The projected CRS is **planar** (not long/lat) to ensure distances/areas
  are correct in meters. P2–P4 reuse this CRS.
- P1 may also define per-cell reef-extent area in later steps of the pipeline
  (see P2) but the canonical shape/CRS originate here.


P2 — Conservation Metrics (found in the PRIMARY region directory)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Purpose: compute per-cell ecological metrics and reef extent & effective cover.

Key file:
- conservation_metrics_<delta>.rds : sf with per-cell metrics in the projected CRS.

Important variables emitted by P2:
- extent_area_present_m2_total_cover (num, m²)
    Reef extent per cell (area of small reef patches aggregated into the 5 km grid).
    **This is an acceptable reef-extent column.**
- effective_cover_present_m2_total_cover (num, m²)
    "Effective" (absolute) coral cover (present). This is NOT an area of grid,
    but a sum of cover; used for ecological signal and some objective components.
- total_risk, local_risk_* (num)         : risk layers.
- temporal_score_* (num)                  : temporal objective components.
- trait_present_* (lgl)                   : presence flags (competitive, weedy, etc.).
- ECOREGION, grid_id, geometry            : identity and shape.

Notes:
- 30% protection TARGETS are evaluated against **reef extent** (not effective cover).
- Effective cover is reported in parallel for ecological interpretation.


P3 — Optimization (found in the Selection directory)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Purpose: convert metrics into an optimized portfolio (selected vs not selected).

Key file:
- Selection/portfolio_optimization_results_<delta>.rds
    A list keyed by ecoregion; each entry contains:
      $grid      : sf with per-cell variables and `selected` (TRUE/FALSE)
      $stats     : tibble summary for that ecoregion
      $trait_*   : (optional) trait matrices used in complementarity

Important fields inside each `$grid` (carried into P4):
- grid_id, ECOREGION
- pixel_area (num, m²)             : **Reef extent per cell prepared by P3 (preferred).**
- extent_area_present_m2_total_cover (num, m²) : Reef-extent fallback (from P2).
- effective_cover_present_m2_total_cover (num, m²) : Effective cover (present).
- obj_temporal, obj_risk, obj_complementarity, obj_spatial (+ _z variants)
- optional: obj_temporal_eff, obj_temporal_eff_z        (density/efficiency)
- fragmentation_risk (num)
- selected (lgl)
- geometry (sfc POLYGON/MULTIPOLYGON in projected CRS)

Notes:
- P3 enforces that neighbor distances & areas are computed in the saved planar CRS.
- Budgeting is done on reef extent (sum of `pixel_area` within the selection).


P4 — Post-Optimization Analysis & Reporting (found in the Selection directory)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Purpose: assemble one per-region analysis grid; compute protection summaries;
produce interactive/static maps, figures, and tables.

Core outputs (Selection/):
- analysis_grid.rds
    sf polygons in the projected CRS, merged across ecoregions; carries `selected`
    and all relevant variables (from P3 grid). This is the canonical artifact for
    figures and tables.
- combined_grid.rds
    A WGS84 copy for quick external plotting or GIS usage.
- region_summary.csv
    One row per region (schema below).

Figures (Selection/):
- Figure1_Spatial_Distribution.png
    Map + subtitle showing both:
      "Eff. cover: <share>% selected | Coral extent: <share>% protected"
    (Both shares recomputed within P4; extent strictly uses the reef-extent columns.)

- Figure2…Figure6b.png : Objective distributions, trade-offs, and summaries.

Tables (Selection/tables/):
- Table_Target_Attainment.png (+ .html)
    Ecoregion table showing **extent-based** protected % (benchmark against 30% target).
- Table_Effective_Cover_Protected.png (+ .html)
    Effective (absolute) cover protected; informative only (no 30% target).

Interactive map (Selection/interactive/):
- portfolio_map_<REGION>.html
    Leaflet, WGS84, polygons colored by selection status.


Variable Dictionary (Key Fields You Will See)
---------------------------------------------

Identity & geometry:
- grid_id   (chr) : Unique cell identifier (stable across pipelines).
- ECOREGION (chr) : Marine ecoregion.
- geometry  (sfc) : Polygon in projected CRS (P1–P4 use the same local CRS).

Reef-extent (area) columns — used for protection targets (30%):
- pixel_area (num, m²)                      : P3’s preferred **reef extent** per cell.
- extent_area_present_m2_total_cover (num)  : Reef-extent fallback (from P2).
- extent_area_present_m2 (num)              : Legacy reef-extent name (fallback).

Effective (absolute) coral cover — ecological signal, **not** the 30% target:
- effective_cover_present_m2_total_cover (num, m²) : Sum of coral cover per cell (present).
  Reported as totals/selected shares; also used to shape temporal/density objectives.

Objective system (if present):
- obj_temporal, obj_risk, obj_complementarity, obj_spatial (num)
- obj_temporal_z, obj_risk_z, obj_complementarity_z, obj_spatial_z (num)
- obj_temporal_eff, obj_temporal_eff_z (num)           : optional density/efficiency terms.
- fragmentation_risk (num)

Selection:
- selected (lgl) : TRUE if cell is in the optimized portfolio.


Where Variables Are Used
------------------------

- grid_id / ECOREGION : all pipelines; identity, joins, and ecoregion-wise reporting.
- geometry : all pipelines; P4 transforms to WGS84 for mapping.
- pixel_area / extent_area_present_* :
    *P3* budgeting and *P4* protection math (strictly reef extent). P4 **never**
    uses polygon geometry area nor `grid_area_m2` for protection percentages.
- effective_cover_present_m2_total_cover :
    Shaping objectives (P2→P3) and P4’s **informative** effective-cover summaries.
- selected :
    From P3; drives P4 figures/tables, maps, and summary statistics.
- objective_* & *_z :
    Diagnostics and figure content in P4 (Figures 2,3,3b,4,6a,6b).


Protection Computation (P4 Guardrail)
-------------------------------------
P4 always computes reef-extent protection using the first available column in:
    1) pixel_area
    2) extent_area_present_m2_total_cover
    3) extent_area_present_m2
No geometry-area or grid-area columns are used.

Formulas (per region):
- Original_Extent_km2  = sum(reef_extent_column) / 1e6
- Protected_Extent_km2 = sum(reef_extent_column where selected) / 1e6
- Extent_Protection_%  = 100 × Protected / Original

Effective cover (present) is summarized in parallel:
- Effective_Cover_Selected_km2        = sum(effective_cover_present_m2_total_cover where selected) / 1e6
- Effective_Cover_Selected_Percent    = 100 × selected_effective / total_effective
(No target is applied to effective cover.)


region_summary.csv — Field Schema
---------------------------------
- Region (chr)
- Grid_Type (chr) : “Polygon/Hexagon” (usually) or “Point” (fallback only).
- Total_Grid_Cells (int)
- Selected_Cells (int)
- Selection_Rate (num, %) : count-based share; for context only.
- Original_Extent_km2 (num)             : reef extent total (km²).
- Protected_Extent_km2 (num)            : reef extent selected (km²).
- Extent_Protection_Percent (num, %)    : 100 × Protected/Original (extent-based target).
- Hybrid_Geometry_Available (lgl)
- CRS_Used (chr)
- (optional) Effective_Cover_Selected_km2 (num)
- (optional) Effective_Cover_Selected_Percent (num, %)

Tip: When in doubt, trust P4’s recomputed extent percentage; it uses only the
reef-extent columns listed under the guardrail above.


Minimal Run Order
-----------------
1) P1 → write grid_polygons_laea.rds and CRS sidecars to PRIMARY region directory.
2) P2 → write conservation_metrics_<delta>.rds to PRIMARY region directory.
3) P3 → write portfolio_optimization_results_<delta>.rds to Selection/.
4) P4 → read P3 results, produce analysis_grid.rds, figures, tables, and summaries
        in Selection/. Optionally write combined_grid.rds in WGS84.


Quick QA Checklist (per region)
-------------------------------
- analysis_grid.rds is an sf polygon grid with `selected` and at least one reef-extent column.
- region_summary.csv values match P4 recomputation (Original/Protected/Percent).
- Figures and tables exist and read sensibly; Table_Target_Attainment uses extent.
- Interactive HTML map opens with basemap + overlays in Selection/interactive/.
- Global CSVs in the top-level folder include your region’s row(s).

Contact & Notes
---------------
- If any protection % seems off, confirm the reef-extent columns exist:
  `pixel_area`, `extent_area_present_m2_total_cover`, or `extent_area_present_m2`.
  Contact: joseph.mbui@mq.edu.au
  
- Potential areas of imporovement
  Gurobi implements a binary selections; fiuture iterations will impement NSGA algporithm from NSGA package. 
  
- If the interactive map is blank, check that geometry is polygonal and that CRS
  transformation to WGS84 succeeds (see CRS sidecars in the PRIMARY region directory).

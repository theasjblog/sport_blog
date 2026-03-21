# Storage Strategy

This document defines what should live in Git, what should live in GCS, and what should be generated on demand for this Quarto sport blog.

## Goals

- Keep the Git repo small, readable, and source-focused.
- Keep large binary assets out of Git history.
- Make image and data publishing repeatable.
- Separate source-of-truth assets from public website assets.
- Avoid storing generated output unless there is a clear reason.

## Current Findings

- The repo is now mostly source-only after the history reset and cleanup.
- Most post media used by the site is already hosted in GCS under `storage.googleapis.com/blogs_josa/sport_small/...`.
- Some pages still depend on remote parquet reads from GCS during render.
- The current scripts reflect an ad hoc workflow:
  - `inst/img_resize.R` depends on local folders under `Downloads`.
  - `inst/gcs.R` uploads local files with path substitution rules.
- The storage boundary is working in practice, but it is not explicit or enforced.
- There is still at least one local post image folder in Git: `posts/others/2025-10-15_WOLLONGONG_COST/img`.
- `_freeze/` is still tracked, so the repo currently keeps some generated render state.

## Recommended Rule Set

Use this simple policy:

- Git contains source.
- GCS contains binaries and published derived assets.
- Quarto build output is generated on demand.

## What Should Live In Git

- Quarto source files:
  - `*.qmd`
  - `_quarto.yml`
  - `_metadata.yml`
  - CSS and small supporting config files
- R scripts and helper code used to build or publish the site
- Small permanent site assets:
  - logo
  - favicon
  - profile image
  - small icons
- Documentation and workflow files:
  - `README.md`
  - `TODO.md`
  - this file

Git should not contain:

- post galleries
- full-size photos
- videos
- temporary resized image outputs
- generated caches
- rendered site output
- large datasets

## What Should Live In GCS

### Private or Cold Storage

Use GCS private storage for source-of-truth binary assets:

- original photos
- original videos
- raw exported datasets
- archival assets that should not be served directly to the site

Suggested prefix layout:

- `sport/raw/...`
- `sport/data/private/...`

### Public Website Storage

Use GCS public storage only for derived assets that are safe and intended for the website:

- compressed post images
- thumbnail images
- lightbox/full-resolution web images
- compressed/public videos
- page-specific public datasets

Suggested prefix layout:

- `sport/web/thumb/...`
- `sport/web/content/...`
- `sport/web/lightbox/...`
- `sport/data/public/...`

## What Should Be Generated On Demand

These should never be treated as source of truth:

- `_site/`
- `posts/**/index_cache/`
- `posts/**/index_files/`
- temporary image resize outputs
- one-off upload logs
- intermediate export folders

These can be deleted and recreated at any time.

## Recommended Media Model

For each image, keep distinct versions for distinct use cases:

- `thumb`: for cards, listings, previews
- `content`: for normal in-post images
- `lightbox`: for enlarged image viewing

Do not use a single image variant for all three purposes.

Preferred format rules:

- use WebP for photographic images when practical
- keep PNG only when transparency or screenshot sharpness matters
- keep videos outside Git and publish compressed versions only

## Recommended Data Model

Separate data into two categories:

- source data
- public site payload

Source data:

- should live outside Git
- should usually be private in GCS
- can be large, messy, and designed for analysis

Public site payload:

- should be small
- should be stable
- should be purpose-built for specific pages
- should expose only the columns needed by the site

Examples:

- `race_finder.qmd` should consume a compact published dataset for the race finder, not a broader upstream table if avoidable.
- `records.qmd` should consume a small derived ranking payload, not a more general raw dataset.

## Recommended Directory and Ownership Rules

### In Git

Post folders should normally contain only:

- `index.qmd`
- metadata if needed

Avoid storing:

- `img/`
- `img_modal/`
- generated figures
- local media exports

The only exceptions should be very small, intentional assets that are simpler to keep in Git than in GCS.

### In GCS

Each published post media set should live under a stable path derived from the post slug.

Example structure:

- `sport/web/thumb/2025-07-13_BRACEBRIDGE_SPRINT_TRIATHLON/...`
- `sport/web/content/2025-07-13_BRACEBRIDGE_SPRINT_TRIATHLON/...`
- `sport/web/lightbox/2025-07-13_BRACEBRIDGE_SPRINT_TRIATHLON/...`

This is clearer than relying on one catch-all namespace like `sport_small`.

## Workflow Recommendations

### Image Workflow

Replace the current ad hoc process with one explicit pipeline:

1. Put originals in private GCS or a local staging area outside Git.
2. Run a script that:
   - validates file types
   - creates image variants
   - writes outputs to predictable paths
   - optionally creates a manifest
3. Upload derived web assets to public GCS.
4. Reference only published derived assets from post content.

The current scripts in `inst/img_resize.R` and `inst/gcs.R` should be replaced or refactored into a single documented workflow.

### Data Workflow

Use a similar publish pattern for datasets:

1. Maintain raw datasets outside Git.
2. Create small page-specific derived datasets.
3. Publish only those derived datasets to public GCS.
4. Point Quarto pages at the derived public paths.

## `_freeze` Policy

Keep `_freeze/` only if it provides enough value in:

- reproducibility
- faster repeat renders
- stable CI behavior

Otherwise, remove it from Git and regenerate as needed.

Recommendation for now:

- keep `_freeze/` temporarily
- revisit later once the data and image pipelines are more stable

It is not the main storage problem anymore.

## Immediate Next Actions

- Decide whether local post-level `img/` folders are allowed at all.
- Replace `sport_small` with a clearer public web asset naming convention.
- Refactor image resize and upload into one documented script.
- Create small public datasets for high-traffic interactive pages.
- Decide whether `_freeze/` remains tracked.
- Add this policy to the normal content/publishing workflow.

## Proposed Default Policy

If there is doubt, use this rule:

- source text and code go in Git
- original binaries go in private GCS
- website-ready binaries go in public GCS
- generated output is disposable

That rule is simple enough to follow consistently and should keep the repo clean over time.

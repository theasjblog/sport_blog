# TODO

This file collects cleanup and improvement ideas for the repo and site.

## Your Inputs

- Reduce repo size. The repo checkout is small, but `.git` is about `1.9G`, so the main issue is Git history, not the current working tree.
- Improve image handling. Images are stored in GCS, but the current setup still needs a cleaner small-image workflow and better delivery for performance.
- Review how data is stored in GCS and how the site reads it.
- Review UI elements and suggest site improvements.

## High Priority

- Rewrite Git history to remove old large image blobs and other generated artifacts from history.
  Notes:
  The biggest blobs in history are old files like `posts/**/img_modal/*.png`, many of them 13-19 MB each. Use `git filter-repo` or BFG, then force-push carefully.
- Stop tracking generated render artifacts in the content tree.
  Notes:
  There are many committed `posts/**/index_cache` folders and a few `posts/**/index_files` folders. These make the repo noisy and should be ignored unless there is a deliberate reason to keep them.
- Decide what should live in Git, what should live in GCS, and what should be generated on demand.
  Notes:
  Right now the boundary is not very explicit. Write down a simple rule such as:
  raw media in GCS cold storage, derived web media in GCS public storage, source content in Git, build outputs not tracked.

## Repo Cleanup

- Add ignore rules for common junk and generated files.
  Notes:
  `.DS_Store`, `log.log`, `log_2.log`, `posts/**/index_cache/`, `posts/**/index_files/`, and similar local artifacts should not be tracked.
- Review whether `_freeze/` should stay in Git.
  Notes:
  It is only about `9.5M` now, so it is not the main size problem. Keep it only if it materially improves reproducibility or deploy speed.
- Remove dead or one-off scripts, or move them into a documented `scripts/` workflow.
  Notes:
  Files like `inst/gcs.R` and `inst/img_resize.R` look like useful utilities, but they are currently ad hoc and include interactive/local-path assumptions.
- Add a short maintenance guide.
  Notes:
  Document how to add a post, upload images, generate small variants, refresh data, and publish.

## Image Pipeline

- Create a proper image pipeline with at least three variants per image.
  Notes:
  Suggested variants: `thumb`, `content`, and `lightbox` or `full`. Do not use one asset for every use case.
- Convert photographic images to WebP and keep PNG only where transparency or screenshots require it.
- Add responsive images instead of fixed-width markdown images where possible.
  Notes:
  Several pages embed remote images with a fixed width like `width="500"` or very small gallery widths like `width="100"`. This is simple, but not ideal for performance or layout.
- Separate original uploads from public website assets in GCS.
  Notes:
  Use a naming scheme like `sport/raw/...`, `sport/web/...`, and `sport/thumb/...` instead of relying on manual folder swaps.
- Replace interactive/manual upload steps with a repeatable script.
  Notes:
  `inst/gcs.R` currently uses `gcs_auth(file.choose())`, local logs, and string substitutions. This should become a script with config, validation, and predictable output.
- Add an image manifest if you want long-term consistency.
  Notes:
  A CSV or parquet manifest can track original path, derived path, width, height, format, and alt text status.

## Data and Build Workflow

- Reduce dependence on live remote reads during render where possible.
  Notes:
  `race_finder.qmd`, `records.qmd`, and some scripts read parquet files directly from GCS over HTTP. That works, but it makes builds depend on remote availability and data format stability.
- Precompute small page-specific datasets for interactive pages.
  Notes:
  Instead of querying large shared parquet files on render, publish lean derived datasets for the race finder and records pages.
- Add a data contract for the parquet files in GCS.
  Notes:
  Define expected columns, types, and refresh cadence so the site does not silently break when upstream data changes.
- Consider a scheduled data refresh workflow.
  Notes:
  There is no `.github/` workflow right now. A GitHub Actions job could render, validate links/data access, and publish in a repeatable way.
- Add basic failure handling for missing remote data.
  Notes:
  If GCS data is unavailable, the page should fail clearly or fall back to a cached local snapshot.

## UI and Site Structure

- Redesign the homepage so it feels like a real front page rather than a short welcome plus two large linked images.
  Notes:
  Good candidates: recent posts, featured race report, key stats, topic cards, and a clearer introduction.
- Add real site styling.
  Notes:
  `styles.css` is effectively empty and the site currently relies almost entirely on the default `cosmo` theme.
- Improve listing pages.
  Notes:
  The listing pages are functional, but they would benefit from card layouts, excerpts, stronger thumbnails, and clearer category styling.
- Review navigation and metadata.
  Notes:
  The RSS navbar link looks misconfigured, and there are a few content typos in the main config and homepage copy that are worth cleaning up.
- Make page types more distinct.
  Notes:
  Race reports, gear reviews, season summaries, and travel posts should not all look identical.
- Review mobile layout carefully.
  Notes:
  Fixed-width images and large embedded tables are likely to feel cramped on small screens.

## Content and Information Architecture

- Standardize post structure.
  Notes:
  Define a consistent pattern for hero image, result summary, analysis, gallery, and related links.
- Improve metadata consistency across posts.
  Notes:
  Categories exist, but you could also add tags, summary text, location, sport, season, and featured flags.
- Add a drafts or backlog workflow.
  Notes:
  The current `README.md` has useful future post ideas. Move those into a more explicit backlog section or content calendar.

## Concrete Findings From This Review

- `.git` is about `1.9G`, which is the dominant size problem.
- Current checked-out content is relatively small:
  `_freeze/` about `9.5M`, `posts/` about `11M`, `img/` well under `2M`.
- No GitHub Actions workflow is present.
- No `.quartoignore` was present before this review.
- `styles.css` is effectively empty.
- Several data pages depend on remote parquet reads from GCS during render.
- The repo still contains many per-post render caches under `posts/**/index_cache`.
- A few posts also contain committed `index_files` directories that duplicate rendered assets.

## Candidate First Pass

- Clean history with `git filter-repo`, then force-push once.
- Add durable ignore rules for generated files and local junk.
- Replace the ad hoc image upload process with a scripted resize-and-publish workflow.
- Create a lightweight custom homepage and custom listing cards.
- Add one GitHub Actions workflow for render, link checking, and deploy.
- Create small derived datasets for `race_finder.qmd` and `records.qmd`.

## Existing Ideas From README

- Age group qualified
- Transfer from medals to blog (multisport only)
- Schedule
- Gear
- Road to Australia
- Planning: season, week techniques, etc.
- Injuries
- Strength
- Script to create the slower res images
- Page with the DT table to find races
- Add a map preview, or a preview of the Garmin site

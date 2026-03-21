# sport_blog

Quarto-based sport blog with source content in Git and post media hosted in GCS.

## Core Rule

- Keep text, code, and site configuration in Git.
- Keep post media in GCS under `https://storage.googleapis.com/blogs_josa/sport/blog_assets/...`.
- Treat render output and caches as disposable.

## Post Structure

Each post should normally contain:

- `posts/<category>/<post-slug>/index.qmd`

If a post needs images during authoring, create a temporary local folder next to the post:

- `posts/<category>/<post-slug>/img/`

That local `img/` folder is only staging. It should not be kept as the long-term source of truth.

## Media Layout In GCS

Published post media lives under:

- `sport/blog_assets/<post-slug>/img/thumbnail/<file>.jpg`
- `sport/blog_assets/<post-slug>/img/full/<file>.jpg`

Thumbnails are used in-page.
Full images are used for the gallery popup/lightbox behavior.

## Creating A New Post

1. Create the post folder and `index.qmd`.
2. Write the post content first.
3. If the post has media, create a temporary local `img/` folder next to `index.qmd`.
4. Put the source images in that local `img/` folder.
5. If the post needs a standard race summary table, add a marker and include:

```md
<!-- summary_race_id: 2025-10-17_ABC123 -->
{{< include summary_table.html >}}
```

or:

```md
<!-- summary_race_date: 2025-10-17 -->
{{< include summary_table.html >}}
```

6. Prepare the post for render:

```bash
zsh scripts/prepare_post_for_render.sh posts/<category>/<post-slug>
```

This wrapper does two things:

- if `posts/<category>/<post-slug>/img/` contains source `.jpg`, `.jpeg`, or `.png` files, it creates `img/full/` and `img/thumbnail/`
- if `index.qmd` includes `summary_table.html`, it creates `summary_table.html` next to the post using `race_id` first, then `race_date`

7. Upload `img/full/` and `img/thumbnail/` to GCS under:

```text
sport/blog_assets/<post-slug>/img/
```

8. Reference only the GCS URLs from `index.qmd`.

Example thumbnail:

```text
https://storage.googleapis.com/blogs_josa/sport/blog_assets/<post-slug>/img/thumbnail/00.jpg
```

Example full image:

```text
https://storage.googleapis.com/blogs_josa/sport/blog_assets/<post-slug>/img/full/00.jpg
```

9. After upload, remove the local source images and temporary local post `img/` folder if no longer needed.

## Recommended Workflow

Use this order for normal publishing work:

1. Start from a template:
   - `templates/race_report_template.qmd`
   - `templates/note_template.qmd`
2. Create the new post under:
   - `posts/race_reports/<post-slug>/index.qmd`
   - or `posts/others/<post-slug>/index.qmd`
3. If the post is still early, keep it under:
   - `posts/_drafts/<post-slug>/index.qmd`
4. If the post needs images, stage them locally in:
   - `posts/<category>/<post-slug>/img/`
5. Run:

```bash
zsh scripts/prepare_post_for_render.sh posts/<category>/<post-slug>
```

6. Upload `img/full/` and `img/thumbnail/` to:
   - `sport/blog_assets/<post-slug>/img/`
7. Update the post so all post media points to GCS.
8. Render and review locally.
9. Remove temporary local post images after upload if they are no longer needed.

The prep script is the standard step before `quarto render`.

## Render command

```
rm -rf .quarto _site _freeze
quarto render
```
## Summary Table Pattern

For standard race-report summary tables, do not render them through R during `quarto render`.

Instead:

1. add a `summary_race_id` or `summary_race_date` marker in the post
2. include `summary_table.html`
3. run the prep script

Quarto then includes that static HTML fragment directly, so the post does not need to start R or build a `DT` widget for the standard summary table.

If you want to run only the summary-table step:

```bash
Rscript scripts/materialize_post_summary_tables.R posts/<category>/<post-slug>
```

## Image Preparation

If you want to run only the image step:

```bash
zsh scripts/generate_image_variants.sh posts/<category>/<post-slug>
```

This creates:

- `img/full/`
- `img/thumbnail/`

## Gallery Pattern

Gallery thumbnails in posts should use the thumbnail asset and link to the matching full asset.

Example:

```md
[![](https://storage.googleapis.com/blogs_josa/sport/blog_assets/<post-slug>/img/thumbnail/00.jpg){ width="100" group="hi" }](https://storage.googleapis.com/blogs_josa/sport/blog_assets/<post-slug>/img/full/00.jpg)
```

## Drafts And Backlog

Use two different buckets:

- `posts/_drafts/`
  For real draft posts that already have a title, structure, and some content.
- `backlog/post_ideas.md`
  For loose ideas, fragments, and posts that are not ready to become draft folders yet.

Suggested flow:

1. Capture ideas in `backlog/post_ideas.md`.
2. Promote real ideas into `posts/_drafts/<post-slug>/index.qmd`.
3. Move the draft into `posts/race_reports/` or `posts/others/` when it is ready to publish.

## Recommended Race Report Template

Use `templates/race_report_template.qmd` as the default starting point.

Recommended order:

1. Front matter with `title`, `subtitle` if useful, `date`, `description`, `categories`, and `image`
2. Intro / context
3. Race narrative sections
4. Gallery
5. Reflections / results
6. Related-post callout if part of a series
7. Static summary table include at the bottom if the post maps to a race entry

## Recommended Notes Template

Use `templates/note_template.qmd` as the default starting point.

Recommended order:

1. Front matter with `title`, `date`, `description`, `categories`, and `image`
2. Intro framing the topic
3. Main sections
4. Optional image or gallery blocks
5. Optional related-post callout if part of a series

Only use a summary table when the post genuinely needs race-specific structured data.

## Placement Rules

- Gallery:
  place it after the main narrative or analysis, not before the reader has any context
- Summary table:
  place it near the bottom of race reports, after the story and reflections
- Related-post links:
  use a callout near the end when the post belongs to a series or cluster
- Hero/listing image:
  make sure `image:` points to a representative thumbnail asset in GCS

## Checking For Broken Media

During refactoring or large media updates, you can check referenced assets with:

```bash
python3 scripts/check_missing_blog_assets.py index.qmd posts
```

This is mainly a maintenance/refactoring helper, not part of the normal long-term publishing flow.

## Notes

- Root-level site assets under `img/` are still local repo assets.
- Parquet datasets are still read from GCS for now.
- `_freeze/` is ignored and not part of the source of truth.
- More automation for data refresh and publish can be added later, but it is not required for the current workflow.


# TODO

- Consider adding a third image variant for normal in-post content, instead of using only `thumbnail` and `full`.
- Consider converting photographic assets to WebP where that improves delivery without hurting workflow simplicity.
- Consider separating original uploads from public website assets more explicitly in GCS if the current layout becomes limiting.
- Consider defining a simple data contract for parquet files in GCS so schema changes do not silently break pages.
- Consider a scheduled or automated data refresh and publish workflow later.
- Add clearer failure handling for missing remote data during render.

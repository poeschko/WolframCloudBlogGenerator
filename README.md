# Wolfram Cloud Blog Generator

A static site generator for Wolfram Cloud notebooks implemented in Wolfram Language.

This was created for a demo at the [European Wolfram Technology Conference 2018](https://www.wolfram.com/events/technology-conference-eu/2018/).

[Live demo](https://www.wolframcloud.com/objects/jpoeschko/my-blog)

## Deployment

To create your own blog, define the `targetDir` in [BlogGenerator.wl](./BlogGenerator.wl) and evaluate all cells. To redeploy at any time, just execute `deploy[]`.

Write your blog posts as notebooks in the `sourceDir`, with names of the form `yyyy-mm-dd-My Title.nb`.

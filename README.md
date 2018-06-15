# Wolfram Cloud Blog Generator

A static site generator for Wolfram Cloud notebooks implemented in Wolfram Language.

This was generated for a demo at the [European Wolfram Technology Conference 2018](https://www.wolfram.com/events/technology-conference-eu/2018/).

[Live demo](https://www.wolframcloud.com/objects/jpoeschko/my-blog)

## Deployment

To create your own blog, define the `targetDir` in [BlogGenerator.wl](./BlogGenerator.wl) and evaluate all cells. To redeploy at any time, just execute `deploy[]`.

Write your blog posts as notebooks in the `sourceDir`, with names of the form `yyyy-mm-dd-My Title.nb`.

## Limitations

* `Hyperlink` should open URLs to the same (Cloud) domain in the same browser tab. Until this is fixed in cloud notebooks, clicking links in the blog will always open a new tab.
* There is apparently a bug in `DynamicModuleBox` handling, forcing us to explicitly pass in `comments` to functions called from the client side, since otherwise it would not be tracked correctly (i.e. the comments wouldn't automatically update after adding a comment). This workaround just makes the code a *little* more convoluted.

# The Kismesis Engine

The engine for the Kismesis static site generator, encouraging everyone to make static sites that are semantic and accessible.

## How To Make Plugins
This is a basic rundown of how to get started. It doesn't tell you much because proper documentation for this is on the way.

1. Begin by making a new Rust library. 
2. Add `extism-pdk` to that library.
3. Add `kismesis` to that library with the `pdk` feature.
4. In `lib.rs`, add `use extism_pdk::*` and `kismesis::pdk::*`.
5. Create a function `fn parser(Json(input): (Json<PluginInput>)) -> FnResult<Json<PlugResult>>`
6. Make sure your program doesn't panic, as this results in an error report that offers very little help to both you and the user. FnResult uses anyhow::Error, so you can use the elvis operator a lot of the time.
7. Use `PluginParseError::new()` to create a new error at the position of one of your tokens, or the ranges given in the input.
8. Use `.add_hint()` to add a hint to an error.

Check the documentation for `extism-pdk` for help with Extism's features and the Kismesis documentation for help with managing Kismesis tokens and AST nodes

Kismesis Plugins are currently unstable and subject to any sort of breaking change. A more ergonomic API is in the making.

## Why isn't this embedded in the SSG

The Kismesis engine is made as a separate crate from the Kismesis static site generator because this way it's possible to simply export the necessary types as public in order to allow for plugins. The necessary interfaces to make this process more ergonomic are still to come, but it is perfectly possible to make plugins as things are.

## Roadmap
- Run plugins in a separate thread to give them a fresh stack (or figure out a way to optimize this crate's stack usage)
- Improve the PDK in order to make things easier
- Allow for recursive templating
- Output errors for irrefutable infinite recursion (this can only be caused by macros and expressions that unconditionally reference each other, and futurely, by template self-reference)
- Somehow realize when two values have been bouncing around through references a lot???? And report an error for that. Somehow.
- Allow plugins to have a second pass once the AST is compiled, so they can have access to almost-fully-compiled AST.

This is not a checklist. Elements will be deleted from the list as they are completed.

## Warning for site generator developers
The Kismesis engine is meant only for the Kismesis SSG, however, if you wish to use Kismesis on your own project, heed this warning:

After registering a file in the engine, it sticks around until manually deleted. Knowing where to delete it is critical. Your users will see an error specifying what happened if you fail to manage this, and since files can be large, keeping them around forever can be wasteful.

In most cases, template files can be kept around until the end of the runtime. Input files can often be removed immediately after their templating was either successful or a failure.

## Related Projects

- The Kismesis Engine
- The Kismesis Static Site Generator
- The Kismesis Markup Language

You might accurately infer that I think ambiguity is the funniest thing ever. Since all these projects are related I think this is basically harmless unless you're in a server that is both MSPA-related and related to any topic adjacent to this project (like Rust, Static Sites, Extism, the Indie Web, etc). DM me if you find an active community that meets this criterion. This section was meant to be a joke, but now that I write this, I am genuinely curious.

## FAQ
FAQ stands for both Forwardly Anticipated Questions and Frequently-ish Asked Questions.

### Why is this name ambiguous?
I find it funny, and I thinks that since all projects named Kismesis are related, this is safe and okay.

### Why do you plan to error when using `<div>`
- The reason for this is that we often utilize divisions as a catch-all, instead of utilizing more semantic alternatives like `header`, `footer`, `main`, `section`, and even `button`. `<container>` will get compiled as `<div>` in case you really know there is no better alternative. Think of it the same way you think of Rust's `unsafe`.
  - No, `<button>` is not that much harder to style than a div.

For questions about the Kismesis SSG, check [its respective readme](https://github.com/lilith-in-starlight/kismesis-ssg).

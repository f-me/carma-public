# Frontend written in PureScript

## How to deploy

- `npm i && npm run bower i` to install dependencies
- `npm run build` to build a bundle
- `npm run clean` to clean bundle and built PureScript modules (`output` dir)
- `npm run clean-build` to build a bundle from scratch
- `npm run prod-build` to build a bundle for production
- `npm run clean-prod-build` to build a bundle for production from scratch
- `npm run watch`
  to start watcher daemon that will automatically rebuild your changes

If you're about to deploy next release to the production server, just run
(this includes `npm i`, `npm run bower i` and `prod-clean-build`):

```bash
npm run prod-release
```

### Additional tasks

- `npm run debug-build` to build debug bundle
  (optimized but not minified, unreachable code is eliminated)
- `npm run debug-clean-build` to build debug bundle
  (optimized but not minified, unreachable code is eliminated) from scratch

## Development **WARNINGS**

- When you use `createElement` always prebind it in `where` section of a class
  otherwise a class will be always new and will be mounted every render.

  Like this:
  ```purescript
  -- some class
  where
    resourcesRenderEl = createElement resourcesRender
    richTextEditorEl = createElement richTextEditor
  ```

- Some dependencies are attached as git submodules such as these:

  - purescript-react-dropzone
  - purescript-react-rich-text-editor

  They will be fetched during `npm install` command (see `postinstall` task in
  [package.json](package.json)) but for Circle-CI for some reason it seems
  they're not fetched. That's why `git submodule ...` commands added to
  Circle-CI task for “pure” frontend explicitly after `npm install` and that's
  working. If you're about to add another dependency this way, you may face same
  issue and now you know how you could fix it.

## About PureScript

Compact instruction that will help you to understand it from Haskell perspective
if you haven't met it before (with comparsions, examples and useful links):

https://github.com/unclechu/purescript-for-haskellers

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

### Additional tasks

- `npm run debug-build` to build debug bundle
  (optimized but not minified, unreachable code is eliminated)
- `npm run debug-clean-build` to build debug bundle
  (optimized but not minified, unreachable code is eliminated) from scratch

## About PureScript

Compact instruction that will help you to understand it from Haskell perspective
if you haven't met it before (with comparsions, examples and useful links):

https://github.com/unclechu/purescript-for-haskellers

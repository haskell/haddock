# Compiling

* Install [node](https://nodejs.org/) and [npm](https://www.npmjs.com)
* Run `npm install` and `npm install gulp-cli -g` in this directory.
* Run `gulp` in this directory. This rebuilds the minified JS files.

# Manual Testing

Generate haddock docs for some Haskell project and replace the generated JS files:

```
gulp && cp *.min.js path-to/generated-haddock-docs
```
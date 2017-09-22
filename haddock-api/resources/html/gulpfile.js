const gulp = require('gulp');
const uglify = require('gulp-uglify');
const browserify = require('browserify');
const source = require('vinyl-source-stream');
const buffer = require('vinyl-buffer');
const tsify = require('tsify');

function buildJS(targetFileName, files) {
  var b = browserify({ entries: files });
  return b
    .plugin(tsify)
    .bundle()
    .pipe(source(targetFileName))
    .pipe(buffer())
    .pipe(uglify().on('error', function(e) { console.log(e); }))
    .pipe(gulp.dest('.'));
}

gulp.task('build-js', function() {
  buildJS('quick-jump.min.js', ['./js-src/quick-jump.ts']);
  buildJS('haddock-bundle.min.js', ['./js-src/init.ts']);
});

gulp.task('default', ['build-js']);
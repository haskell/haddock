cabal test html-test hypsrc-test --test-option=--accept
cp -r html-test/ref/* goldens/new
cp -r hypsrc-test/ref/src/* goldens/new/src

npx --prefix goldens prettier --write goldens
diff -r goldens/old goldens/new > goldens.diff
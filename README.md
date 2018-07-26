# elm-svg-gen

This is a pretty quick and dirty solution for generating [`elm-lang/svg`](http://package.elm-lang.org/packages/elm-lang/svg/latest) code from `.svg` files. I threw it together for work, where we had a bunch of vector icon files that we wanted to have Elm functions for. 

It's probably not suitable for large and(or) complex svg files, as the parser can quite easily hit the `node` stack limit (hint: `node --stack_size=N`). This is because the parser is entirely recursive atm. While this might be quite limiting, it works well enough for my purposes right now. YMMV.

## Installing

```
curl https://raw.githubusercontent.com/jmackie/elm-svg-gen/master/main.js > my/node/stuff/elm-svg-gen.js
```

## Building

```
psc-package install
pulp build -O --to elm-svg-gen.js    # or whatever you want to call it
uglifyjs main.js -o elm-svg-gen.js   # optional
```

## Running

```
node elm-svg-gen.js -- icon.svg another-icons.svg ...
```

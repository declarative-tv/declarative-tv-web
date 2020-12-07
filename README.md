# Fpers

> This project was bootstraped from
> [purescript-halogen-realworld](https://github.com/thomashoneyman/purescript-halogen-realworld)

## Installation

First, clone the repository:

```sh
git clone git@github.com:gillchristian/fpers.git
cd fpers
```

You can enter a development shell with all non-JavaScript dependencies via Nix:

```sh
nix-shell
```

> Alternately, you can install Zephyr and
> [twpurs](https://github.com/gillchristian/tailwind-purs) manually. You can
> install the Zephyr binary
> [from its releases page](https://github.com/coot/zephyr/releases) -- ensure it
> exists in your PATH by moving it to `usr/bin/local`. And
> [twpurs](https://github.com/gillchristian/tailwind-purs) has to be built from
> source with Stack.

Next, install JavaScript dependencies:

```sh
npm install
```

## Dev mode

To run it in dev mode where saving your changes rebuilds and reloads the app,
you can run the command below (which calls `spago build --watch`)

```sh
npm run watch
```

And run the application with

```sh
npm run serve-dev
```

This will open your default browser at [port 1234](http://localhost:1234)

[Tailwind](https://tailwindcss.com/) (v1.9) is used for styles.
[twpurs](https://github.com/gillchristian/tailwind-purs) is used to generate
type safe ready to use classes in PureScript code
([src/Tailwind.purs](https://github.com/gillchristian/fpers/blob/master/src/Tailwind.purs)).

The list of all the available classes and it's mapping to PureScript identifier
can be found in
[css/tailwind-classes.txt](https://github.com/gillchristian/fpers/blob/master/css/tailwind-classes.txt).

When using a new class that isn't yet in
[src/Tailwind.purs](https://github.com/gillchristian/fpers/blob/master/src/Tailwind.purs),
run the command below to include it

```
npm run css:lock
```

NOTE: it searches for all the ocurrences of `T.className`. Wheres `T` is:

```purescript
import Tailwind as T
```

To ganerate all the available classes run the command below

```
npm run css:gen:all
```

NOTE: this generates ~19k classes, which can slow down compilation and/or
the language server, make sure to run the `css:lock` command to only include in 
[src/Tailwind.purs](https://github.com/gillchristian/fpers/blob/master/src/Tailwind.purs)
the used classes.

## Building and running

Next, build the project (this command will run `spago build`; see the
[`package.json`](package.json) file to see all helper scripts for the project):

```sh
npm run build
```

You can bundle the JS for production:

```sh
npm run bundle
```

And, once bundled, you can run a local server to use Fpers (defaults to
[port 8080](http://127.0.0.1:8080), but if this port is already in use it will
increment to 8081, etc.):

```sh
npm run serve
```

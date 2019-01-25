# GLO-2003 - Officiel-projet-h19

## Prerequisities

- [Elm](https://elm-lang.org/) 

## Getting started

After you [install](https://guide.elm-lang.org/install.html) elm, run the following commands in your terminal to download this repo and start a server that compiles Elm for you:

```bash
git clone https://github.com/glo2003/officiel-projet-h19.git
cd officiel-projet-h19
elm reactor
```

Now go to [http://localhost:8000/](http://localhost:8000/) and start looking at the `src` directory. There is a `Main.elm` file that you can open to see the app. When you edit an Elm file, just refresh the corresponding page in your browser and it will recompile!

## Build the thing

```bash
elm make src/Main.elm
```

This will compile the sources as a single `index.html` file that you can now serve like any other HTML file.

**Your backend will have to listen to connection on port `9090` as the frontend is configured to send requests to `localhost:9090`. The [java-starter-kit](https://github.com/glo2003/starter-kit-java) is configured by default to listen to port 9090.**



import Elm from "../src/Main.elm";

const BUILD_MODE = import.meta.env.MODE;
if (BUILD_MODE == "development") {
  console.log("doing development stuff");
}

const app = Elm.Main.init({});

import Elm from "../src/Main.elm";

const BUILD_MODE = import.meta.env.MODE;
if (BUILD_MODE == "development") {
  console.log("doing development stuff");
}

let height = document.documentElement.clientHeight;
let width = document.documentElement.clientWidth;

const app = Elm.Main.init({ flags: { width: width, height: height } });

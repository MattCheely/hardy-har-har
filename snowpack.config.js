// Snowpack Configuration File
// See all supported options: https://www.snowpack.dev/reference/configuration

/** @type {import("snowpack").SnowpackUserConfig } */
module.exports = {
  mount: {
    public: "/",
    src: "/src"
  },
  plugins: [["snowpack-plugin-elm", { debugger: "never" }]],
  optimize: {
    bundle: true,
    minify: true,
    target: "es2020"
  },
  packageOptions: {
    /* ... */
  },
  devOptions: {
    /* ... */
  },
  buildOptions: {
    /* ... */
  }
};

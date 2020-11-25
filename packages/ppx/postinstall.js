const fs = require("fs");

const { platform } = process;
if (!fs.existsSync(platform)) {
  throw new Error(platform + " lenses-ppx binary not found");
}

/**
 * Windows needs two ppx files for some reason
 * One extra with *.exe suffix
 */
if (platform === "win32") {
  fs.copyFileSync("win32", "ppx.exe");
  fs.chmodSync("ppx.exe", 0o744);
}

fs.renameSync(platform, "ppx");
fs.chmodSync("ppx", 0o744);

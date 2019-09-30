const { exec } = require("child_process");
const fs = require("fs");

exec("uname", (error, stdout, stderr) => {
  if (error) {
    // probably windows, uname does not exist
    fs.copyFileSync("windows", "ppx.exe"); // copy as windows needs two ppx files for some reason, one extra with exe suffix
    fs.chmodSync("ppx.exe", 0o744);
    fs.renameSync("windows", "ppx");
  } else {
    const platform = stdout.trim().toLowerCase();
    if (!fs.existsSync(platform)) {
      throw new Error(platform + " lenses-ppx binary not found");
    }
    fs.renameSync(platform, "ppx");
  }
  fs.chmodSync("ppx", 0o744);
});

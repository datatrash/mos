const fs = require("fs");
let version = process.env["MOS_VERSION"];
if (!version) {
    console.log("MOS_VERSION environment variable was not set!");
    process.exit(1);
}
console.log(`MOS Version to install: ${version}`);
fs.writeFileSync("MOS_VERSION", version);
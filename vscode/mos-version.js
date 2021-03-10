const fs = require("fs");
const path = require("path");

let version = process.env["MOS_VERSION"];
if (!version) {
    console.log("MOS_VERSION environment variable was not set!");
    process.exit(1);
}
if (!fs.existsSync("out")) {
    fs.mkdirSync("out");
}
console.log(`MOS Version to install: ${version}`);
fs.writeFileSync(path.join("out", "MOS_VERSION"), version);
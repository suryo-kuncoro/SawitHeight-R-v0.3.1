const fs = require('node:fs');
const path = require('node:path');
const root = path.resolve(__dirname, '..');
const required = [
  'package.json', 'src/main.js', 'src/preload.js', 'src/renderer/index.html',
  'src/renderer/styles.css', 'src/renderer/app.js', 'r/pipeline.R',
  'r/check_environment.R', 'r/install_packages.R', 'assets/icon.ico',
  '.github/workflows/build-windows.yml', 'docs/SOURCE_MAPPING.md', 'docs/reference_tutorial.html'
];
let failed = false;
for (const item of required) {
  const full = path.join(root, item);
  if (!fs.existsSync(full) || fs.statSync(full).size === 0) {
    console.error(`MISSING: ${item}`);
    failed = true;
  } else {
    console.log(`OK: ${item}`);
  }
}

const assertions = [
  ['src/renderer/index.html', 'ground-reference-mode'],
  ['src/renderer/index.html', 'gcp-elevation-field'],
  ['src/renderer/index.html', 'monitoring-mode'],
  ['src/renderer/index.html', 'previous-result-shp'],
  ['src/preload.js', 'selectGcpPoints'],
  ['src/preload.js', 'selectPreviousResult'],
  ['src/main.js', "dialog:gcpPoints"],
  ['src/main.js', "dialog:previousResult"],
  ['r/pipeline.R', "gcp_bias"],
  ['r/pipeline.R', "gcp_anchor"],
  ['r/pipeline.R', "period_field('tumb'"],
  ['r/pipeline.R', 'growth_normal_min_m'],
  ['r/pipeline.R', "'PERLU CEK'"],
  ['src/renderer/index.html', 'growth-normal-min-m'],
  ['r/pipeline.R', 'hasil_tinggi_pokok_'],
  ['r/pipeline.R', "Classification != 18L"],
  ['package.json', '0.3.1']
];
for (const [file, token] of assertions) {
  const text = fs.readFileSync(path.join(root, file), 'utf8');
  if (!text.includes(token)) {
    console.error(`ASSERT FAILED: ${file} tidak memuat ${token}`);
    failed = true;
  } else {
    console.log(`ASSERT OK: ${file} -> ${token}`);
  }
}
process.exit(failed ? 1 : 0);

const { app, BrowserWindow, dialog, ipcMain, shell, Menu } = require('electron');
const { spawn, spawnSync } = require('node:child_process');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const readline = require('node:readline');

const APP_NAME = 'SawitHeight R';
let mainWindow = null;
let activeProcess = null;
let activeRun = null;

function getResourcePath(...parts) {
  if (app.isPackaged) return path.join(process.resourcesPath, ...parts);
  return path.join(__dirname, '..', ...parts);
}

function getAppFile(...parts) {
  if (app.isPackaged) return path.join(app.getAppPath(), ...parts);
  return path.join(__dirname, '..', ...parts);
}

function getSettingsPath() {
  return path.join(app.getPath('userData'), 'settings.json');
}

function readSettings() {
  try {
    return JSON.parse(fs.readFileSync(getSettingsPath(), 'utf8'));
  } catch {
    return {};
  }
}

function writeSettings(settings) {
  fs.mkdirSync(path.dirname(getSettingsPath()), { recursive: true });
  fs.writeFileSync(getSettingsPath(), JSON.stringify(settings, null, 2), 'utf8');
}

function createWindow() {
  mainWindow = new BrowserWindow({
    width: 1460,
    height: 920,
    minWidth: 1120,
    minHeight: 720,
    show: false,
    backgroundColor: '#0b0f11',
    title: APP_NAME,
    icon: getAppFile('assets', 'icon.ico'),
    autoHideMenuBar: true,
    webPreferences: {
      preload: path.join(__dirname, 'preload.js'),
      contextIsolation: true,
      nodeIntegration: false,
      sandbox: true,
      devTools: !app.isPackaged
    }
  });

  mainWindow.loadFile(path.join(__dirname, 'renderer', 'index.html'));

  mainWindow.webContents.setWindowOpenHandler(({ url }) => {
    if (/^https?:\/\//i.test(url)) shell.openExternal(url);
    return { action: 'deny' };
  });

  mainWindow.webContents.on('will-navigate', (event, url) => {
    if (url !== mainWindow.webContents.getURL()) {
      event.preventDefault();
      if (/^https?:\/\//i.test(url)) shell.openExternal(url);
    }
  });

  mainWindow.once('ready-to-show', () => {
    mainWindow.show();
    mainWindow.focus();
  });
}

function emitToRenderer(event) {
  if (mainWindow && !mainWindow.isDestroyed()) {
    mainWindow.webContents.send('analysis:event', event);
  }
}

function normalizeExistingFile(filePath) {
  if (!filePath || typeof filePath !== 'string') return '';
  const resolved = path.resolve(filePath);
  return fs.existsSync(resolved) ? resolved : '';
}

function findRscriptCandidates() {
  const candidates = [];
  const bundled = getResourcePath('vendor', 'R', 'bin', 'Rscript.exe');
  if (fs.existsSync(bundled)) candidates.push(bundled);

  if (process.env.R_HOME) {
    candidates.push(path.join(process.env.R_HOME, 'bin', 'Rscript.exe'));
    candidates.push(path.join(process.env.R_HOME, 'bin', 'x64', 'Rscript.exe'));
  }

  const settings = readSettings();
  if (settings.rscriptPath) candidates.push(settings.rscriptPath);

  const localAppData = process.env.LOCALAPPDATA;
  const roots = [
    process.env.ProgramFiles ? path.join(process.env.ProgramFiles, 'R') : null,
    process.env['ProgramFiles(x86)'] ? path.join(process.env['ProgramFiles(x86)'], 'R') : null,
    localAppData ? path.join(localAppData, 'Programs', 'R') : null
  ].filter(Boolean);

  for (const root of roots) {
    try {
      const dirs = fs.readdirSync(root, { withFileTypes: true })
        .filter((d) => d.isDirectory() && /^R-/i.test(d.name))
        .map((d) => d.name)
        .sort((a, b) => b.localeCompare(a, undefined, { numeric: true }));
      for (const dir of dirs) {
        candidates.push(path.join(root, dir, 'bin', 'Rscript.exe'));
        candidates.push(path.join(root, dir, 'bin', 'x64', 'Rscript.exe'));
      }
    } catch {
      // Optional search path.
    }
  }

  try {
    const result = spawnSync('where', ['Rscript.exe'], { encoding: 'utf8', windowsHide: true });
    if (result.status === 0 && result.stdout) {
      candidates.push(...result.stdout.split(/\r?\n/).filter(Boolean));
    }
  } catch {
    // PATH lookup is optional.
  }

  const unique = [];
  const seen = new Set();
  for (const candidate of candidates) {
    const normalized = normalizeExistingFile(candidate);
    const key = normalized.toLowerCase();
    if (normalized && !seen.has(key)) {
      seen.add(key);
      unique.push(normalized);
    }
  }
  return unique;
}

function getRscriptPath(requestedPath = '') {
  const requested = normalizeExistingFile(requestedPath);
  if (requested) return requested;
  return findRscriptCandidates()[0] || '';
}

function basicConfigValidation(config) {
  const errors = [];
  if (!config || typeof config !== 'object') return ['Konfigurasi tidak valid.'];

  const inputs = config.inputs || {};
  const params = config.parameters || {};

  if (!inputs.point_cloud || !fs.existsSync(inputs.point_cloud)) {
    errors.push('File point cloud LAS/LAZ tidak ditemukan.');
  } else if (!/\.(las|laz)$/i.test(inputs.point_cloud)) {
    errors.push('Point cloud harus berformat .las atau .laz.');
  }

  if (!inputs.tree_points || !fs.existsSync(inputs.tree_points)) {
    errors.push('File titik pokok tidak ditemukan.');
  } else if (!/\.(shp|gpkg|geojson|json)$/i.test(inputs.tree_points)) {
    errors.push('Titik pokok harus berformat SHP, GPKG, GeoJSON, atau JSON spasial.');
  }

  const groundMode = params.ground_reference_mode || (params.use_external_dtm ? 'external_dtm' : 'csf_tin');
  const validGroundModes = new Set(['csf_tin', 'external_dtm', 'gcp_bias', 'gcp_anchor']);
  if (!validGroundModes.has(groundMode)) {
    errors.push('Mode referensi tanah tidak dikenali.');
  }

  if (groundMode === 'external_dtm') {
    if (!inputs.external_dtm || !fs.existsSync(inputs.external_dtm)) {
      errors.push('DTM independen dipilih tetapi file raster tidak ditemukan.');
    } else if (!/\.(tif|tiff)$/i.test(inputs.external_dtm)) {
      errors.push('DTM independen harus berformat GeoTIFF (.tif/.tiff).');
    }
  }

  if (groundMode === 'gcp_bias' || groundMode === 'gcp_anchor') {
    if (!inputs.gcp_points || !fs.existsSync(inputs.gcp_points)) {
      errors.push('Mode GCP dipilih tetapi file titik GCP tidak ditemukan.');
    } else if (!/\.(shp|gpkg|geojson|json)$/i.test(inputs.gcp_points)) {
      errors.push('Titik GCP harus berformat SHP, GPKG, GeoJSON, atau JSON spasial.');
    }
    if (!String(params.gcp_elevation_field || '').trim()) {
      errors.push('Field elevasi tanah GCP wajib diisi.');
    }
    if (!Number.isFinite(Number(params.ground_dtm_resolution_m)) || Number(params.ground_dtm_resolution_m) <= 0) {
      errors.push('Resolusi DTM referensi harus lebih besar dari 0.');
    }
  }

  const monitoringMode = String(params.monitoring_mode || 'first').trim();
  if (!['first', 'monitoring'].includes(monitoringMode)) {
    errors.push('Mode periode harus first atau monitoring.');
  }

  const periodCode = String(params.period_code || '').trim().toUpperCase();
  if (!/^[A-Z0-9]{1,3}$/.test(periodCode)) {
    errors.push('Kode periode wajib 1-3 karakter huruf/angka, contoh D1, D2, P01.');
  }

  if (monitoringMode === 'monitoring') {
    if (!String(params.tree_id_field || '').trim()) {
      errors.push('Monitoring membutuhkan Field ID pohon yang stabil dan unik.');
    }
    if (!inputs.previous_result_shp || !fs.existsSync(inputs.previous_result_shp)) {
      errors.push('Monitoring membutuhkan Shapefile hasil periode sebelumnya.');
    } else if (!/\.shp$/i.test(inputs.previous_result_shp)) {
      errors.push('Hasil periode sebelumnya harus berformat .shp.');
    }
    const previousPeriod = String(params.previous_period_code || '').trim().toUpperCase();
    if (!/^[A-Z0-9]{1,3}$/.test(previousPeriod)) {
      errors.push('Kode periode sebelumnya wajib 1-3 karakter, contoh D1.');
    } else if (previousPeriod === periodCode) {
      errors.push('Kode periode saat ini harus berbeda dari periode sebelumnya.');
    }
  }

  if (!inputs.output_root) {
    errors.push('Folder output belum dipilih.');
  }

  const positiveFields = [
    ['buffer_m', 'Buffer'],
    ['canopy_threshold_m', 'Threshold kanopi'],
    ['sor_k', 'SOR k'],
    ['sor_m', 'SOR m'],
    ['csf_cloth_resolution', 'CSF cloth resolution'],
    ['csf_class_threshold', 'CSF class threshold'],
    ['chm_resolution', 'Resolusi CHM'],
    ['chm_min_auto_resolution', 'Minimum resolusi CHM otomatis'],
    ['threads', 'Jumlah thread']
  ];
  for (const [key, label] of positiveFields) {
    if (!Number.isFinite(Number(params[key])) || Number(params[key]) <= 0) {
      errors.push(`${label} harus lebih besar dari 0.`);
    }
  }

  if (Number(params.height_break_1_m) >= Number(params.height_break_2_m)) {
    errors.push('Batas tinggi pertama harus lebih kecil daripada batas tinggi kedua.');
  }

  if (!Number.isFinite(Number(params.growth_normal_min_m)) || Number(params.growth_normal_min_m) <= 0) {
    errors.push('Delta minimum status NORMAL harus lebih besar dari 0 meter.');
  }

  const epsg = Number(params.fallback_epsg || 0);
  if (epsg && (!Number.isInteger(epsg) || epsg <= 0)) {
    errors.push('EPSG fallback harus berupa bilangan bulat positif.');
  }

  return errors;
}

function createRunConfig(config) {
  const timestamp = new Date().toISOString().replace(/[:.]/g, '-');
  const periodCode = String(config?.parameters?.period_code || 'NA').trim().toUpperCase().replace(/[^A-Z0-9]/g, '').slice(0, 3) || 'NA';
  const runName = `run_${periodCode}_${timestamp}`;
  const outputRoot = path.resolve(config.inputs.output_root);
  fs.mkdirSync(outputRoot, { recursive: true });
  const runDir = path.join(outputRoot, runName);
  fs.mkdirSync(runDir, { recursive: true });

  const completeConfig = {
    ...config,
    app: {
      name: APP_NAME,
      version: app.getVersion(),
      created_at: new Date().toISOString(),
      run_dir: runDir
    }
  };
  const configPath = path.join(runDir, 'run_config.json');
  fs.writeFileSync(configPath, JSON.stringify(completeConfig, null, 2), 'utf8');
  return { runDir, configPath, completeConfig };
}

function parseEventLine(line) {
  const marker = 'APP_EVENT:';
  const idx = line.indexOf(marker);
  if (idx < 0) return null;
  const jsonText = line.slice(idx + marker.length).trim();
  try {
    return JSON.parse(jsonText);
  } catch {
    return { type: 'log', level: 'warning', message: `Event R tidak dapat diparse: ${jsonText}` };
  }
}

function getBackendScriptPath(scriptName) {
  const candidates = [];

  if (app.isPackaged) {
    // Primary location: copied by electron-builder extraResources.
    candidates.push(path.join(process.resourcesPath, 'r', scriptName));

    // Fallback location: included via files + asarUnpack.
    candidates.push(path.join(process.resourcesPath, 'app.asar.unpacked', 'r', scriptName));
  } else {
    candidates.push(path.join(__dirname, '..', 'r', scriptName));
  }

  for (const candidate of candidates) {
    if (fs.existsSync(candidate)) return candidate;
  }

  return { missing: true, candidates };
}

function runRProcess({ rscriptPath, scriptName, args = [], modeName }) {
  return new Promise((resolve, reject) => {
    if (activeProcess) {
      reject(new Error('Masih ada proses R yang berjalan.'));
      return;
    }

    const resolvedScript = getBackendScriptPath(scriptName);
    if (typeof resolvedScript !== 'string') {
      reject(new Error(
        `Script backend tidak ditemukan. Lokasi yang diperiksa: ${resolvedScript.candidates.join(' | ')}`
      ));
      return;
    }
    const scriptPath = resolvedScript;

    const child = spawn(rscriptPath, [scriptPath, ...args], {
      windowsHide: true,
      shell: false,
      env: {
        ...process.env,
        R_DEFAULT_PACKAGES: process.env.R_DEFAULT_PACKAGES || 'datasets,utils,grDevices,graphics,stats,methods'
      }
    });

    activeProcess = child;
    activeRun = { modeName, pid: child.pid };
    emitToRenderer({ type: 'process', status: 'started', mode: modeName, pid: child.pid });

    const stdoutLines = readline.createInterface({ input: child.stdout });
    const stderrLines = readline.createInterface({ input: child.stderr });
    const rawStdout = [];
    const rawStderr = [];
    let lastStructuredError = '';

    stdoutLines.on('line', (line) => {
      rawStdout.push(line);
      const event = parseEventLine(line);
      if (event?.type === 'fatal' && event.message) lastStructuredError = event.message;
      if (event) emitToRenderer(event);
      else if (line.trim()) emitToRenderer({ type: 'log', level: 'info', message: line });
    });

    stderrLines.on('line', (line) => {
      rawStderr.push(line);
      if (line.trim()) emitToRenderer({ type: 'log', level: 'error', message: line });
    });

    child.on('error', (error) => {
      activeProcess = null;
      activeRun = null;
      emitToRenderer({ type: 'process', status: 'error', mode: modeName, message: error.message });
      reject(error);
    });

    child.on('close', (code, signal) => {
      activeProcess = null;
      activeRun = null;
      const payload = {
        type: 'process',
        status: code === 0 ? 'completed' : 'failed',
        mode: modeName,
        code,
        signal,
        stderr: rawStderr.slice(-30).join('\n')
      };
      emitToRenderer(payload);
      if (code === 0) resolve(payload);
      else reject(new Error(lastStructuredError || rawStderr.slice(-10).join('\n') || `Rscript berhenti dengan kode ${code}.`));
    });
  });
}

async function selectFile(filters) {
  const result = await dialog.showOpenDialog(mainWindow, {
    properties: ['openFile'],
    filters
  });
  return result.canceled ? '' : result.filePaths[0];
}

ipcMain.handle('dialog:pointCloud', () => selectFile([
  { name: 'Point Cloud', extensions: ['las', 'laz'] }
]));

ipcMain.handle('dialog:treePoints', () => selectFile([
  { name: 'Data titik spasial', extensions: ['shp', 'gpkg', 'geojson', 'json'] }
]));

ipcMain.handle('dialog:previousResult', () => selectFile([
  { name: 'Hasil periode sebelumnya (Shapefile)', extensions: ['shp'] }
]));

ipcMain.handle('dialog:gcpPoints', () => selectFile([
  { name: 'Titik GCP / ground anchor', extensions: ['shp', 'gpkg', 'geojson', 'json'] }
]));

ipcMain.handle('dialog:dtm', () => selectFile([
  { name: 'GeoTIFF DTM', extensions: ['tif', 'tiff'] }
]));

ipcMain.handle('dialog:rscript', () => selectFile([
  { name: 'Rscript', extensions: ['exe'] },
  { name: 'Semua file', extensions: ['*'] }
]));

ipcMain.handle('dialog:outputFolder', async () => {
  const result = await dialog.showOpenDialog(mainWindow, {
    properties: ['openDirectory', 'createDirectory']
  });
  return result.canceled ? '' : result.filePaths[0];
});

ipcMain.handle('app:getState', () => ({
  version: app.getVersion(),
  settings: readSettings(),
  rscriptCandidates: findRscriptCandidates(),
  packaged: app.isPackaged,
  activeRun
}));

ipcMain.handle('app:saveSettings', (_event, settings) => {
  writeSettings(settings || {});
  return { ok: true };
});

ipcMain.handle('environment:detect', (_event, requestedPath) => {
  const rscriptPath = getRscriptPath(requestedPath);
  return {
    found: Boolean(rscriptPath),
    rscriptPath,
    candidates: findRscriptCandidates(),
    bundled: rscriptPath ? rscriptPath.includes(path.join('vendor', 'R')) : false
  };
});

ipcMain.handle('environment:check', async (_event, requestedPath) => {
  const rscriptPath = getRscriptPath(requestedPath);
  if (!rscriptPath) throw new Error('Rscript.exe tidak ditemukan. Pilih lokasi Rscript secara manual.');
  writeSettings({ ...readSettings(), rscriptPath });
  await runRProcess({
    rscriptPath,
    scriptName: 'check_environment.R',
    args: [],
    modeName: 'environment-check'
  });
  return { ok: true, rscriptPath };
});

ipcMain.handle('environment:installPackages', async (_event, requestedPath) => {
  const rscriptPath = getRscriptPath(requestedPath);
  if (!rscriptPath) throw new Error('Rscript.exe tidak ditemukan.');
  await runRProcess({
    rscriptPath,
    scriptName: 'install_packages.R',
    args: [],
    modeName: 'package-install'
  });
  return { ok: true };
});

ipcMain.handle('analysis:validate', async (_event, payload) => {
  const config = payload?.config;
  const errors = basicConfigValidation(config);
  if (errors.length) return { ok: false, errors };

  const rscriptPath = getRscriptPath(payload?.rscriptPath);
  if (!rscriptPath) return { ok: false, errors: ['Rscript.exe tidak ditemukan.'] };

  const tempDir = fs.mkdtempSync(path.join(os.tmpdir(), 'sawitheight-validate-'));
  const tempConfig = {
    ...config,
    app: { name: APP_NAME, version: app.getVersion(), run_dir: tempDir }
  };
  const configPath = path.join(tempDir, 'validate_config.json');
  fs.writeFileSync(configPath, JSON.stringify(tempConfig, null, 2), 'utf8');

  try {
    await runRProcess({
      rscriptPath,
      scriptName: 'pipeline.R',
      args: ['validate', configPath],
      modeName: 'validation'
    });
    return { ok: true, rscriptPath };
  } catch (error) {
    return { ok: false, errors: [error.message] };
  } finally {
    fs.rmSync(tempDir, { recursive: true, force: true });
  }
});

ipcMain.handle('analysis:start', async (_event, payload) => {
  const config = payload?.config;
  const errors = basicConfigValidation(config);
  if (errors.length) return { ok: false, errors };
  if (activeProcess) return { ok: false, errors: ['Masih ada proses yang berjalan.'] };

  const rscriptPath = getRscriptPath(payload?.rscriptPath);
  if (!rscriptPath) return { ok: false, errors: ['Rscript.exe tidak ditemukan.'] };

  writeSettings({
    ...readSettings(),
    rscriptPath,
    lastConfig: config
  });

  const run = createRunConfig(config);
  emitToRenderer({ type: 'run-created', runDir: run.runDir, configPath: run.configPath });

  runRProcess({
    rscriptPath,
    scriptName: 'pipeline.R',
    args: ['run', run.configPath],
    modeName: 'analysis'
  }).catch((error) => {
    emitToRenderer({ type: 'fatal', message: error.message, runDir: run.runDir });
  });

  return { ok: true, runDir: run.runDir, configPath: run.configPath };
});

ipcMain.handle('analysis:cancel', async () => {
  if (!activeProcess) return { ok: false, message: 'Tidak ada proses aktif.' };
  const pid = activeProcess.pid;
  if (process.platform === 'win32') {
    spawnSync('taskkill', ['/pid', String(pid), '/T', '/F'], { windowsHide: true });
  } else {
    activeProcess.kill('SIGTERM');
  }
  emitToRenderer({ type: 'process', status: 'cancelled', pid });
  return { ok: true };
});

ipcMain.handle('shell:openPath', async (_event, targetPath) => {
  if (!targetPath || !fs.existsSync(targetPath)) return 'Path tidak ditemukan.';
  return shell.openPath(targetPath);
});

ipcMain.handle('shell:showItem', (_event, targetPath) => {
  if (targetPath && fs.existsSync(targetPath)) shell.showItemInFolder(targetPath);
  return { ok: true };
});

app.setName(APP_NAME);
app.setAppUserModelId('com.pmnp.sawitheight');

app.whenReady().then(() => {
  Menu.setApplicationMenu(null);
  createWindow();
  app.on('activate', () => {
    if (BrowserWindow.getAllWindows().length === 0) createWindow();
  });
});

app.on('before-quit', (event) => {
  if (activeProcess) {
    event.preventDefault();
    const pid = activeProcess.pid;
    if (process.platform === 'win32') {
      spawnSync('taskkill', ['/pid', String(pid), '/T', '/F'], { windowsHide: true });
    } else {
      activeProcess.kill('SIGTERM');
    }
    activeProcess = null;
    app.exit(0);
  }
});

app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') app.quit();
});

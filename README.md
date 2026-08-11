# SawitHeight R

Aplikasi desktop Windows untuk menjalankan analisis tinggi pohon sawit dari dense point
cloud fotogrametri dengan backend **R/lidR**. Ini bukan pembungkus tutorial: tombol
**Jalankan Analisis** mengeksekusi pipeline R, menampilkan progress dan log, menangani
error per tahap, lalu mengelola seluruh output dalam satu folder run.

## Catatan build self-contained R

Build mandiri mengunci runtime ke **R 4.5.3** dan memasang `lidR` dari repository resmi **r-lidar R-universe**, dengan CRAN sebagai fallback untuk dependency umum. Penguncian ini mencegah kegagalan binary saat versi R release terbaru belum memiliki binary `lidR` yang kompatibel.


## Fitur MVP

- Input LAS/LAZ, titik pokok SHP/GPKG/GeoJSON, DTM GeoTIFF, dan titik GCP sebagai referensi tanah independen.
- Deteksi `Rscript.exe`, pemeriksaan package, dan instalasi package ke user library.
- Validasi ekstensi, geometri, CRS proyeksi, irisan extent, cakupan DTM, parameter,
  serta hak tulis folder output.
- QC duplikat dan Statistical Outlier Removal.
- Empat mode referensi tanah:
  - CSF dense cloud → TIN.
  - DTM independen / sebelum tanam.
  - GCP untuk validasi dan koreksi mean bias DTM hasil CSF.
  - GCP sebagai ground anchor untuk memperkuat permukaan TIN.
- Pembuatan normalized CHM dengan algoritma `pitfree`.
- Statistik buffer menggunakan fractional pixel coverage dari `exactextractr`.
- Progress dan log R real-time.
- Pembatalan proses.
- Output CSV, GeoPackage, GeoTIFF, QC PNG, JSON, manifest, dan laporan HTML.
- Konfigurasi build untuk Portable EXE dan installer per-user tanpa elevasi admin.

## Output setiap run

```text
run_YYYY-MM-DDT...
├── run_config.json
├── analysis.log
├── normalized_pointcloud.laz      # opsional
├── nCHM_sawit.tif
├── tinggi_pokok_sawit.csv
├── shapefile/hasil_tinggi_pokok_D1.shp
├── qc_density.png
├── qc_nchm.png
├── qc_histogram_height.png
├── ground_reference/
│   ├── ground_reference_summary.json
│   ├── dtm_csf_raw.tif / dtm_csf_plus_gcp_anchor.tif
│   ├── dtm_bias_corrected.tif            # mode GCP bias, bila diterapkan
│   └── gcp_*validation.csv/.gpkg         # mode GCP
├── output_manifest.csv
├── result_summary.json
└── report.html
```

## Menjalankan untuk pengembangan

Persyaratan:

- Windows 10/11 64-bit.
- Node.js 24 untuk proses build.
- R yang kompatibel beserta package: `jsonlite`, `lidR`, `terra`, `sf`,
  `exactextractr`, dan `RCSF`.

```powershell
npm install
npm start
```

Aplikasi dapat mendeteksi R pada lokasi umum. Jika tidak ditemukan, pilih
`Rscript.exe`, misalnya:

```text
C:\Program Files\R\R-4.x.x\bin\Rscript.exe
```

## Membuat EXE melalui GitHub Actions

1. Buat repository GitHub baru.
2. Unggah seluruh isi folder proyek.
3. Buka **Actions → Build Windows EXE → Run workflow**.
4. Pilih:
   - `bundle_r = false`: EXE lebih kecil, menggunakan R pada komputer pengguna.
   - `bundle_r = true`: membundel R dan package; ukuran jauh lebih besar tetapi runtime
     analisis dapat berjalan tanpa instalasi R terpisah.
5. Unduh artifact setelah workflow selesai.

Hasil build yang ditargetkan:

```text
SawitHeight-R-Portable-0.3.1.exe
SawitHeight-R-Setup-0.3.1.exe
```

## Catatan penting

- Build portable tidak memerlukan proses instalasi, tetapi kebijakan AppLocker/antivirus
  perusahaan tetap dapat membatasi executable.
- EXE yang belum ditandatangani sertifikat code-signing dapat memunculkan SmartScreen.
- Build self-contained berukuran besar karena membawa Electron, R, library geospasial,
  dan DLL dependensi.
- MVP memuat satu LAS/LAZ ke RAM. Untuk dataset sangat besar atau kumpulan tile,
  gunakan data yang sudah dipotong atau kembangkan mode LAScatalog.
- DTM independen harus menggunakan CRS proyeksi yang sama dan menutupi seluruh extent LAS. GCP wajib memiliki CRS dan field elevasi tanah numerik.
- Parameter kelas tinggi 1,5 m dan 2,5 m adalah nilai awal dari tutorial, bukan standar
  biologis universal; sesuaikan dengan umur tanam dan hasil validasi lapangan.

## Status pengujian

Struktur Electron dan JavaScript telah diperiksa secara statis. Proyek belum dikompilasi menjadi EXE di lingkungan ini. Backend R juga harus diuji di Windows dengan data LAS/LAZ nyata karena lingkungan pembuat proyek ini tidak menyediakan runtime R maupun dataset point cloud uji. Versi yang dikunci untuk build awal adalah Electron 43.0.0 dan electron-builder 26.0.12.

## Troubleshooting: `GH_TOKEN` tidak ditemukan saat build

Workflow build memanggil `electron-builder` dengan `--publish never`. Opsi ini penting karena pada lingkungan CI, electron-builder dapat mendeteksi draft release dan mencoba melakukan implicit publishing. Build EXE dan upload artifact dilakukan terpisah; GitHub Release hanya dibuat pada langkah `Publish GitHub Release` ketika workflow dipicu oleh tag `v*`.

## Compatibility fix — noise removal
Pipeline noise removal uses `classify_noise(..., sor(...))` followed by `filter_poi(..., Classification != 18L)` instead of `lidR::remove_noise()`. This avoids API/export differences across lidR versions while preserving the LAS noise class semantics.

## Ground Reference v0.2.0

Versi 0.2.0 mengikuti perubahan Step 03B **Referensi Tanah Independen** pada tutorial terbaru. Mode GCP bias selalu menghitung residual `Z_DTM - Z_GCP`, mean bias, SD, dan RMSE. Koreksi mean bias dapat diaktifkan/dinonaktifkan dari UI; aplikasi tidak mengasumsikan sendiri batas SD yang dianggap "kecil". Mode GCP anchor membentuk permukaan TIN dari gabungan ground hasil CSF dan titik GCP, kemudian merasterkannya pada resolusi yang ditentukan pengguna sebelum normalisasi point cloud.


## Monitoring multi-periode v0.3.1

Output titik pokok utama sekarang selalu **Shapefile (.shp)**. Karena format DBF pada Shapefile membatasi nama field hingga 10 karakter, metrik periode memakai nama ringkas: `rerata_D1`, `maks_D1`, `min_D1`, `sd_D1`, `npix_D1`, `cover_D1`, `kelas_D1`, dan `qc_D1`. Pada baseline, `kelas_D1` adalah kelas tinggi absolut.

Mode **Periode pertama / baseline** membuat atribut periode awal. Mode **Monitoring lanjutan** membutuhkan Shapefile hasil sebelumnya dan ID pokok yang stabil. Aplikasi membawa atribut historis berdasarkan `tree_id`, menambahkan metrik periode saat ini, lalu menghitung `tumb_D2 = maks_D2 - maks_D1`. Nilai negatif tidak dipaksa menjadi nol.

Contoh evolusi output:

```text
D1: tree_id, rerata_D1, maks_D1, min_D1, ...
D2: tree_id, rerata_D1, maks_D1, ..., rerata_D2, maks_D2, ..., tumb_D2
D3: tree_id, atribut D1 + D2, rerata_D3, maks_D3, ..., tumb_D3
```

### Status pertumbuhan mulai periode D2

Mulai periode monitoring D2 dan seterusnya, field `kelas_Dx` **tidak lagi berisi kelas tinggi absolut**. Field tersebut menjadi status pertumbuhan yang diturunkan dari `tumb_Dx = maks_Dx - maks_D(previous)`. Ambang `growth_normal_min_m` dapat diatur di UI (default 0,10 m):

- `NORMAL`: delta >= ambang minimum.
- `ANOMALI`: delta >= 0 tetapi lebih kecil dari ambang minimum (tinggi relatif sama/mirip atau pertumbuhan kecil).
- `PERLU CEK`: delta negatif atau tidak dapat dihitung.

Contoh D2: `maks_D1=2.40`, `maks_D2=2.58`, `tumb_D2=0.18`, `kelas_D2=NORMAL`.

Kode periode dibatasi 1-3 karakter alfanumerik (`D1`, `D2`, `P01`) agar seluruh nama field periode tetap aman untuk Shapefile.
